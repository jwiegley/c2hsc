{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Applicative
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Foldable hiding (concat, mapM_)
import           Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable hiding (mapM, forM)
import           Language.C.Data.Ident
import           Language.C.Data.InputStream
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Parser
import           Language.C.Pretty
import           Language.C.Syntax.AST
import           Language.C.System.GCC
import           Language.C.System.Preprocess
import           Prelude hiding (concat, sequence, mapM, mapM_, foldr)
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.PrettyPrint as P
import           Text.StringTemplate

version :: String
version = "0.6.0"

copyright :: String
copyright = "2012"

c2hscSummary :: String
c2hscSummary = "c2hsc v" ++ version ++ ", (C) John Wiegley " ++ copyright

data C2HscOptions = C2HscOptions
    { gcc       :: FilePath
    , cppopts   :: String
    , prefix    :: String
    , useStdout :: Bool
    , overrides :: FilePath
    , verbose   :: Bool
    , debug     :: Bool
    , files     :: [FilePath] }
    deriving (Data, Typeable, Show, Eq)

c2hscOptions :: C2HscOptions
c2hscOptions = C2HscOptions
    { gcc       = def &= typFile
                  &= help "Specify explicit path to gcc or cpp"
    , cppopts   = def &= typ "OPTS"
                  &= help "Pass OPTS to the preprocessor"
    , prefix    = def &= typ "PREFIX"
                  &= help "Use PREFIX when naming modules"
    , useStdout = def &= name "stdout"
                  &= help "Send all output to stdout (for testing)"
    , overrides = def &= typFile
                  &= help "FILE contains \"C type -> FFI type\" translations"
    , verbose   = def &= name "v"
                  &= help "Report progress verbosely"
    , debug     = def &= name "D"
                  &= help "Report debug information"
    , files     = def &= args &= typFile } &=
    summary c2hscSummary &=
    program "c2hsc" &=
    help "Create an .hsc Bindings-DSL file from a C API header file"

------------------------------ IMPURE FUNCTIONS ------------------------------

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

main :: IO ()
main = getArgs >>= runArgs

smokeTest :: IO ()
smokeTest = runArgs ["--prefix=Test", "--stdout", "test/smoke2.h"]

runArgs :: [String] -> IO()
runArgs mainArgs = do
  opts <- withArgs (if null mainArgs then ["--help"] else mainArgs)
          (cmdArgs c2hscOptions)
  when (null (prefix opts)) $
    error "Please specify a module prefix to use with --prefix"

  gccExe <- findExecutable $ case gcc opts of "" -> "gcc"; x -> x
  case gccExe of
    Nothing      -> error $ "Cannot find executable '" ++ gcc opts ++ "'"
    Just gccPath -> parseFile gccPath opts

-- Once gcc is found, setup to parse the C file by running the preprocessor.
-- Then, identify the input file absolutely so we know which declarations to
-- print out at the end.

parseFile :: FilePath -> C2HscOptions -> IO ()
parseFile gccPath opts =
  for_ (files opts) $ \fileName -> do
    result <- runPreprocessor (newGCC gccPath)
                              (rawCppArgs [cppopts opts] fileName)
    case result of
      Left err     -> error $ "Failed to run cpp: " ++ show err
      Right stream -> do
        overrideState <- defineTypeOverrides (overrides opts)
        let pos = initPos fileName
            HscOutput hscs helpercs _ =
              execState (overrideState >>
                         parseCFile stream (posFile pos) pos)
                        newHscState
        writeProducts opts fileName hscs helpercs

defineTypeOverrides :: FilePath -> IO (Output ())
defineTypeOverrides [] = return (void defaultOverrides)
defineTypeOverrides overridesFile = do
  contents <- readFile overridesFile
  return $ mapM_ (\line ->
                   let (cName:ffiName:[]) = splitOn " -> " line
                   in overrideType cName ffiName)
                 (lines contents)

overrideType :: String -> String -> Output ()
overrideType cName ffiName =
  defineType cName Typedef { typedefName     = ffiName
                           , typedefOverride = True }

defaultOverrides :: Output [()]
defaultOverrides = mapM (uncurry overrideType)
                        [ ("size_t",    "CSize")
                        , ("intptr_t",  "IntPtr")
                        , ("uintptr_t", "WordPtr") ]

-- Write out the gathered data

writeProducts :: C2HscOptions -> FilePath -> [String] -> [String] -> IO ()
writeProducts opts fileName hscs helpercs = do
  let code   = newSTMP $
               unlines [ "#include <bindings.dsl.h>"
                       , "#include \"$headerFileName$\""
                       , "module $libName$.$cFileName$ where"
                       , "import Foreign.Ptr"
                       , "#strict_import"
                       , "" ]
      vars   = [ ("libName",   prefix opts)
               , ("cFileName", cap)
               , ("headerFileName", fileName) ]
      cap    = capitalize . dropExtension . takeFileName $ fileName
      target = cap ++ ".hsc"

  handle <- if useStdout opts
            then return System.IO.stdout
            else openFile target WriteMode

  hPutStrLn handle $ toString $ setManyAttrib vars code

  -- Sniff through the file again, but looking only for local #include's
  includes <- filter ("#include \"" `isPrefixOf`) . lines
                     <$> readFile fileName
  for_ includes $ \inc ->
    hPutStrLn handle $ "import "
                    ++ prefix opts ++ "."
                    ++ (capitalize . takeWhile (/= '.') . drop 10 $ inc)

  traverse_ (hPutStrLn handle) hscs

  unless (useStdout opts) $ do
    hClose handle
    putStrLn $ "Wrote " ++ target

  when (length helpercs > 0) $ do
    let targetc = cap ++ ".hsc.helper.c"
    handlec <- if useStdout opts
               then return System.IO.stdout
               else openFile targetc WriteMode

    hPutStrLn handlec "#include <bindings.cmacros.h>"
    traverse_ (hPutStrLn handlec) includes
    hPutStrLn handlec ""
    traverse_ (hPutStrLn handlec) helpercs

    unless (useStdout opts) $ do
      hClose handlec
      putStrLn $ "Wrote " ++ targetc

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toTitle x : camelCase xs

camelCase :: String -> String
camelCase []       = []
camelCase ('_':xs) = capitalize xs
camelCase (x:xs)   = x : camelCase xs

------------------------------- PURE FUNCTIONS -------------------------------

-- Rather than writing to the .hsc and .hsc.helper.c files directly from the
-- IO monad, they are collected in an HscOutput value in the State monad.  The
-- actual writing is done by writeProducts.  This keeps all the code below
-- pure, and since the data sets involved are relatively small, performance is
-- not a critical issue.

data Typedef = Typedef { typedefName     :: String
                       , typedefOverride :: Bool }
type TypeMap   = M.Map String Typedef
data HscOutput = HscOutput [String] [String] TypeMap
type Output    = State HscOutput

newHscState :: HscOutput
newHscState = HscOutput [] [] M.empty

appendHsc :: String -> Output ()
appendHsc hsc = do
  HscOutput hscs xs types <- get
  put $ HscOutput (hscs ++ [hsc]) xs types

appendHelper :: String -> Output ()
appendHelper helperc = do
  HscOutput xs helpercs types <- get
  put $ HscOutput xs (helpercs ++ [helperc]) types

defineType :: String -> Typedef -> Output ()
defineType key value = do
  HscOutput xs ys types <- get
  hasOverride <- fmap typedefOverride <$> lookupType key
  case hasOverride of
    Just True -> return ()
    _         -> put $ HscOutput xs ys (M.insert key value types)

lookupType :: String -> Output (Maybe Typedef)
lookupType key = do
  HscOutput _ _ types <- get
  return $ M.lookup key types

-- Now we are ready to parse the C code from the preprocessed input stream,
-- located in the given file and starting at the specified position.  The
-- result of a parse is a list of global declarations, so filter the list down
-- to those occurring in the target file, and then print the declarations in
-- Bindings-DSL format.

parseCFile :: InputStream -> FilePath -> Position -> Output ()
parseCFile stream fileName pos =
  case parseC stream pos of
    Left err -> error $ "Failed to compile: " ++ show err
    Right (CTranslUnit decls _) -> generateHsc decls
  where
    generateHsc :: [CExtDecl] -> Output ()
    generateHsc = traverse_ (appendNode fileName)

declInFile :: FilePath -> CExtDecl -> Bool
declInFile fileName = (== fileName) . posFile . posOfNode . declInfo

declInfo :: CExtDecl -> NodeInfo
declInfo (CDeclExt (CDecl _ _ info))       = info
declInfo (CFDefExt (CFunDef _ _ _ _ info)) = info
declInfo (CAsmExt _ info)                  = info

-- These are the top-level printing routines.  We are only interested in
-- declarations and function defitions (which almost always means inline
-- functions if the target file is a header file).
--
-- We will end up printing the following constructs:
--
--   - Structure definitions
--   - Opaque types (i.e., forward declarations of pointer type)
--   - Enums
--   - Extern Functions
--   - Inline Functions

appendNode :: FilePath -> CExtDecl -> Output ()

appendNode fp dx@(CDeclExt (CDecl declSpecs items _)) =
  case items of
    [] ->
      when (declInFile fp dx) $ do
        appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
        appendType declSpecs ""

    xs ->
      for_ xs $ \(declrtr, _, _) ->
        for_ (splitDecl declrtr) $ \(declrtr', ddrs, nm) ->
          case ddrs of
            CFunDeclr (Right (_, _)) _ _:_ ->
              when (declInFile fp dx) $
                appendFunc "#ccall" declSpecs declrtr'

            _ ->
              -- If the type is a typedef, record the equivalence so we can
              -- look it up later
              case declSpecs of
                CStorageSpec (CTypedef _):_ -> do
                  when (declInFile fp dx) $ do
                    appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
                    appendType declSpecs nm

                  dname <- declSpecTypeName declSpecs
                  unless (null dname || dname == "<" ++ nm ++ ">") $ do
                    when (declInFile fp dx) $
                      appendHsc $ "#synonym_t " ++ nm ++ " , " ++ dname

                    defineType nm Typedef { typedefName     = dname
                                          , typedefOverride = False }
                _ ->
                  when (declInFile fp dx) $ do
                    dname <- declSpecTypeName declSpecs
                    appendHsc $ "#globalvar " ++ nm ++ " , " ++ dname
  where
    splitDecl declrtr = do      -- in the Maybe Monad
      d@(CDeclr ident ddrs _ _ _) <- declrtr
      return (d, ddrs, case ident of Just (Ident nm _ _) -> nm; _ -> "")

appendNode fp dx@(CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume functions defined in headers are inline functions
  when (declInFile fp dx) $ do
    appendFunc "#cinline" declSpecs declrtr

    let CDeclr ident ddrs _ _ _ = declrtr

    for_ ident $ \(Ident nm _ _) ->
      case head ddrs of
        CFunDeclr (Right (decls, _)) _ _ -> do
          retType <- derDeclrTypeName' True declSpecs (tail ddrs)
          funType <- applyDeclrs True retType ddrs
          if not (null retType)
            then appendHelper $ "BC_INLINE" ++ show (length decls)
                             ++ "(" ++ nm ++ ", " ++ funType ++ ")"
            else appendHelper $ "BC_INLINE" ++ show (length decls)
                             ++ "VOID(" ++ nm ++ ", " ++ funType ++ ")"
        _ -> return ()

appendNode _ (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

appendFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator a -> Output ()
appendFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let _:retDeclr:_ = splitWhen isFuncDeclr ddrs
      funcDeclr:_  = dropWhile (not . isFuncDeclr) ddrs

  retType  <- derDeclrTypeName declSpecs retDeclr
  argTypes <- (++) <$> getArgTypes funcDeclr
                   <*> pure [ "IO (" ++ retType ++ ")" ]

  let name' = nameFromIdent ident
      code  = newSTMP "$marker$ $name$ , $argTypes;separator=' -> '$"
      -- I have to call setAttribute separately since argTypes :: [String]
      code' = setAttribute "argTypes" argTypes code
      vars  = [ ("marker",  marker)
              , ("name",    name') ]

  appendHsc $ toString $ setManyAttrib vars code'

  where
    getArgTypes x = filter (not . null) <$> sequence (getArgTypes' x)

    getArgTypes' (CFunDeclr (Right (decls, _)) _ _) = map cdeclTypeName decls
    getArgTypes' _ = []

    nameFromIdent (Just (Ident n _ _)) = n
    nameFromIdent _ = "<no name>"

    isFuncDeclr (CFunDeclr {}) = True
    isFuncDeclr _ = False

appendType :: [CDeclarationSpecifier a] -> String -> Output ()
appendType declSpecs declrName = traverse_ appendType' declSpecs
  where
    appendType' (CTypeSpec (CSUType (CStruct _ ident decls _ _) _)) = do
      let name' = identName ident
      when (isNothing decls) $
        appendHsc $ "#opaque_t " ++ name'

      for_ decls $ \xs -> do
        appendHsc $ "#starttype " ++ name'
        for_ xs $ \x ->
          for_ (cdeclName x) $ \declName -> do
            let CDecl declSpecs' ((Just y, _, _):_) _ = x
            case y of
              CDeclr _ (CArrDeclr {}:zs) _ _ _ -> do
                tname <- derDeclrTypeName declSpecs' zs
                appendHsc $ "#array_field " ++ declName ++ " , " ++ tname
              _ -> do
                tname <- cdeclTypeName x
                appendHsc $ "#field " ++ declName ++ " , " ++ tname
        appendHsc "#stoptype"

    appendType' (CTypeSpec (CEnumType (CEnum ident defs _ _) _)) = do
      let name' = identName ident
      appendHsc $ "#integral_t " ++ name'

      for_ defs $ \ds ->
        for_ ds $ \(Ident nm _ _, _) ->
          appendHsc $ "#num " ++ nm

    appendType' _ = return ()

    identName ident = case ident of
                        Nothing -> declrName
                        Just (Ident nm _ _) -> nm

-- The remainder of this file is some hairy code for turning various
-- constructs into Bindings-DSL type names, such as turning "int ** foo" into
-- the type name "Ptr (Ptr CInt)".

data Signedness = None | Signed | Unsigned deriving (Eq, Show, Enum)

cdeclName :: CDeclaration a -> Maybe String
cdeclName (CDecl _ more _) =
  case more of
    (Just (CDeclr (Just (Ident nm _ _)) _ _ _ _), _, _) : _ -> Just nm
    _ -> Nothing

cdeclTypeName :: CDeclaration a -> Output String
cdeclTypeName = cdeclTypeName' False

cdeclTypeName' :: Bool -> CDeclaration a -> Output String
cdeclTypeName' cStyle (CDecl declSpecs more _) =
  case more of
    (Just x, _, _) : _ -> declrTypeName' cStyle declSpecs x
    _                  -> declSpecTypeName' cStyle declSpecs

declSpecTypeName :: [CDeclarationSpecifier a] -> Output String
declSpecTypeName = declSpecTypeName' False

declSpecTypeName' :: Bool -> [CDeclarationSpecifier a] -> Output String
declSpecTypeName' cStyle = flip (derDeclrTypeName' cStyle) []

declrTypeName :: [CDeclarationSpecifier a] -> CDeclarator a -> Output String
declrTypeName = declrTypeName' False

declrTypeName' :: Bool -> [CDeclarationSpecifier a] -> CDeclarator a
               -> Output String
declrTypeName' cStyle declSpecs (CDeclr _ ddrs _ _ _) =
  derDeclrTypeName' cStyle declSpecs ddrs

derDeclrTypeName :: [CDeclarationSpecifier a] -> [CDerivedDeclarator a]
                 -> Output String
derDeclrTypeName = derDeclrTypeName' False

derDeclrTypeName' :: Bool -> [CDeclarationSpecifier a] -> [CDerivedDeclarator a]
                  -> Output String
derDeclrTypeName' cStyle declSpecs ddrs = do
  nm <- fullTypeName' None declSpecs
  applyDeclrs cStyle nm ddrs

  where
    fullTypeName' :: Signedness -> [CDeclarationSpecifier a] -> Output String
    fullTypeName' _ [] = return ""

    fullTypeName' s (CTypeQual qual:xs) =
      if cStyle
      then do
        baseType <- fullTypeName' s xs
        return $ qualToStr qual ++ " " ++ baseType
      else
        fullTypeName' s xs

    fullTypeName' _ (CTypeSpec (CSignedType _):[]) =
      if cStyle then return "signed" else return "CInt"
    fullTypeName' _ (CTypeSpec (CUnsigType _):[]) =
      if cStyle then return "unsigned" else return "CUInt"

    fullTypeName' s (x:xs) =
      case x of
        CTypeSpec (CSignedType _) -> fullTypeName' Signed xs
        CTypeSpec (CUnsigType _)  -> fullTypeName' Unsigned xs
        CTypeSpec tspec           -> if cStyle
                                     then cTypeName tspec s
                                     else typeName tspec s
        _ -> fullTypeName' s xs

concatM :: (Monad f, Functor f) => [f [a]] -> f [a]
concatM xs = concat <$> sequence xs

applyDeclrs :: Bool -> String -> [CDerivedDeclarator a] -> Output String

applyDeclrs cStyle baseType (CPtrDeclr {}:f@CFunDeclr {}:ds) = do
  baseType' <- applyDeclrs cStyle baseType ds
  applyDeclrs cStyle baseType' [f]

applyDeclrs cStyle baseType (CFunDeclr (Right (decls, _)) _ _:_)
  | cStyle    = renderList ", " (funTypes decls baseType)
  | otherwise = do
    argTypes <- renderList " -> " (funTypes decls baseType)
    return $ "FunPtr (" ++ argTypes ++ ")"

  where renderList str xs = intercalate str <$> filter (not . null) <$> xs
        funTypes xs bt    = (++) <$> mapM (cdeclTypeName' cStyle) xs
                                 <*> pure [bt]

applyDeclrs cStyle baseType decl@(CPtrDeclr quals _:[])
  | cStyle && baseType == "" = applyDeclrs cStyle "void" decl
  | cStyle                  = return $ baseType ++ "*"
                                    ++ preQualsToString quals
  | baseType == ""          = return "Ptr ()"
  | baseType == "CChar"     = return "CString"
  | otherwise               = return $ "Ptr " ++ baseType

applyDeclrs cStyle baseType (CPtrDeclr quals _:xs)
  | cStyle    = concatM [ applyDeclrs cStyle baseType xs
                        , pure "*"
                        , pure (preQualsToString quals) ]
  | otherwise = concatM [ pure "Ptr ("
                        , applyDeclrs cStyle baseType xs
                        , pure ")" ]

applyDeclrs cStyle baseType (CArrDeclr quals _ _:xs)
  | cStyle    = concatM [ pure (sufQualsToString quals)
                        , applyDeclrs cStyle baseType xs
                        , pure "[]" ]
  | otherwise = concatM [ pure "Ptr ("
                        , applyDeclrs cStyle baseType xs
                        , pure ")" ]

applyDeclrs _ baseType _ = return baseType

prefixWith :: a -> [a] -> [a]
prefixWith _ [] = []
prefixWith x xs = x:xs

preQualsToString :: [CTypeQualifier a] -> String
preQualsToString = prefixWith ' ' . qualsToStr

sufQualsToString :: [CTypeQualifier a] -> String
sufQualsToString = prefixWith ' ' . qualsToStr

suffixWith :: a -> [a] -> [a]
suffixWith _ [] = []
suffixWith x xs = xs ++ [x]

qualsToStr :: [CTypeQualifier a] -> String
qualsToStr = unwords . map qualToStr

qualToStr :: CTypeQualifier t -> String
qualToStr (CConstQual _)  = "const"
qualToStr (CVolatQual _)  = "volatile"
qualToStr (CRestrQual _)  = "restricted"
qualToStr (CInlineQual _) = ""
qualToStr (CAttrQual _)   = error "Unimplemented: attribute qualifiers"

-- Simple translation from C types to Foreign.C.Types types.  We represent
-- Void as the empty string so that returning void becomes IO (), and passing
-- a void star becomes Ptr ().

typeName :: CTypeSpecifier a -> Signedness -> Output String

typeName (CVoidType _) _   = return ""
typeName (CFloatType _) _  = return "CFloat"
typeName (CDoubleType _) _ = return "CDouble"
typeName (CBoolType _) _   = return "CInt"

typeName (CCharType _) s   = case s of
                               Signed   -> return "CSChar"
                               Unsigned -> return "CUChar"
                               _        -> return "CChar"
typeName (CShortType _) s  = case s of
                               Signed   -> return "CShort"
                               Unsigned -> return "CUShort"
                               _        -> return "CShort"
typeName (CIntType _) s    = case s of
                               Signed   -> return "CInt"
                               Unsigned -> return "CUInt"
                               _        -> return "CInt"
typeName (CLongType _) s   = case s of
                               Signed   -> return "CLong"
                               Unsigned -> return "CULong"
                               _        -> return "CLong"

typeName (CTypeDef (Ident nm _ _) _) _ = do
  definition <- lookupType nm
  case definition of
    Nothing -> return $ "<" ++ nm ++ ">"
    Just (Typedef { typedefName = defNm }) ->
      return defNm

typeName (CSUType (CStruct _ (Just (Ident nm _ _)) _ _ _) _) _ =
  return $ "<" ++ nm ++ ">"
typeName (CEnumType (CEnum (Just (Ident nm _ _)) _ _ _) _) _ =
  return $ "<" ++ nm ++ ">"

typeName (CComplexType _) _  = return ""
typeName (CTypeOfExpr _ _) _ = return ""
typeName (CTypeOfType _ _) _ = return ""

typeName _ _ = return ""

-- Translation from C back to C.  Needed because there's no good way to pretty
-- print a function's return type (including pointers on the declarator) in
-- language-c.

cTypeName :: CTypeSpecifier a -> Signedness -> Output String

cTypeName (CVoidType _) _   = return ""
cTypeName (CFloatType _) _  = return "float"
cTypeName (CDoubleType _) _ = return "double"
cTypeName (CBoolType _) _   = return "int"

cTypeName (CCharType _) s   = case s of
                               Signed   -> return "signed char"
                               Unsigned -> return "unsigned char"
                               _        -> return "char"
cTypeName (CShortType _) s  = case s of
                               Signed   -> return "signed short"
                               Unsigned -> return "unsigned short"
                               _        -> return "hort"
cTypeName (CIntType _) s    = case s of
                               Signed   -> return "signed int"
                               Unsigned -> return "unsigned int"
                               _        -> return "int"
cTypeName (CLongType _) s   = case s of
                               Signed   -> return "signed long"
                               Unsigned -> return "unsigned long"
                               _        -> return "long"

cTypeName (CTypeDef (Ident nm _ _) _) _ = do
  definition <- lookupType nm
  case definition of
    Nothing -> return nm
    Just (Typedef { typedefOverride = True }) ->
      return nm
    Just (Typedef { typedefName = defNm, typedefOverride = False }) ->
      return defNm

cTypeName (CComplexType _) _  = return ""
cTypeName (CSUType _ _) _     = return ""
cTypeName (CEnumType _ _) _   = return ""
cTypeName (CTypeOfExpr _ _) _ = return ""
cTypeName (CTypeOfType _ _) _ = return ""

cTypeName _ _ = return ""

-- c2hsc.hs
