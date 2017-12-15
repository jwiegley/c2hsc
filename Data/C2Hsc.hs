{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.C2Hsc where

import           Control.Applicative
import           Control.Logging
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Data
import           Data.Default
import           Data.Foldable hiding (concat, elem, mapM_)
import           Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (pack)
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
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           Text.PrettyPrint as P hiding ((<>))
import           Text.StringTemplate

data C2HscOptions = C2HscOptions
    { gcc          :: FilePath
    , cppopts      :: [String]
    , prefix       :: String
    , filePrefix   :: [String]
    , overrides    :: FilePath
    , verbose      :: Bool
    , debug        :: Bool
    , files        :: [FilePath]
    }
    deriving (Data, Typeable, Show, Eq)

instance Default C2HscOptions where
    def = C2HscOptions "/usr/bin/gcc" [] "" [] "" True False []

------------------------------ IMPURE FUNCTIONS ------------------------------

-- This function is used for debugging
processString :: String -> IO String
processString str = do
    tmpDir <- getTemporaryDirectory
    withTempFile tmpDir "c2hsc.src" $ \path h -> do
        hPutStr h str
        hClose h
        withTempFile tmpDir "c2hsc.out" $ \outPath outH -> do
            runArgs def { files  = [path]
                        , prefix = "Spec"
                        } (Just outH) True
            hClose outH
            readFile outPath

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

runArgs :: C2HscOptions -> Maybe Handle -> Bool -> IO ()
runArgs opts output omitHeader = do
  gccExe <- findExecutable $ case gcc opts of "" -> "gcc"; x -> x
  case gccExe of
    Nothing      -> error $ "Cannot find executable '" ++ gcc opts ++ "'"
    Just gccPath -> for_ (files opts) $ \fileName ->
        parseFile gccPath fileName output omitHeader opts

-- Once gcc is found, setup to parse the C file by running the preprocessor.
-- Then, identify the input file absolutely so we know which declarations to
-- print out at the end.

parseFile :: FilePath -> FilePath -> Maybe Handle -> Bool -> C2HscOptions -> IO ()
parseFile gccPath fileName output omitHeader opts = do
    result <- runPreprocessor (newGCC gccPath)
                              (rawCppArgs
                                (cppopts opts)
                                fileName)
    case result of
      Left err     -> error $ "Failed to run cpp: " ++ show err
      Right stream -> do
        overrideState <- defineTypeOverrides (overrides opts)
        let pos = initPos fileName
            HscOutput hscs helpercs _ =
              let ps = filePrefix opts
                  fm = if null ps
                          then (posFile pos ==)
                          else \fn -> any (`isPrefixOf` fn) ps
              in execState (overrideState >> parseCFile stream fm pos)
                           newHscState
        writeProducts opts fileName output omitHeader hscs helpercs

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
  defineType cName $ Just Typedef { typedefName     = ffiName
                                  , typedefOverride = True }

defaultOverrides :: Output ()
defaultOverrides = mapM_ (uncurry overrideType)
                         [ ("size_t",    "CSize")
                         , ("intptr_t",  "IntPtr")
                         , ("uintptr_t", "WordPtr") ]

makeModuleName :: String -> String
makeModuleName = Prelude.concatMap capitalize . splitOn "-"

-- Write out the gathered data

writeProducts :: C2HscOptions
              -> FilePath
              -> Maybe Handle
              -> Bool
              -> [String]
              -> [String]
              -> IO ()
writeProducts opts fileName output omitHeader hscs helpercs = do
  let code   = newSTMP $
          if omitHeader
          then ""
          else unlines
              [ "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
              , "#include <bindings.dsl.h>"
              , "#include \"$headerFileName$\""
              , "module $libName$$cFileName$ where"
              , "import Foreign.Ptr"
              , "#strict_import"
              , ""
              ]
      pre    = if  null (prefix opts) then "" else prefix opts ++ "."
      vars   = [ ("libName", pre)
               , ("cFileName", cap)
               , ("headerFileName", fileName) ]
      cap    = makeModuleName . dropExtension . takeFileName $ fileName
      target = cap ++ ".hsc"

  handle <- case output of
      Just h -> return h
      Nothing -> openFile target WriteMode

  hPutStrLn handle $ toString $ setManyAttrib vars code

  -- Sniff through the file again, but looking only for local #include's
  includes <- filter ("#include \"" `isPrefixOf`) . lines
                     <$> readFile fileName
  for_ includes $ \inc -> do
    let incPath      = splitOn "\"" inc !! 1
        incPathParts = map dropTrailingPathSeparator $ splitPath $ dropExtension incPath
        modName      = pre ++ intercalate "." (map makeModuleName incPathParts)
    hPutStrLn handle $ "import " ++ modName

  traverse_ (hPutStrLn handle) hscs

  when (isNothing output) $ do
    hClose handle
    log' $ "Wrote " <> pack target

  unless (null helpercs) $ do
    let targetc = cap ++ ".hsc.helper.c"
    handlec <- case output of
        Just h -> return h
        Nothing -> openFile targetc WriteMode

    hPutStrLn handlec "#include <bindings.cmacros.h>"
    traverse_ (hPutStrLn handlec) includes
    hPutStrLn handlec ""
    traverse_ (hPutStrLn handlec) helpercs

    when (isNothing output) $ do
      hClose handlec
      log' $ "Wrote " <> pack targetc

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

data Typedef = Typedef
    { typedefName     :: String
    , typedefOverride :: Bool
    }
    deriving Show

type TypeMap = M.Map String (Maybe Typedef)

data HscOutput = HscOutput
    { hoHsc     :: [String]
    , hoHelperC :: [String]
    , hoTypes   :: TypeMap
    }

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

defineType :: String -> Maybe Typedef -> Output ()
defineType key value = do
  HscOutput xs ys types <- get
  hasOverride <- fmap typedefOverride <$> lookupType key
  case hasOverride of
    Just True -> return ()
    _         -> put $ HscOutput xs ys (M.insert key value types)

lookupType :: String -> Output (Maybe Typedef)
lookupType key = do
  HscOutput _ _ types <- get
  return . join $ M.lookup key types

-- Now we are ready to parse the C code from the preprocessed input stream,
-- located in the given file and starting at the specified position.  The
-- result of a parse is a list of global declarations, so filter the list down
-- to those occurring in the target file, and then print the declarations in
-- Bindings-DSL format.

parseCFile :: InputStream -> (FilePath -> Bool) -> Position -> Output ()
parseCFile stream fm pos =
  case parseC stream pos of
    Left err -> error $ "Failed to compile: " ++ show err
    Right (CTranslUnit decls _) -> generateHsc decls
  where
    generateHsc :: [CExtDecl] -> Output ()
    generateHsc = traverse_ (appendNode fm)

declMatches :: (FilePath -> Bool) -> CExtDecl -> Bool
declMatches fm = fm . posFile . posOfNode . declInfo

declInfo :: CExtDecl -> NodeInfo
declInfo (CDeclExt (CDecl _ _ info))         = info
declInfo (CDeclExt (CStaticAssert _ _ info)) = info
declInfo (CFDefExt (CFunDef _ _ _ _ info))   = info
declInfo (CAsmExt _ info)                    = info

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

appendNode :: (FilePath -> Bool) -> CExtDecl -> Output ()

appendNode _ (CDeclExt (CStaticAssert _ _ _)) = return ()

appendNode fm dx@(CDeclExt (CDecl declSpecs items _)) =
  case items of
    [] ->
      when (declMatches fm dx) $ do
        appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
        appendType declSpecs ""

    xs ->
      for_ xs $ \(declrtr, _, _) ->
        for_ (splitDecl declrtr) $ \(declrtr', ddrs, nm) ->
          case ddrs of
            CPtrDeclr{}:CFunDeclr (Right _) _ _:_ ->
              when (declMatches fm dx) $
                appendFunc "#callback" declSpecs declrtr'

            CFunDeclr (Right (_, _)) _ _:_ ->
              when (declMatches fm dx) $
                appendFunc "#ccall" declSpecs declrtr'

            CArrDeclr{}:CPtrDeclr{}:_ ->
              when (declMatches fm dx) $ do
                dname <- declSpecTypeName True declSpecs
                appendHsc $ "#globalarray " ++ nm ++ " , Ptr " ++ tyParens dname

            CArrDeclr{}:_ ->
              when (declMatches fm dx) $ do
                dname <- declSpecTypeName True declSpecs
                appendHsc $ "#globalarray " ++ nm ++ " , " ++ tyParens dname

            CPtrDeclr{}:_ ->
              when (declMatches fm dx) $ do
                dname <- declSpecTypeName True declSpecs
                appendHsc $ "#globalvar " ++ nm ++ " , Ptr " ++ tyParens dname

            _ ->
              -- If the type is a typedef, record the equivalence so we can
              -- look it up later
              case declSpecs of
                CStorageSpec (CTypedef _):_ -> do
                  when (declMatches fm dx) $ do
                    appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
                    appendType declSpecs nm

                  dname <- declSpecTypeName True declSpecs
                  unless (null dname || dname == "<" ++ nm ++ ">") $ do
                    when (declMatches fm dx) $
                      appendHsc $ "#synonym_t " ++ nm ++ " , " ++ dname
                    -- We saw the synonym, override the defineType just above
                    defineType nm $ Just Typedef
                        { typedefName     = dname
                        , typedefOverride = False
                        }

                _ ->
                  when (declMatches fm dx) $ do
                    dname <- declSpecTypeName True declSpecs
                    appendHsc $ "#globalvar " ++ nm ++ " , " ++ tyParens dname
  where
    splitDecl declrtr = do      -- in the Maybe Monad
      d@(CDeclr ident ddrs _ _ _) <- declrtr
      return (d, ddrs, case ident of Just (Ident nm _ _) -> nm; _ -> "")

appendNode fm dx@(CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume functions defined in headers are inline functions
  when (declMatches fm dx) $ do
    appendFunc "#cinline" declSpecs declrtr

    let CDeclr ident ddrs _ _ _ = declrtr

    for_ ident $ \(Ident nm _ _) ->
      case head ddrs of
        CFunDeclr (Right (decls, _)) _ _ -> do
          retType <- derDeclrTypeName' True False declSpecs (tail ddrs)
          funType <- applyDeclrs True False retType ddrs
          appendHelper $
            "BC_INLINE" ++ show (length decls)
            ++ (if not (null retType) then "" else "VOID")
            ++ "(" ++ nm ++ ", " ++ funType ++ ")"
        _ -> return ()

appendNode _ (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

appendFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator a -> Output ()
appendFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let _:retDeclr:_ = splitWhen isFuncDeclr ddrs
      funcDeclr:_  = dropWhile (not . isFuncDeclr) ddrs

  retType  <- derDeclrTypeName False declSpecs retDeclr
  argTypes <- (++) <$> getArgTypes funcDeclr
                   <*> pure [ "IO " ++ tyParens retType ]

  let name' = nameFromIdent ident
      code  = newSTMP "$marker$ $name$ , $argTypes;separator=' -> '$"
      -- I have to call setAttribute separately since argTypes :: [String]
      code' = setAttribute "argTypes" argTypes code
      vars  = [ ("marker",  marker)
              , ("name",    name') ]

  appendHsc $ toString $ setManyAttrib vars code'

  where
    getArgTypes x = filter (not . null) <$> sequence (getArgTypes' x)

    getArgTypes' (CFunDeclr (Right (decls, _)) _ _) =
        map (cdeclTypeName False) decls
    getArgTypes' _ = []

    nameFromIdent (Just (Ident n _ _)) = n
    nameFromIdent _ = "<no name>"

    isFuncDeclr (CFunDeclr {}) = True
    isFuncDeclr _ = False

structTagPrefix :: CStructTag -> String
structTagPrefix CStructTag = "struct "
structTagPrefix CUnionTag = "union "

appendType :: [CDeclarationSpecifier a] -> String -> Output ()
appendType declSpecs declrName = traverse_ appendType' declSpecs
  where
    appendType' (CTypeSpec (CSUType (CStruct tag ident decls _ _) _)) = do
      let name' = identName (structTagPrefix tag) ident
      seen <- M.member name' . hoTypes <$> get
      when (isNothing decls && not seen) $ do
        appendHsc $ "#opaque_t " ++ name'
        defineType name' Nothing

      for_ decls $ \xs -> do
        appendHsc $ "#starttype " ++ name'
        for_ xs $ \x ->
          for_ (cdeclNames x) $ \declName -> do
            let CDecl declSpecs' ((Just y, _, _):_) _ = x
            case y of
              CDeclr _ (CArrDeclr {}:zs) _ _ _ -> do
                tname <- derDeclrTypeName True declSpecs' zs
                appendHsc $ "#array_field " ++ declName ++ " , " ++ tname
              _ -> do
                tname <- cdeclTypeName True x
                appendHsc $ "#field " ++ declName ++ " , " ++ tname
        appendHsc "#stoptype"

    appendType' (CTypeSpec (CEnumType (CEnum ident defs _ _) _)) = do
      let name' = identName "enum " ident
      unless (null name') $ appendHsc $ "#integral_t " ++ name'

      for_ defs $ \ds ->
        for_ ds $ \(Ident nm _ _, _) ->
          appendHsc $ "#num " ++ nm

    appendType' _ = return ()

    identName pref ident = case ident of
                        Nothing -> declrName
                        Just (Ident nm _ _) -> pref ++ nm

-- The remainder of this file is some hairy code for turning various
-- constructs into Bindings-DSL type names, such as turning "int ** foo" into
-- the type name "Ptr (Ptr CInt)".

data Signedness = None | Signed | Unsigned deriving (Eq, Show, Enum)
data Lengthiness = Unspecified | Long deriving (Eq, Show, Enum)

cdeclNames :: CDeclaration a -> [String]
cdeclNames (CDecl _ more _) =
  collect more []
  where
    collect []     nms = reverse nms
    collect (m:ms) nms = collect ms $ case m of
        (Just (CDeclr (Just (Ident nm _ _)) _ _ _ _), _, _)
          -> nm:nms
        _ ->    nms
cdeclNames (CStaticAssert _ _ _) = []

cdeclTypeName :: Bool -> CDeclaration a -> Output String
cdeclTypeName = cdeclTypeName' False

cdeclTypeName' :: Bool -> Bool -> CDeclaration a -> Output String
cdeclTypeName' cStyle isDirect (CDecl declSpecs more _) =
  case more of
    (Just x, _, _) : _ -> declrTypeName' cStyle isDirect declSpecs x
    _                  -> declSpecTypeName' cStyle isDirect declSpecs
cdeclTypeName' _ _ (CStaticAssert _ _ _) = error "Unhandled static assertion"

declSpecTypeName :: Bool -> [CDeclarationSpecifier a] -> Output String
declSpecTypeName = declSpecTypeName' False

declSpecTypeName' :: Bool -> Bool -> [CDeclarationSpecifier a] -> Output String
declSpecTypeName' cStyle isDirect = flip (derDeclrTypeName' cStyle isDirect) []

declrTypeName :: Bool -> [CDeclarationSpecifier a] -> CDeclarator a
              -> Output String
declrTypeName = declrTypeName' False

declrTypeName' :: Bool -> Bool -> [CDeclarationSpecifier a] -> CDeclarator a
               -> Output String
declrTypeName' cStyle isDirect declSpecs (CDeclr _ ddrs _ _ _) =
  derDeclrTypeName' cStyle isDirect declSpecs ddrs

derDeclrTypeName :: Bool -> [CDeclarationSpecifier a] -> [CDerivedDeclarator a]
                 -> Output String
derDeclrTypeName = derDeclrTypeName' False

derDeclrTypeName' :: Bool
                  -> Bool
                  -> [CDeclarationSpecifier a]
                  -> [CDerivedDeclarator a]
                  -> Output String
derDeclrTypeName' cStyle isDirect declSpecs ddrs = do
  nm <- fullTypeName' (if cStyle then "int" else "") Unspecified None declSpecs
  applyDeclrs cStyle isDirect nm ddrs

  where
    fullTypeName' :: String -> Lengthiness -> Signedness -> [CDeclarationSpecifier a] -> Output String
    fullTypeName' "" _ None []
        | cStyle    = return "void"
        | otherwise = return ""
    fullTypeName' "" _ Unsigned []
        | cStyle    = return "int"
        | otherwise = return "CUInt"
    fullTypeName' "" _ Signed []
        | cStyle    = return "int"
        | otherwise = return "CInt"

    fullTypeName' ty _ _ [] = return ty

    fullTypeName' ty l s (x:xs) = case x of
      CTypeSpec (CSignedType _)
          | cStyle    -> ("signed " ++) <$> fullTypeName' ty l Signed xs
          | otherwise -> fullTypeName' ty l Signed xs

      CTypeSpec (CUnsigType _)
          | cStyle    -> ("unsigned " ++) <$> fullTypeName' ty l Unsigned xs
          | otherwise -> fullTypeName' ty l Unsigned xs

      CTypeSpec tspec@(CLongType _)
        | cStyle    -> fullTypeName' ((case l of
                                          Long -> "long "
                                          Unspecified -> "") ++ cTypeName tspec) Long s xs
        | otherwise -> typeName tspec l s >>= \ty' -> fullTypeName' ty' Long s xs

      CTypeSpec tspec
        | cStyle    -> fullTypeName' ((case l of
                                          Long -> "long "
                                          Unspecified -> "") ++ cTypeName tspec) Unspecified s xs
        | otherwise -> typeName tspec l s >>= \ty' -> fullTypeName' ty' Unspecified s xs

      CTypeQual qual
        | cStyle -> do
          baseType <- fullTypeName' ty l s xs
          return $ let q = qualToStr qual
                   in if null q
                      then baseType
                      else q ++ " " ++ baseType

      _ -> fullTypeName' ty l s xs

concatM :: (Monad f, Functor f) => [f [a]] -> f [a]
concatM xs = concat <$> sequence xs

applyDeclrs :: Bool -> Bool -> String -> [CDerivedDeclarator a] -> Output String

applyDeclrs cStyle _isDirect baseType (CPtrDeclr {}:f@CFunDeclr {}:ds) = do
  baseType' <- applyDeclrs cStyle False baseType ds
  applyDeclrs cStyle False baseType' [f]

applyDeclrs cStyle isDirect baseType (CFunDeclr (Right (decls, _)) _ _:_)
  | cStyle    = renderList ", " (funTypes decls baseType)
  | otherwise = do
    argTypes <- renderList " -> " (funTypes decls (if null baseType
                                                   then "IO ()"
                                                   else baseType))
    return $ "FunPtr " ++ tyParens argTypes

  where renderList str xs = intercalate str <$> filter (not . null) <$> xs
        funTypes xs bt    = (++) <$> mapM (cdeclTypeName' cStyle isDirect) xs
                                 <*> pure [bt]

applyDeclrs cStyle isDirect baseType decl@(CPtrDeclr quals _:[])
  | cStyle && baseType == "" = applyDeclrs cStyle isDirect "void" decl
  | cStyle                  = return $ baseType ++ "*"
                                    ++ preQualsToString quals
  | baseType == ""          = return "Ptr ()"
  | baseType == "CChar"     = return "CString"
  | otherwise               = return $ "Ptr " ++ baseType

applyDeclrs cStyle isDirect baseType (CPtrDeclr quals _:xs)
  | cStyle    = concatM [ applyDeclrs cStyle isDirect baseType xs
                        , pure "*"
                        , pure (preQualsToString quals) ]
  | otherwise = concatM [ pure "Ptr "
                        , tyParens `fmap`
                              applyDeclrs cStyle isDirect baseType xs ]

applyDeclrs cStyle isDirect baseType (CArrDeclr quals _ _:xs)
  | cStyle    = concatM [ pure (sufQualsToString quals)
                        , applyDeclrs cStyle isDirect baseType xs
                        , pure "[]" ]
  | otherwise = concatM [ pure $ if isDirect then "" else "Ptr "
                        , tyParens `fmap`
                              applyDeclrs cStyle isDirect baseType xs ]

applyDeclrs _ _ baseType _ = return baseType

preQualsToString :: [CTypeQualifier a] -> String
preQualsToString = prefixWith ' ' . qualsToStr

prefixWith :: a -> [a] -> [a]
prefixWith _ [] = []
prefixWith x xs = x:xs

sufQualsToString :: [CTypeQualifier a] -> String
sufQualsToString = suffixWith ' ' . qualsToStr

suffixWith :: a -> [a] -> [a]
suffixWith _ [] = []
suffixWith x xs = xs ++ [x]

qualsToStr :: [CTypeQualifier a] -> String
qualsToStr = unwords . map qualToStr

qualToStr :: CTypeQualifier t -> String
qualToStr (CConstQual _)    = "const"
qualToStr (CVolatQual _)    = "volatile"
qualToStr (CRestrQual _)    = "restricted"
qualToStr (CAtomicQual _)   = "atomic"
qualToStr (CAttrQual _)     = ""
qualToStr (CNullableQual _) = ""
qualToStr (CNonnullQual _)  = ""

-- Simple translation from C types to Foreign.C.Types types.  We represent
-- Void as the empty string so that returning void becomes IO (), and passing
-- a void star becomes Ptr ().

typeName :: CTypeSpecifier a -> Lengthiness -> Signedness -> Output String

typeName (CVoidType _) _ _   = return ""
typeName (CFloatType _) _ _  = return "CFloat"
typeName (CDoubleType _) _ _ = return "CDouble"
typeName (CBoolType _) _ _   = return "CInt"

typeName (CCharType _) _ Signed   = return "CSChar"
typeName (CCharType _) _ Unsigned = return "CUChar"
typeName (CCharType _) _ _        = return "CChar"

typeName (CShortType _) _ Signed   = return "CShort"
typeName (CShortType _) _ Unsigned = return "CUShort"
typeName (CShortType _) _ _        = return "CShort"

typeName (CIntType _) Long Signed   = return "CLong"
typeName (CIntType _) Long Unsigned = return "CULong"
typeName (CIntType _) Long _        = return "CLong"

typeName (CIntType _) _ Signed   = return "CInt"
typeName (CIntType _) _ Unsigned = return "CUInt"
typeName (CIntType _) _ _        = return "CInt"

typeName (CLongType _) Long Signed   = return "CLLong"
typeName (CLongType _) Long Unsigned = return "CULLong"
typeName (CLongType _) Long _        = return "CLLong"

typeName (CLongType _) _ Signed   = return "CLong"
typeName (CLongType _) _ Unsigned = return "CULong"
typeName (CLongType _) _ _        = return "CLong"

typeName (CTypeDef (Ident nm _ _) _) _ _ = do
  definition <- lookupType nm
  case definition of
    Nothing -> return $ "<" ++ nm ++ ">"
    Just (Typedef { typedefName = defNm }) ->
      return defNm

typeName (CSUType (CStruct tag (Just (Ident nm _ _)) _ _ _) _) _ _ =
  return $ "<" ++ structTagPrefix tag ++ nm ++ ">"
typeName (CEnumType (CEnum (Just (Ident nm _ _)) _ _ _) _) _ _ =
  return $ "<enum " ++ nm ++ ">"

typeName (CComplexType _) _ _  = return ""
typeName (CTypeOfExpr _ _) _ _ = return ""
typeName (CTypeOfType _ _) _ _ = return ""

typeName _ _ _ = return ""

cTypeName :: CTypeSpecifier a -> String
cTypeName (CVoidType _)               = ""
cTypeName (CFloatType _)              = "float"
cTypeName (CDoubleType _)             = "double"
cTypeName (CBoolType _)               = "int"
cTypeName (CCharType _)               = "char"
cTypeName (CShortType _)              = "short"
cTypeName (CIntType _)                = "int"
cTypeName (CLongType _)               = "long"
cTypeName (CTypeDef (Ident nm _ _) _) = nm
cTypeName (CComplexType _)            = ""
cTypeName (CSUType _ _)               = ""
cTypeName (CEnumType _ _)             = ""
cTypeName (CTypeOfExpr _ _)           = ""
cTypeName (CTypeOfType _ _)           = ""

cTypeName _ = ""

tyParens :: String -> String
tyParens ty =
  if null ty || ' ' `elem` ty
    then concat ["(", ty, ")"]
    else ty

-- c2hsc.hs
