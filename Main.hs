module Main where

-- parseFile "/usr/bin/gcc" ["-U__BLOCKS__", "/Users/johnw/src/hlibgit2/libgit2/include/git2/types.h"]

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable hiding (mapM_)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Language.C.Data.Ident
import           Language.C.Data.InputStream
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Parser
import           Language.C.Pretty
import           Language.C.Syntax.AST
import           Language.C.System.GCC
import           Language.C.System.Preprocess
import           Prelude
import           System.Directory
import           System.Environment
import           Text.PrettyPrint as P
import           Text.StringTemplate

------------------------------ IMPURE FUNCTIONS ------------------------------
import Control.Applicative

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

main :: IO ()
main = do
  gccExe <- findExecutable "gcc"
  case gccExe of
    Nothing      -> error "Cannot find 'gcc' executable on the PATH"
    Just gccPath -> getArgs >>= parseFile gccPath

-- Once gcc is found, setup to parse the C file by running the preprocessor.
-- Then, identify the input file absolutely so we know which declarations to
-- print out at the end.

parseFile :: FilePath -> [String] -> IO ()
parseFile gccPath args = do
  fileName <- canonicalizePath $ last args
  result   <- runPreprocessor (newGCC gccPath)
                              (rawCppArgs (init args) fileName)
  case result of
    Left err     -> error $ "Failed to run cpp: " ++ show err
    Right stream -> do
      let HscOutput hscs helpercs _ =
            execState (parseCFile stream fileName (initPos fileName))
                      newHscState
      writeProducts hscs helpercs

-- Write out the gathered data

writeProducts :: [String] -> [String] -> IO ()
writeProducts hscs helpercs = do
  mapM_ putStrLn hscs
  mapM_ putStrLn helpercs

------------------------------- PURE FUNCTIONS -------------------------------

-- Rather than writing to the .hsc and .hsc.helper.c files directly from the
-- IO monad, they are collected in an HscOutput value in the State monad.  The
-- actual writing is done by writeProducts.  This keeps all the code below
-- pure, and since the data sets involved are relatively small, performance is
-- not a critical issue.

type TypeMap   = M.Map String String
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

defineType :: String -> String -> Output ()
defineType key value = do
  HscOutput xs ys types <- get
  put $ HscOutput xs ys (M.insert key value types)

lookupType :: String -> Output (Maybe String)
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
    generateHsc = mapM_ (appendNode fileName)

declInFile :: FilePath -> CExtDecl -> Bool
declInFile fileName = (fileName ==) . infoFile . declInfo

infoFile :: NodeInfo -> String
infoFile = posFile . posOfNode

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
  for_ items $ \(declrtr, _, _) -> do
    for_ (splitDecl declrtr) $ \(d, ddrs, name) ->
      case ddrs of
        CFunDeclr (Right (_, _)) _ _ : _ ->
          when (declInFile fp dx) $
            appendFunc "#ccall" declSpecs d

        _ -> do
          when (declInFile fp dx) $ do
            appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
            appendType declSpecs name

          -- If the type is a typedef, record the equivalence so we can look
          -- it up later, when a CTypeDef is encountered (see 'typeName',
          -- below)
          case head declSpecs of
            CStorageSpec (CTypedef _) ->
              declSpecTypeName declSpecs >>= defineType name
            _ -> return ()
  where
    splitDecl declrtr = do
      -- Take advantage of the Maybe monad to save us some effort
      d@(CDeclr ident ddrs _ _ _) <- declrtr
      (Ident name _ _)            <- ident
      return (d, ddrs, name)

appendNode fp dx@(CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume functions defined in headers are inline functions
  when (declInFile fp dx) $
    appendFunc "#cinline" declSpecs declrtr

appendNode _ (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

appendFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator a -> Output ()
appendFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  retType  <- derDeclrTypeName declSpecs (tail ddrs)
  argTypes <- sequence $ getArgTypes (head ddrs)

  let name' = nameFromIdent ident
      tmpl  = "$marker$ $name$ , $argTypes;separator=' -> '$ -> IO ($retType$)"
      code  = newSTMP tmpl
      code' = setAttribute "argTypes" argTypes code

  appendHsc $ toString $ setManyAttrib [ ("marker",  marker)
                                       , ("name",    name')
                                       , ("retType", retType) ] code'
  where
    getArgTypes (CFunDeclr (Right (decls, _)) _ _) = map cdeclTypeName decls
    getArgTypes _ = []

    nameFromIdent :: Maybe Ident -> String
    nameFromIdent name = case name of
      Just (Ident n _ _) -> n
      _ -> "<no name>"

appendType :: [CDeclarationSpecifier a] -> String -> Output ()
appendType declSpecs _ = mapM_ appendType' declSpecs
  where
    appendType' (CTypeSpec (CSUType (CStruct _ ident decls _ _) _)) =
      for_ ident $ \(Ident name _ _) ->
        case decls of
          Nothing -> appendHsc $ "#opaque_t " ++ name
          Just xs -> do
            appendHsc $ "#starttype " ++ name
            for_ xs $ \x ->
              for_ (cdeclName x) $ \declName -> do
                tname <- cdeclTypeName x
                appendHsc $ "#field " ++ declName ++ " , " ++ tname
            appendHsc "#stoptype"

    appendType' (CTypeSpec (CEnumType (CEnum ident defs _ _) _)) = do
      for_ ident $ \(Ident name _ _) ->
        appendHsc $ "#integral_t " ++ name

      for_ defs $ \ds ->
        for_ ds $ \((Ident name _ _), _) -> do
          appendHsc $ "#num " ++ name

    appendType' _ = return ()

-- The remainder of this file is some hairy code for turning various
-- constructs into Bindings-DSL type names, such as turning "int ** foo" into
-- the type name "Ptr (Ptr CInt)".

data Signedness = None | Signed | Unsigned deriving (Eq, Show, Enum)

cdeclName :: CDeclaration a -> Maybe String
cdeclName (CDecl _ more _) =
  case more of
    (Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _) : _ -> Just name
    _ -> Nothing

cdeclTypeName :: CDeclaration a -> Output String
cdeclTypeName (CDecl declSpecs more _) =
  case more of
    (Just x, _, _) : _ -> declrTypeName declSpecs x
    _                  -> declSpecTypeName declSpecs

declSpecTypeName :: [CDeclarationSpecifier a] -> Output String
declSpecTypeName = flip derDeclrTypeName []

declrTypeName :: [CDeclarationSpecifier a] -> CDeclarator a -> Output String
declrTypeName declSpecs (CDeclr _ ddrs _ _ _) = derDeclrTypeName declSpecs ddrs

derDeclrTypeName :: [CDeclarationSpecifier a] -> [CDerivedDeclarator a]
                   -> Output String
derDeclrTypeName declSpecs ddrs =
  applyPointers <$> fullTypeName' None declSpecs <*> pure ddrs
  where
    fullTypeName' :: Signedness -> [CDeclarationSpecifier a] -> Output String
    fullTypeName' _ []     = return ""
    fullTypeName' s (x:xs) =
      case x of
        CTypeSpec (CSignedType _) -> fullTypeName' Signed xs
        CTypeSpec (CUnsigType _)  -> fullTypeName' Unsigned xs
        CTypeSpec tspec           -> typeName tspec s
        _                         -> fullTypeName' s xs

    applyPointers :: String -> [CDerivedDeclarator a] -> String
    applyPointers baseType [] = baseType
    applyPointers baseType (x:[]) =
      case x of
        CPtrDeclr _ _ -> if baseType == ""
                         then "Ptr ()"
                         else "Ptr " ++ baseType
        _ -> ""
    applyPointers baseType (x:xs) =
      case x of
        CPtrDeclr _ _ -> "Ptr (" ++ applyPointers baseType xs ++ ")"
        _ -> ""

-- Simple translation from C types to Foreign.C.Types types.  We represent
-- Void as the empty string so that returning void becomes IO (), and passing
-- a void star becomes Ptr ().

typeName :: CTypeSpecifier a -> Signedness -> Output String

typeName (CVoidType _) _   = return $ ""
typeName (CFloatType _) _  = return $ "CFloat"
typeName (CDoubleType _) _ = return $ "CDouble"
typeName (CBoolType _) _   = return $ "CInt"

typeName (CCharType _) s   = case s of
                               Signed   -> return $ "CSChar"
                               Unsigned -> return $ "CUChar"
                               _        -> return $ "CChar"
typeName (CShortType _) s  = case s of
                               Signed   -> return $ "CShort"
                               Unsigned -> return $ "CUShort"
                               _        -> return $ "CShort"
typeName (CIntType _) s    = case s of
                               Signed   -> return $ "CInt"
                               Unsigned -> return $ "CUInt"
                               _        -> return $ "CInt"
typeName (CLongType _) s   = case s of
                               Signed   -> return $ "CLong"
                               Unsigned -> return $ "CULong"
                               _        -> return $ "CLong"

typeName (CTypeDef (Ident name _ _) _) _ = do
  definition <- lookupType name
  return $ fromMaybe ("<" ++ name ++ ">") definition

typeName (CComplexType _) _  = return $ ""
typeName (CSUType _ _) _     = return $ ""
typeName (CEnumType _ _) _   = return $ ""
typeName (CTypeOfExpr _ _) _ = return $ ""
typeName (CTypeOfType _ _) _ = return $ ""

typeName _ _ = return $ ""

-- c2hsc.hs
