module Main where

-- setupParse "/usr/bin/gcc" ["-U__BLOCKS__", "/Users/johnw/src/hlibgit2/libgit2/include/git2/types.h"]

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Language.C.Data.Ident
import Language.C.Data.InputStream
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Syntax.AST
import Language.C.System.GCC
import Language.C.System.Preprocess
import Prelude
import System.Directory
import System.Environment
import Text.StringTemplate

------------------------------ IMPURE FUNCTIONS ------------------------------

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
      let HscOutput hscs helpercs =
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

data HscOutput = HscOutput [String] [String]

type Output = State HscOutput

newHscState :: HscOutput
newHscState = HscOutput [] []

appendHsc :: String -> Output ()
appendHsc hsc = do
  HscOutput hscs xs <- get
  put $ HscOutput (hscs ++ [hsc]) xs

appendHelper :: String -> Output ()
appendHelper helperc = do
  HscOutput xs helpercs <- get
  put $ HscOutput xs (helpercs ++ [helperc])

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
    generateHsc = mapM_ printNode . filter declInFile

    declInFile :: CExtDecl -> Bool
    declInFile = (fileName ==) . infoFile . declInfo

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

printNode :: CExtDecl -> Output ()

printNode (CDeclExt (CDecl declSpecs items _)) =
  forM_ items $ \(declrtr, _, _) ->
    case declrtr of
      Nothing -> return ()
      Just d@(CDeclr ident ddrs _ _ _) ->
        case ident of
          Nothing -> return ()
          Just (Ident name _ _) ->
            case ddrs of
              CFunDeclr (Right (_, _)) _ _ : _ ->
                appendFunc "#ccall" declSpecs d
              _ ->
                appendType declSpecs name

printNode (CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume that defined functions in headers are inline functions
  appendFunc "#cinline" declSpecs declrtr

printNode (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

appendFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator a -> Output ()
appendFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let name' = nameFromIdent ident
      tmpl  = "$marker$ $name$ , $argTypes;separator=' -> '$ -> IO ($retType$)"
      code  = newSTMP tmpl
      code' = setAttribute "argTypes" (getArgTypes (head ddrs)) code

  appendHsc $ toString $ setManyAttrib
    [ ("marker",  marker)
    , ("name",    name')
    , ("retType", derDeclrTypeName declSpecs (tail ddrs)) ] code'

  where
    getArgTypes (CFunDeclr (Right (decls, _)) _ _) = map cdeclTypeName decls
    getArgTypes _ = []

    nameFromIdent :: Maybe Ident -> String
    nameFromIdent name = case name of
      Just (Ident n _ _) -> n
      _ -> "<no name>"

appendType :: [CDeclarationSpecifier a] -> String -> Output ()
appendType declSpecs name = mapM_ appendType' declSpecs
  where
    appendType' (CTypeSpec (CSUType (CStruct _ ident decls _ _) _)) =
      case ident of
        Nothing -> return ()
        Just (Ident name' _ _) ->
          case decls of
            Nothing -> appendHsc $ "#opaque_t " ++ name'
            Just xs -> do
              appendHsc $ "#starttype " ++ name'
              appendHsc "#stoptype"

    appendType' _ = return ()

-- The remainder of this file is some hairy code for turning various
-- constructs into Bindings-DSL type names, such as turning "int ** foo" into
-- the type name "Ptr (Ptr CInt)".

data Signedness = None | Signed | Unsigned deriving (Eq, Show, Enum)

cdeclTypeName :: CDeclaration a -> String
cdeclTypeName (CDecl declSpecs more _) =
  case more of
    (Just x, _, _) : _ -> declrTypeName declSpecs x
    _                  -> declSpecTypeName declSpecs

declSpecTypeName :: [CDeclarationSpecifier a] -> String
declSpecTypeName = flip derDeclrTypeName []

declrTypeName :: [CDeclarationSpecifier a] -> CDeclarator a -> String
declrTypeName declSpecs (CDeclr _ ddrs _ _ _) = derDeclrTypeName declSpecs ddrs

derDeclrTypeName :: [CDeclarationSpecifier a] -> [CDerivedDeclarator a]
                   -> String
derDeclrTypeName declSpecs ddrs =
  applyPointers (fullTypeName' None declSpecs) ddrs
  where
    fullTypeName' :: Signedness -> [CDeclarationSpecifier a] -> String
    fullTypeName' _ []     = ""
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

typeName :: CTypeSpecifier a -> Signedness -> String

typeName (CVoidType _) _   = ""
typeName (CFloatType _) _  = "CFloat"
typeName (CDoubleType _) _ = "CDouble"
typeName (CBoolType _) _   = "CInt"

typeName (CCharType _) s =
  case s of
    Signed   -> "CSChar"
    Unsigned -> "CUChar"
    _        -> "CChar"
typeName (CShortType _) s =
  case s of
    Signed   -> "CShort"
    Unsigned -> "CUShort"
    _        -> "CShort"
typeName (CIntType _) s =
  case s of
    Signed   -> "CInt"
    Unsigned -> "CUInt"
    _        -> "CInt"
typeName (CLongType _) s =
  case s of
    Signed   -> "CLong"
    Unsigned -> "CULong"
    _        -> "CLong"

typeName (CTypeDef (Ident name _ _) _) _ = "<" ++ name ++ ">"

typeName (CComplexType _) _  = ""
typeName (CSUType _ _) _     = ""
typeName (CEnumType _ _) _   = ""
typeName (CTypeOfExpr _ _) _ = ""
typeName (CTypeOfType _ _) _ = ""

typeName _ _ = ""

-- c2hsc.hs
