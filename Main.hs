module Main where

import Control.Monad
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

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

main :: IO()
main = do
  gccExe <- findExecutable "gcc"
  case gccExe of
    Nothing   -> error "Cannot find 'gcc' executable on the PATH"
    Just path -> getArgs >>= setupParse path

-- Once gcc is found, setup for parsing the C file by running the
-- preprocessor, and identifying our input file absolutely so that we know
-- which declarations to print out as Bindings-DSL macros.

setupParse :: FilePath -> [String] -> IO ()
setupParse gccPath args = do
  fileName <- canonicalizePath $ last args
  result   <- runPreprocessor (newGCC gccPath)
                             (rawCppArgs (init args) fileName)
  case result of
    Left err     -> error $ "Failed to run cpp on: " ++ show err
    Right stream -> parseCFile stream fileName (initPos fileName)

-- Now we are ready to parse the C code from the preprocessed input stream,
-- located in the given file and starting at the specified position.  The
-- result of a parse is a list of global declarations, so filter the list down
-- to those occurring in the target file, and then print the declarations in
-- Bindings-DSL format.

parseCFile :: InputStream -> FilePath -> Position -> IO ()
parseCFile stream fileName pos =
  case parseC stream pos of
    Left err -> error $ "Failed to compile: " ++ show err
    Right (CTranslUnit decls _) ->
      mapM_ printNode $ filter (declInFile fileName) decls

declInFile :: FilePath -> CExtDecl -> Bool
declInFile fileName = (fileName ==) . infoFile . declInfo

declInfo :: CExtDecl -> NodeInfo
declInfo (CDeclExt (CDecl _ _ info))       = info
declInfo (CFDefExt (CFunDef _ _ _ _ info)) = info
declInfo (CAsmExt _ info)                  = info

infoFile :: NodeInfo -> String
infoFile = posFile . posOfNode

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

printNode :: CExtDecl -> IO ()

printNode (CDeclExt (CDecl declSpecs items _)) =
  forM_ items $ \(declrtr, _, _) ->
    case declrtr of
      Nothing -> return ()
      Just d@(CDeclr ident ddrs _ _ _) ->
        case ident of
          Nothing -> return ()
          Just (Ident {}) ->
            case ddrs of
              CFunDeclr (Right (_, _)) _ _ : _ ->
                printFunc "#ccall" declSpecs d
              _ -> return ()

printNode (CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume that defined functions in headers are inline functions
  printFunc "#cinline" declSpecs declrtr

printNode (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

printFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator b -> IO ()
printFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let name' = nameFromIdent ident
      tmpl  = "$marker$ $name$ , $argTypes;separator=' -> '$ -> IO ($retType$)"
      code  = newSTMP tmpl
      code' = setAttribute "argTypes" (getArgTypes (head ddrs)) code
  putStrLn $ toString $ setManyAttrib
    [ ("marker",  marker)
    , ("name",    name')
    , ("retType", applyPointers fullType (tail ddrs)) ] code'
  where fullType = fullTypeName declSpecs

nameFromIdent :: Maybe Ident -> String
nameFromIdent name = case name of
  Just (Ident n _ _) -> n
  _ -> "<no name>"

-- The remainder of this file is some hairy code for turning declarators into
-- full type names, such as turning "int ** foo" into the type name "Ptr (Ptr
-- <CInt>)".  This is really the hardest job done by this utility.

getArgTypes :: CDerivedDeclarator a -> [String]

getArgTypes (CFunDeclr (Left _) _ _) = undefined
getArgTypes (CFunDeclr (Right (decls, _)) _ _) =
  flip map decls $
    \(CDecl declSpecs more _) ->
      let baseType = fullTypeName declSpecs in
      -- Determine if the declarator (if there is one) has pointer type
      case more of
        (Just x, _, _) : _ -> applyPointersToDeclr baseType x
        _                  -> baseType

getArgTypes (CPtrDeclr {}) = undefined
getArgTypes (CArrDeclr {}) = undefined

applyPointersToDeclr :: String -> CDeclarator a -> String
applyPointersToDeclr baseType (CDeclr _ dds _ _ _) =
  applyPointers baseType dds

applyPointers :: String -> [CDerivedDeclarator a] -> String
applyPointers baseType [] = baseType
applyPointers baseType (x:[]) =
  case x of
    CPtrDeclr _ _ -> if baseType == ""
                     then "Ptr ()"
                     else "Ptr <" ++ baseType ++ ">"
    _ -> ""
applyPointers baseType (x:xs) =
  case x of
    CPtrDeclr _ _ -> "Ptr (" ++ applyPointers baseType xs ++ ")"
    _ -> ""

data Signedness = None | Signed | Unsigned
                deriving (Eq, Show, Enum)

fullTypeName :: [CDeclarationSpecifier a] -> String
fullTypeName = fullTypeName' None
  where
    fullTypeName' :: Signedness -> [CDeclarationSpecifier a] -> String
    fullTypeName' _ []     = ""
    fullTypeName' s (x:xs) =
      case x of
        CTypeSpec (CSignedType _) -> fullTypeName' Signed xs
        CTypeSpec (CUnsigType _)  -> fullTypeName' Unsigned xs
        CTypeSpec tspec           -> fromMaybe "" $ typeName tspec s
        _                         -> fullTypeName' s xs

-- Simple translation from C types to Foreign.C.Types types.  We represent
-- Void as the empty string so that returning void becomes IO (), and passing
-- a void star becomes Ptr ().

typeName :: CTypeSpecifier a -> Signedness -> Maybe String

typeName (CVoidType _) _   = Just ""
typeName (CFloatType _) _  = Just "CFloat"
typeName (CDoubleType _) _ = Just "CDouble"
typeName (CBoolType _) _   = Just "CInt"

typeName (CCharType _) s =
  case s of
    Signed   -> Just "CSChar"
    Unsigned -> Just "CUChar"
    _        -> Just "CChar"
typeName (CShortType _) s =
  case s of
    Signed   -> Just "CShort"
    Unsigned -> Just "CUShort"
    _        -> Just "CShort"
typeName (CIntType _) s =
  case s of
    Signed   -> Just "CInt"
    Unsigned -> Just "CUInt"
    _        -> Just "CInt"
typeName (CLongType _) s =
  case s of
    Signed   -> Just "CLong"
    Unsigned -> Just "CULong"
    _        -> Just "CLong"

typeName (CTypeDef (Ident name _ _) _) _ = Just name

--typeName (CComplexType _)  = Nothing
--typeName (CSUType _ _)     = Nothing
--typeName (CEnumType _ _)   = Nothing
--typeName (CTypeOfExpr _ _) = Nothing
--typeName (CTypeOfType _ _) = Nothing

typeName _ _ = Nothing

-- c2hsc.hs
