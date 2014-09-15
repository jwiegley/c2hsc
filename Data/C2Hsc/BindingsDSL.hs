module Data.C2Hsc.BindingsDSL where

import           Control.Applicative
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.State
import           Data.C2Hsc
import           Data.Foldable hiding (concat, elem, mapM_)
import           Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable hiding (mapM, forM)
import           Language.C.Data.Ident
import           Language.C.Pretty
import           Language.C.Syntax.AST
import           Prelude hiding (concat, sequence, mapM, mapM_, foldr)
import           Text.PrettyPrint as P hiding ((<>))
import           Text.StringTemplate

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

appendNode :: FilePath -> (FilePath -> Bool) -> CExtDecl -> Output ()

appendNode _ fm dx@(CDeclExt (CDecl declSpecs items _)) =
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

            _ ->
              -- If the type is a typedef, record the equivalence so we can
              -- look it up later
              case declSpecs of
                CStorageSpec (CTypedef _):_ -> do
                  when (declMatches fm dx) $ do
                    appendHsc $ "{- " ++ P.render (pretty dx) ++ " -}"
                    appendType declSpecs nm

                  dname <- declSpecTypeName declSpecs
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
                    dname <- declSpecTypeName declSpecs
                    appendHsc $ "#globalvar " ++ nm ++ " , " ++ dname
  where
    splitDecl declrtr = do      -- in the Maybe Monad
      d@(CDeclr ident ddrs _ _ _) <- declrtr
      return (d, ddrs, case ident of Just (Ident nm _ _) -> nm; _ -> "")

appendNode _ fm dx@(CFDefExt (CFunDef declSpecs declrtr _ _ _)) =
  -- Assume functions defined in headers are inline functions
  when (declMatches fm dx) $ do
    appendFunc "#cinline" declSpecs declrtr

    let CDeclr ident ddrs _ _ _ = declrtr

    for_ ident $ \(Ident nm _ _) ->
      case head ddrs of
        CFunDeclr (Right (decls, _)) _ _ -> do
          retType <- derDeclrTypeName' True declSpecs (tail ddrs)
          funType <- applyDeclrs True retType ddrs
          appendHelper $
            "BC_INLINE" ++ show (length decls)
            ++ (if not (null retType) then "" else "VOID")
            ++ "(" ++ nm ++ ", " ++ funType ++ ")"
        _ -> return ()

appendNode _ _ (CAsmExt _ _) = return ()

-- Print out a function as #ccall or #cinline.  The syntax is the same for
-- both externs and inlines, except that we want to do extra work for inline
-- and create a helper file with some additional macros.

appendFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator a -> Output ()
appendFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let _:retDeclr:_ = splitWhen isFuncDeclr ddrs
      funcDeclr:_  = dropWhile (not . isFuncDeclr) ddrs

  retType  <- derDeclrTypeName declSpecs retDeclr
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

    getArgTypes' (CFunDeclr (Right (decls, _)) _ _) = map cdeclTypeName decls
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
                tname <- derDeclrTypeName declSpecs' zs
                appendHsc $ "#array_field " ++ declName ++ " , " ++ tname
              _ -> do
                tname <- cdeclTypeName x
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

cdeclNames :: CDeclaration a -> [String]
cdeclNames (CDecl _ more _) =
  collect more []
  where
    collect []     nms = reverse nms
    collect (m:ms) nms = collect ms $
      case m of
        (Just (CDeclr (Just (Ident nm _ _)) _ _ _ _), _, _)
          -> nm:nms
        _ ->    nms

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
        return $ let q = qualToStr qual
                 in if null q
                    then baseType
                    else q ++ " " ++ baseType
      else
        fullTypeName' s xs

    fullTypeName' _ (CTypeSpec (CSignedType _):[]) =
      return $ if cStyle then "signed" else "CInt"
    fullTypeName' _ (CTypeSpec (CUnsigType _):[]) =
      return $ if cStyle then "unsigned" else "CUInt"

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
    argTypes <- renderList " -> " (funTypes decls (if null baseType
                                                   then "IO ()"
                                                   else baseType))
    return $ "FunPtr " ++ tyParens argTypes

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
  | otherwise = concatM [ pure "Ptr "
                        , tyParens `fmap` applyDeclrs cStyle baseType xs ]

applyDeclrs cStyle baseType (CArrDeclr quals _ _:xs)
  | cStyle    = concatM [ pure (sufQualsToString quals)
                        , applyDeclrs cStyle baseType xs
                        , pure "[]" ]
  | otherwise = concatM [ pure "Ptr "
                        , tyParens `fmap` applyDeclrs cStyle baseType xs ]

applyDeclrs _ baseType _ = return baseType

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

typeName (CSUType (CStruct tag (Just (Ident nm _ _)) _ _ _) _) _ =
  return $ "<" ++ structTagPrefix tag ++ nm ++ ">"
typeName (CEnumType (CEnum (Just (Ident nm _ _)) _ _ _) _) _ =
  return $ "<enum " ++ nm ++ ">"

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

cTypeName (CTypeDef (Ident nm _ _) _) _ = return nm

cTypeName (CComplexType _) _  = return ""
cTypeName (CSUType _ _) _     = return ""
cTypeName (CEnumType _ _) _   = return ""
cTypeName (CTypeOfExpr _ _) _ = return ""
cTypeName (CTypeOfType _ _) _ = return ""

cTypeName _ _ = return ""

tyParens :: String -> String
tyParens ty =
  if null ty || ' ' `elem` ty
    then concat ["(", ty, ")"]
    else ty
