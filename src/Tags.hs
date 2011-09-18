----------------------------------------------------------------------------
-- |
-- Module      :  Tags
-- License     :  MIT (see LICENSE)
-- Authors     :  Bit Connor <bit@mutantlemon.com>
--
-- Maintainer  :  Bit Connor <bit@mutantlemon.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- Extract Tags from a Haskell Module
--
-----------------------------------------------------------------------------

module Tags
    (
      Tag(..)
    , createTags
    , tagToString
    ) where

import Data.Vector(Vector, (!))
import Language.Haskell.Exts.Annotated (SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Pretty (prettyPrintStyleMode, Style(..), Mode(OneLineMode), defaultMode)

data Tag = Tag
    { tagName :: String
    , tagFile :: String
    , tagPattern :: String
    , tagKind :: TagKind
    , tagLine :: Int
    , tagParent :: Maybe (TagKind, String)
    , tagSignature :: Maybe String
    , tagAccess :: Maybe TagAccess
    }
    deriving (Eq, Ord, Show)

data TagKind
    = TModule
    | TExport
    | TImport
    | TType
    | TData
    | TNewType
    | TConstructor
    | TFunction
    deriving (Eq, Ord, Show)

-- Access modifiers comes from the C++ world. Haskell doesn't have them, but
-- they are useful for marking our tags with similar meanings. For example,
-- marking all exported functions as public, or marking qualified imports
-- differently from unqualified ones.
data TagAccess
    = AccessPublic
    | AccessPrivate
    | AccessProtected
    deriving (Eq, Ord, Show)

-- First letter of each kind name must be unique!
tagKindName :: TagKind -> String
tagKindName TModule = "module"
tagKindName TExport = "export"
tagKindName TImport = "import"
tagKindName TType = "type"
tagKindName TData = "data"
tagKindName TNewType = "newtype"
tagKindName TConstructor = "constructor"
tagKindName TFunction = "function"

tagKindLetter :: TagKind -> Char
tagKindLetter = head . tagKindName

tagToString :: Tag -> String
tagToString tag =
    let parentStr = case tagParent tag of
            Nothing -> ""
            Just (kind, name) -> "\t" ++ tagKindName kind ++ ":" ++ name
        signatureStr = case tagSignature tag of
            Nothing -> ""
            Just sig -> "\tsignature:("++ sig ++ ")"
        accessStr = case tagAccess tag of
            Nothing -> ""
            Just AccessPublic -> "\taccess:public"
            Just AccessPrivate -> "\taccess:private"
            Just AccessProtected -> "\taccess:protected"
    in tagName tag ++ "\t" ++
            tagFile tag ++ "\t" ++
            tagPattern tag ++ ";\"\t" ++
            tagKindLetter (tagKind tag) : "\t" ++
            "line:" ++ show (tagLine tag) ++
            parentStr ++
            signatureStr ++
            accessStr

type FileLines = Vector String
type TagC = FileLines -> Tag

createTags :: (Module SrcSpanInfo, FileLines) -> [Tag]
createTags (Module _ mbHead _ imports decls, fileLines) =
    let moduleTags = map tagC (maybe [] createModuleTags mbHead)
        importTags = map (tagC . createImportTag) imports
        declsTags = map tagC (concatMap createDeclTags decls)
        exportTags = filter ((==TExport) . tagKind) moduleTags
    -- TODO If there is no ModuleHead then apply public access modifier to all
    -- declarations tags
    in moduleTags ++ importTags ++ (applyAccessModifiers exportTags declsTags)
    where
        tagC :: TagC -> Tag
        tagC = ($ fileLines)
createTags _ = error "TODO Module is XmlPage/XmlHybrid (!)"

-- Apply a public access modifier to all declarations that are listed in the
-- module export specification
applyAccessModifiers :: [Tag] -> [Tag] -> [Tag]
applyAccessModifiers exportTags declTags = map applySingle declTags
    where
        applySingle :: Tag -> Tag
        applySingle tag =
            let name = tagName tag
                exported = any ((==name) . tagName) exportTags
            in if exported
                then tag { tagAccess = Just AccessPublic }
                else tag

createTag :: String -> TagKind -> Maybe (TagKind, String) -> Maybe String -> Maybe TagAccess -> SrcSpanInfo -> TagC
createTag name kind parent signature access (SrcSpanInfo (SrcSpan file line _ _ _) _) fileLines = Tag
    { tagName = name
    , tagFile = file
    -- TODO This probably needs to be escaped:
    , tagPattern = "/^" ++ (fileLines ! (line - 1)) ++ "$/"
    , tagKind = kind
    , tagLine = line
    , tagParent = parent
    , tagSignature = signature
    , tagAccess = access
    }

createModuleTags :: ModuleHead SrcSpanInfo -> [TagC]
createModuleTags (ModuleHead _ (ModuleName moduleLoc moduleName) _ mbExportSpecList) =
    case mbExportSpecList of
        Nothing -> [moduleTag]
        Just (ExportSpecList _ exports) ->
            moduleTag : map createExportTag exports
    where
        moduleTag = createTag moduleName TModule Nothing Nothing Nothing moduleLoc
        createExportTag :: ExportSpec SrcSpanInfo -> TagC
        createExportTag exportSpec =
            let (name, loc) = extractExportSpec exportSpec
            in createTag name TExport Nothing Nothing Nothing loc

createImportTag :: ImportDecl SrcSpanInfo -> TagC
createImportTag (ImportDecl loc (ModuleName _ name) qualified _ _ mbAlias mbSpecs) =
    let signature = case mbAlias of
            Nothing -> Nothing
            Just (ModuleName _ alias) -> Just alias
        access = case mbSpecs of
            Just (ImportSpecList _ False _) ->
                if qualified then Just AccessProtected else Nothing
            Just (ImportSpecList _ True _) ->
                -- imported names are excluded by 'hiding'
                if qualified then Just AccessProtected else Just AccessPrivate
            Nothing -> if qualified then Just AccessProtected else Just AccessPublic
    in createTag name TImport Nothing signature access loc

createDeclTags :: Decl SrcSpanInfo -> [TagC]
createDeclTags (TypeDecl _ hd _) =
    let (name, loc) = extractDeclHead hd
    in [createTag name TType Nothing Nothing Nothing loc]
createDeclTags (DataDecl _ dataOrNew _ hd constructors _) =
    let (name, loc) = extractDeclHead hd
        kind = case dataOrNew of
            DataType _ -> TData
            NewType _ -> TNewType
        dataTag = createTag name kind Nothing Nothing Nothing loc
    in dataTag : map (createConstructorTag (kind, name)) constructors
createDeclTags (TypeSig _ names t) =
    map createFunctionTag names
    where
        sig = prettyPrintStyleMode (Style OneLineMode 0 0) defaultMode t
        createFunctionTag :: Name SrcSpanInfo -> TagC
        createFunctionTag name =
            let (n, loc) = extractName name
            in createTag n TFunction Nothing (Just sig) Nothing loc
createDeclTags _ = []

-- TODO Also create tags for record fields
createConstructorTag :: (TagKind, String) -> QualConDecl SrcSpanInfo -> TagC
createConstructorTag parent (QualConDecl _ _ _ con) =
    let (name, loc) = extractConDecl con
    in createTag name TConstructor (Just parent) Nothing Nothing loc

extractExportSpec :: ExportSpec SrcSpanInfo -> (String, SrcSpanInfo)
extractExportSpec (EVar _ name) = extractQName name
extractExportSpec (EAbs _ name) = extractQName name
extractExportSpec (EThingAll _ name) = extractQName name
extractExportSpec (EThingWith _ name _) = extractQName name
extractExportSpec (EModuleContents _ (ModuleName loc name)) = (name, loc)

extractDeclHead :: DeclHead SrcSpanInfo -> (String, SrcSpanInfo)
extractDeclHead (DHead _ name _) = extractName name
extractDeclHead (DHInfix _ _ name _) = extractName name
extractDeclHead (DHParen _ hd') = extractDeclHead hd'

extractConDecl :: ConDecl SrcSpanInfo -> (String, SrcSpanInfo)
extractConDecl (ConDecl _ name _) = extractName name
extractConDecl (InfixConDecl _ _ name _) = extractName name
extractConDecl (RecDecl _ name _) = extractName name

extractName :: Name SrcSpanInfo -> (String, SrcSpanInfo)
extractName (Ident loc name) = (name, loc)
extractName (Symbol loc name) = (name, loc)

extractQName :: QName SrcSpanInfo -> (String, SrcSpanInfo)
extractQName (Qual loc (ModuleName _ moduleName) name) =
    (moduleName ++ "." ++ fst (extractName name), loc)
extractQName (UnQual loc name) = (fst (extractName name), loc)
extractQName (Special loc _) = ("_special_", loc)  -- TODO
