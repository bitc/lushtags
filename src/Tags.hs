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
import Language.Haskell.Exts.Annotated
    (
      ImportDecl(..)
    , Module(..)
    , ModuleHead(..)
    , ModuleName(..)
    , Decl(..)
    , DeclHead(..)
    , SrcSpan(..)
    , SrcSpanInfo(..)
    , Name(..)
    )

data Tag = Tag
    { tagName :: String
    , tagFile :: String
    , tagPattern :: String
    , tagKind :: TagKind
    , tagLine :: Int
    , tagScope :: Maybe String
    , tagSignature :: Maybe String
    }
    deriving (Eq, Ord, Show)

data TagKind
    = TModule
    | TImport
    | TType
    | TFunction
    deriving (Eq, Ord, Show)

tagKindLetter :: TagKind -> String
tagKindLetter TModule = "m"
tagKindLetter TImport = "i"
tagKindLetter TType = "t"
tagKindLetter TFunction = "f"

tagToString :: Tag -> String
tagToString tag =
    let scopeStr = maybe "" ("\tclass:"++) (tagScope tag)
        signatureStr = case tagSignature tag of
            Nothing -> ""
            Just sig -> "\tsignature:("++ sig ++ ")"
    in tagName tag ++ "\t" ++
            tagFile tag ++ "\t" ++
            tagPattern tag ++ ";\"\t" ++
            tagKindLetter (tagKind tag) ++
            "\tline:" ++ show (tagLine tag) ++
            scopeStr ++
            signatureStr

type FileLines = Vector String
type TagC = FileLines -> Tag

createTags :: (Module SrcSpanInfo, FileLines) -> [Tag]
createTags (Module _ mbHead _ imports decls, fileLines) =
    let moduleTag = case mbHead of
            Just (ModuleHead _ (ModuleName loc name) _ _) ->
                [tagC $ createTag name TModule Nothing Nothing loc]
            Nothing -> []
        importTags = map (tagC . createImportTag) imports
        declsTags = map tagC (concatMap createDeclTags decls)
    in moduleTag ++ importTags ++ declsTags
    where
        tagC :: TagC -> Tag
        tagC = ($ fileLines)
createTags _ = error "TODO Module is XmlPage/XmlHybrid (!)"

createTag :: String -> TagKind -> Maybe String -> Maybe String -> SrcSpanInfo -> TagC
createTag name kind scope signature (SrcSpanInfo (SrcSpan file line _ _ _) _) fileLines = Tag
    { tagName = name
    , tagFile = file
    -- TODO This probably needs to be escaped:
    , tagPattern = "/^" ++ (fileLines ! (line - 1)) ++ "$/"
    , tagKind = kind
    , tagLine = line
    , tagScope = scope
    , tagSignature = signature
    }

createImportTag :: ImportDecl SrcSpanInfo -> TagC
createImportTag (ImportDecl loc (ModuleName _ name) _ _ _ mbAlias _) =
    let signature = case mbAlias of
            Nothing -> Nothing
            Just (ModuleName _ alias) -> Just alias
    in createTag name TImport Nothing signature loc

createDeclTags :: Decl SrcSpanInfo -> [TagC]
createDeclTags (TypeDecl _ hd _) =
    let (name, loc) = extractDeclHead hd
    in [createTag name TType Nothing Nothing loc]
createDeclTags _ = []

extractDeclHead :: DeclHead SrcSpanInfo -> (String, SrcSpanInfo)
extractDeclHead (DHead _ name _) = extractName name
extractDeclHead (DHInfix _ _ name _) = extractName name
extractDeclHead (DHParen _ hd') = extractDeclHead hd'

extractName :: Name SrcSpanInfo -> (String, SrcSpanInfo)
extractName (Ident loc name) = (name, loc)
extractName (Symbol loc name) = (name, loc)
