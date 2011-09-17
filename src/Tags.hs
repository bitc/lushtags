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
    , SrcSpan(..)
    , SrcSpanInfo(..)
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
    | TFunction
    deriving (Eq, Ord, Show)

tagKindLetter :: TagKind -> String
tagKindLetter TModule = "m"
tagKindLetter TImport = "i"
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

type TagC = Vector String -> Tag

createTags :: (Module SrcSpanInfo, Vector String) -> [Tag]
createTags (Module _ mbHead _ imports _, fileLines) =
    let moduleTag = case mbHead of
            Just (ModuleHead _ (ModuleName loc name) _ _) ->
                [tagC $ createTag name TModule Nothing Nothing loc]
            Nothing -> []
        importTags = map (tagC . createImportTag) imports
    in moduleTag ++ importTags
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
