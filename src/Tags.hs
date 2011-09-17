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
    in tagName tag ++ "\t" ++ tagFile tag ++ "\t" ++ tagPattern tag ++ ";\"\t" ++ tagKindLetter (tagKind tag) ++ "\tline:" ++ show (tagLine tag) ++ scopeStr

createTags :: (Module SrcSpanInfo, Vector String) -> [Tag]
createTags (Module _ mbHead _ imports _, fileLines) =
    let moduleTag = case mbHead of
            Just (ModuleHead _ (ModuleName loc name) _ _) -> [createTag name TModule Nothing loc]
            Nothing -> []
        importTags = map createImportTag imports
    in moduleTag ++ importTags
    where
        createImportTag :: ImportDecl SrcSpanInfo -> Tag
        createImportTag (ImportDecl loc (ModuleName _ name) _ _ _ _ _) = createTag name TImport Nothing loc
        createTag :: String -> TagKind -> Maybe String -> SrcSpanInfo -> Tag
        createTag name kind scope (SrcSpanInfo (SrcSpan file line _ _ _) _) = Tag
            { tagName = name
            , tagFile = file
            , tagPattern = patternFromLine line
            , tagKind = kind
            , tagLine = line
            , tagScope = scope
            }
        patternFromLine :: Int -> String
        patternFromLine lineNumber =
            let index = lineNumber - 1
                line = fileLines ! index
            in "/^" ++ line ++ "$/"
createTags _ = error "TODO Module is XmlPage/XmlHybrid (!)"

