----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- License     :  MIT (see LICENSE)
-- Authors     :  Bit Connor <bit@mutantlemon.com>
--
-- Maintainer  :  Bit Connor <bit@mutantlemon.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- lushtags, Create ctags compatible tags files for Haskell programs
--
-----------------------------------------------------------------------------

module Main (main) where

import Data.List (isPrefixOf)
import Data.Vector(Vector, fromList)
import Language.Haskell.Exts.Annotated (parseFileContentsWithMode, ParseMode(..), knownExtensions, ParseResult(ParseOk, ParseFailed))
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T (unpack, lines, pack, unlines)
import qualified Data.Text.IO as T (readFile, putStr)

import Tags (Tag, createTags, tagToString)

main :: IO ()
main = do
    rawArgs <- getArgs
    let files = dropOptions rawArgs
    case files of
        [] -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " [--] <file>"
        filename:_ -> processFile filename >>= printTags

printTags :: [Tag] -> IO ()
printTags tags =
    T.putStr $ T.unlines $ map (T.pack . tagToString) tags

processFile :: FilePath -> IO [Tag]
processFile file = do
    (fileContents, fileLines) <- loadFile file
    case parseFileContentsWithMode parseMode fileContents of
        ParseFailed loc message ->
            -- TODO Better error reporting
            fail $ "Parse error: " ++ show loc ++ ": " ++ message
        ParseOk parsedModule -> return $ createTags (parsedModule, fileLines)
    where
        parseMode = ParseMode
            { parseFilename = file
            , extensions = knownExtensions
            , ignoreLanguagePragmas = False
            , ignoreLinePragmas = True
            , fixities = Nothing
            }

loadFile :: FilePath -> IO (String, Vector String)
loadFile file = do
    text <- T.readFile file
    let fullContents = T.unpack text
    let textLines = T.lines text
    let stringLines = map T.unpack textLines
    let fileLines = fromList stringLines
    return (fullContents, fileLines)

dropOptions :: [String] -> [String]
dropOptions args =
    let (start, end) = span (/= "--") args
    in  filter (not . isPrefixOf "-") start ++ dropSep end
    where
        dropSep ("--":xs) = xs
        dropSep xs        = xs
