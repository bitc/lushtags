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

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List (isPrefixOf, partition, intercalate)
import Data.Vector (Vector, fromList)
import Language.Haskell.Exts (parseFileContentsWithMode)
import Language.Haskell.Exts.Parser ( ParseMode (..)
                                    , ParseResult (ParseOk, ParseFailed)
                                    )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (UnknownExtension)
                                       , classifyExtension
                                       )
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T (unpack, lines, pack, unlines)
import qualified Data.Text.IO as T (readFile, putStr)
import Control.Monad (unless)

import Tags (Tag, createTags, tagToString)


data ParseOptions
    = ParseOptions
    { ignoreParseError     :: Bool
    , predefinedExtensions :: [Extension]
    } deriving (Show, Eq)


main :: IO ()
main = do
    rawArgs <- getArgs

    let (options, files) = getOptions rawArgs

        parseOpts
          = ParseOptions
          { ignoreParseError = "--ignore-parse-error" `elem` options

          , predefinedExtensions = [ classifyExtension (drop 2 opt)
                                   | opt <- options
                                   , take 2 opt == "-X"
                                   ]
          }

    case files of
        [] -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " [options] [--] <file>"
        filename:_ -> do
            let unknownExts =
                    foldr (\case UnknownExtension x -> (x :) ; _ -> id) []
                        $ predefinedExtensions parseOpts

            unless (null unknownExts) $
                error $ "Unknown extensions: " ++ intercalate ", " unknownExts

            processFile filename parseOpts >>= printTags

printTags :: [Tag] -> IO ()
printTags tags =
    T.putStr $ T.unlines $ map (T.pack . tagToString) tags

processFile :: FilePath -> ParseOptions -> IO [Tag]
processFile file opts = do
    (fileContents, fileLines) <- loadFile file
    case parseFileContentsWithMode parseMode fileContents of
        ParseFailed loc message ->
            if ignoreParseError opts then
                return []
            else
                -- TODO Better error reporting
                fail $ "Parse error: " ++ show loc ++ ": " ++ message
        ParseOk parsedModule -> return $ createTags (parsedModule, fileLines)
    where
        parseMode = ParseMode
            { parseFilename = file
            , baseLanguage = Haskell2010
            , extensions = predefinedExtensions opts
            , ignoreLanguagePragmas = False
            , ignoreLinePragmas = True
            , fixities = Nothing
            , ignoreFunctionArity = False
            }

loadFile :: FilePath -> IO (String, Vector String)
loadFile file = do
    text <- T.readFile file
    let fullContents = T.unpack text
    let textLines = T.lines text
    let stringLines = map T.unpack textLines
    let fileLines = fromList stringLines
    return (fullContents, fileLines)

getOptions :: [String] -> ([String], [String])
getOptions args =
    let (start, end) = span (/= "--") args
        (options, nonOptions) = partition (isPrefixOf "-") start
    in (options, nonOptions ++ dropSep end)
    where
        dropSep ("--":xs) = xs
        dropSep xs        = xs
