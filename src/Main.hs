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
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T (unpack, lines)
import qualified Data.Text.IO as T (readFile)

main :: IO ()
main = do
    rawArgs <- getArgs
    let files = dropOptions rawArgs
    case files of
        [] -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " [--] <file>"
        filename:_ -> processFile filename

processFile :: FilePath -> IO ()
processFile file = do
    (fileContents, fileLines) <- loadFile file
    -- Testing code:
    putStrLn fileContents
    putStrLn (show fileLines)

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
