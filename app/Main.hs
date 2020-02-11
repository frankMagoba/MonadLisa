{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib

import Data.List          ( group, reverse, sort, sortOn )
import Data.Text          ( toUpper, unlines, words )
import Prelude            hiding ( unlines, words )
import System.Environment ( getArgs )

main :: IO ()
main = do
    paths <- getArgs
    bookWords <- (words . unlines) <$> mapM epubText paths
    let sortedWords = sort $ toUpper <$> bookWords
    let grouppedWords = group sortedWords
    let sortedWordGroups =
            reverse $
            map (\g -> (head g, length g)) $ sortOn length $ grouppedWords
    mapM_ print $ take 250 sortedWordGroups
