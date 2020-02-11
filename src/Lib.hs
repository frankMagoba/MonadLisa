{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( epubText
    ) where

import           Codec.Archive.Zip
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import           Data.Conduit.Combinators  ( sinkLazy )
import qualified Data.List                 as List
import qualified Data.Map                  as M
import           Data.Text
    ( Text
    , pack
    , toUpper
    , unlines
    , unpack
    , unwords
    , words
    )
import           Prelude                   hiding ( unlines, unwords, words )
import           System.FilePath           ( normalise, takeDirectory, (</>) )
import           Text.Regex                ( mkRegex, subRegex )
import qualified Text.XML                  as XML
import           Text.XML.Cursor           ( ($//), ($|), (&|) )
import qualified Text.XML.Cursor           as XML

data FileCursor =
    FileCursor
        { filePath :: FilePath
        , content  :: BL.ByteString
        , cursor   :: XML.Cursor
        }

fileCursor :: FilePath -> ZipArchive FileCursor
fileCursor filepath = do
    selector <- mkEntrySelector $ normalise filepath
    string   <- sourceEntry selector sinkLazy
    let doc = XML.parseLBS_ XML.def string
    let cur = XML.fromDocument doc
    return $ FileCursor filepath string cur

epubContainer :: ZipArchive FileCursor
epubContainer = fileCursor "META-INF/container.xml"

epubOpfs :: ZipArchive [FileCursor]
epubOpfs = do
    (FileCursor _ _ cur) <- epubContainer
    let paths :: [FilePath]
        paths = unpack <$> concat
            (cur $// XML.laxElement "rootfile" &| XML.attribute "full-path")
    mapM fileCursor paths

opfChapters :: FileCursor -> ZipArchive [FileCursor]
opfChapters (FileCursor opfPath _ cur) = mapM fileCursor paths
  where
    paths =
        cur
            $// XML.attributeIs "media-type" "application/xhtml+xml"
            &|  \elem ->
                    let href = unwords $ elem $| XML.attribute "href"
                    in  takeDirectory opfPath </> unpack href

epubChapters :: ZipArchive [FileCursor]
epubChapters = concat <$> (epubOpfs >>= mapM opfChapters)

chapterText :: FileCursor -> Text
chapterText (FileCursor _ content _) =
    pack $ subRegex (mkRegex "(<[^>]*>|[^a-zA-Z ])") (BL.toString content) ""

epubText :: FilePath -> IO Text
epubText path = do
    chapters <- withArchive path epubChapters
    return $ unlines $ chapterText <$> chapters
