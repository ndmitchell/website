
module Website.Page(Page(..), readPage, linkPages) where

import Data.Maybe
import System.FilePath
import Website.Download
import Website.Driver


data Page = Page
    {pgName :: String           -- ^ The id of the page
    ,pgUrl :: URL               -- ^ The URL from the root
    ,pgRoot :: String           -- ^ How to get to the root
    ,pgTitle :: String          -- ^ The title of the page
    ,pgFulltitle :: String      -- ^ The full title of the page
    ,pgTags :: [String]         -- ^ The tags for a page
    ,pgDownloads :: [Download]  -- ^ Associated downloads
    ,pgAll :: [(String,Page)]   -- ^ Links to all pages
    ,pgAttribs :: [(String,String)] -- ^ Raw attributes
    }


readPage :: Bool -> FilePath -> IO Page
readPage debug file = do
    let base = takeBaseName file
    res <- readMetadataHead file
    let fulltitle = res !# "title"
    return Page{pgName = base
               ,pgUrl = (if base == "index" then "" else base ++ "/") ++
                        (if debug then "index.html" else "")
               ,pgRoot = if base == "index" then "" else "../"
               ,pgTitle = fromMaybe fulltitle $ lookup "shortname" res
               ,pgFulltitle = fulltitle
               ,pgTags = words (res !# "tags")
               ,pgDownloads = []
               ,pgAll = []
               ,pgAttribs = res
               }


linkPages :: [Page] -> [Download] -> [(String,Page)]
linkPages ps ds
        | not $ null evil = error $ "Some downloads have incorrect pages: " ++ show evil
        | otherwise = res
    where
        res = map f ps

        pages = map pgName ps
        evil = filter (`notElem` pages) $ map dlPage ds

        f page = (pgName page, page{pgAll = res, pgDownloads = filter ((==) (pgName page) . dlPage) ds})
