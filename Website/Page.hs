
module Website.Page(Page) where

import Data.Maybe
import System.FilePath
import Website.Download
import Website.Driver


data Page = Page
    {name :: String           -- ^ The id of the page
    ,url :: URL               -- ^ The URL from the root
    ,root :: String           -- ^ How to get to the root
    ,title :: String          -- ^ The title of the page
    ,fulltitle :: String      -- ^ The full title of the page
    ,tags :: [String]         -- ^ The tags for a page
    ,downloads :: [Download]  -- ^ Associated downloads
    }


readPage :: Bool -> FilePath -> IO Page
readPage debug file = do
    let base = takeBaseName file
    res <- readMetadataHead file
    let fulltitle = res !# "title"
    return Page{name = base
               ,url = (if base == "index" then "" else base ++ "/") ++
                      (if debug then "index.html" else "")
               ,root = if base == "index" then "" else "../"
               ,title = fromMaybe fulltitle $ lookup "shortname" res
               ,fulltitle = fulltitle
               ,tags = words (res !# "tags")
               ,downloads = []}


addDownloads :: [Page] -> [Download] -> [Page]
addDownloads ps ds
        | not $ null evil = error "Some downloads have incorrect pages"
        | otherwise = map f ps
    where
        pages = map name ps
        evil = filter (`notElem` pages) $ map dlPage ds

        f page = page{downloads = filter ((==) (name page) . dlPage) ds}
