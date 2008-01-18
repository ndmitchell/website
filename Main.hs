
module Main(main) where

import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import Text.HTML.TagSoup
import Website.Driver


main = do
    copy "downloads/*.bib;*.pdf" "downloads/"
    copy "elements/" "elements/"

    pages <- getDirWildcards "pages/*.html"
    
    -- first copy the associated image files
    flip mapM_ pages $ \x -> let x2 = takeBaseName x in
        copy ("pages/" ++ x2 ++ "*.png") (x2 ++ "/")

    -- now process the actual files    
    let outloc x | takeBaseName x == "index" = "index.html"
                 | otherwise = takeBaseName x </> "index.html"
    prefix <- readFile "elements/prefix.txt"
    suffix <- readFile "elements/suffix.txt"
    process (rewrite prefix suffix) [(p, outloc p) | p <- pages]

ndm = "http://www-users.cs.york.ac.uk/~ndm/"


rewrite :: String -> String -> Config -> String -> IO String
rewrite prefix suffix c s = putChar '.' >> return
        (renderTags $ page c2 $ parseTagsOptions popts $ prefix ++ s ++ suffix)
    where
        c2 = c += ("root", if takeBaseName (c !+ "file") == "index" then "" else "../")

        popts = parseOptions{optLookupEntity = entity}
        entity (':':xs) = [TagText $ c2 !+ xs]
        entity xs = optLookupEntity parseOptions xs


urlTag  c x = (c !+ "root") ++ "tags/#" ++ x

-- is idempotent
srcPage x = if x2 == "index" then "pages/index.html" else "pages" </> x2 <.> "html"
    where x2 = takeBaseName x

urlPage c x = concat [c !+ "root"
                     ,if x2 == "index" then "" else x2 ++ "/"
                     ,if c !? "debug" then "index.html" else ""]
    where x2 = takeBaseName x

titlePage c x = head $ dropWhile null [a !+ "shortname", a !+ "title", takeBaseName x]
    where a = c !> srcPage x




page :: Config -> [Tag] -> [Tag]
page c (x:xs) = tag c x ++ page c xs
page c [] = []


tag :: Config -> Tag -> [Tag]
tag c (TagOpen ('?':name) []) = [TagText $ c !+ name]
tag c (TagOpen (':':name) atts) = parseTags $ custom c name (map (uncurry (++)) atts)
tag c (TagOpen ('!':name) atts) | name /= "DOCTYPE" = parseTags $
        if ("more","") `elem` atts 
        then "<span class='more'>(<a href='" ++ url ++ "' class='more'>read&nbsp;more</a>)</span>"
        else "<a href='" ++ url ++ "'>" ++ text ++ "</a>"
    where
        tag   = ":" `isPrefixOf` name
        title = if tag then tail name else titlePage c name
        url   = if tag then urlTag c (tail name) else urlPage c name
        text  = if null atts then title else uncurry (++) (head atts)
        

tag c x = [x]



custom :: Config -> String -> [String] -> String


custom c "catch" _ | not $ c !? "catch" = ""
                   | otherwise =
    "<a href='" ++ ndm ++ "catch/'>" ++
        "<img style='border:0;' src='" ++ (c !+ "root") ++ "elements/valid-catch.png' " ++
              "alt='Checked by Catch!' height='31' width='88' />" ++
    "</a>"


custom c "tags" _ = unwords $ map f $ words $ c !+ "tags"
    where f x = "<a href='" ++ urlTag c x ++ "'>" ++ x ++ "</a>"


custom c "email" [a] = "<span class='es_address'>" ++ concatMap f a ++ "</span>"
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]


custom c "menu" _ = "<ul id='menu'>" ++ concatMap f links ++ "</ul>"
    where
        -- (title, page, gap)
        links = [("","index",False)] ++ pick "admin" ++ gap (pick "popular") ++ [("All pages...","tags",False)]
        gap ((a,b,_):xs) = (a,b,True):xs
        getName page def = if null def then titlePage c page else def

        pick tag = sort [(getName page "",page,False) | x <- configAttribs c, tag `elem` words (x !+ "tags")
                   ,let page = takeBaseName (x !+ "file"), page /= "index"]

        f (title,page,gap) = "<li" ++ (if gap then " style='margin-top:10px'" else "") ++
                             "><a href='" ++ urlPage c page ++ "'>" ++ getName page title ++ "</a></li>"


custom c "downloads" _
        | null res = ""
        | otherwise = "<h2>Downloads</h2><ul>" ++
                      (concatMap (showDownload "") $ concat $ groupDownloads res) ++
                      "</ul>"
    where res = getDownloads c


custom _ name _ = error $ "Custom tag not known, " ++ name



---------------------------------------------------------------------
-- DOWNLOADS

type URL = String
data Download = Paper URL String String
              | Release URL
              | Manual URL String
              | Draft URL String String
              | Slides URL String String
              | Darcs URL
              | Haddock URL
              | Blog URL
              deriving Show

downloadType :: Download -> String
downloadType = map toLower . head . words . show

download =
    [f3 "paper" Paper
    ,f1 "release" Release
    ,f2 "manual" Manual
    ,f3 "draft" Draft
    ,f3 "slides" Slides
    ,f1 "darcs" Darcs
    ,f1 "haddock" Haddock
    ,f1 "blog" Blog
    ]
    where
        f1 s y = (s, \[a]     -> y a    )
        f2 s y = (s, \[a,b]   -> y a b  )
        f3 s y = (s, \[a,b,c] -> y a b c)


getDownloads :: FindAttribs a => a -> [Download]
getDownloads a = concatMap f download
    where f (name,op) = reverse $ map (op . split '|') (a !* name)

-- in order, so Paper is first and Blog is last
groupDownloads :: [Download] -> [[Download]]
groupDownloads xs = filter (not . null) $ map f download
    where f (typ,_) = [x | x <- xs, downloadType x == typ]


showDownload :: String -> Download -> String
showDownload extra d = "<li class='" ++ downloadType d ++ "'>" ++ f d ++ [' '|not $ null extra] ++ extra ++ "</li>"
    where
        f (Darcs url) = "<a href='http://darcs.net/'>darcs</a> get --partial <a href='" ++ url ++ "'>" ++ url ++ "</a>"
        f (Paper url text msg) = link (down "paper" url) text msg
        f (Release url) = link url "Released version" ""
        f (Manual url text) = link url text ""
        f (Draft url text msg) = link (down "draft" url) text msg
        f (Slides url text msg) = link (down "slides" url) text msg
        f (Haddock url) = link url "Haddock documentation" ""
        f (Blog url) = link url "Related Blog posts" ""

        down typ url = "../downloads/" ++ typ ++ "-" ++ url
        link url text msg = "<a href='" ++ url ++ "'>" ++ text ++ "</a>" ++
                            if null msg then "" else " - " ++ msg
