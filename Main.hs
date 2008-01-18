
module Main(main) where

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
tag c (TagOpen (':':name) atts) = custom c name (map (uncurry (++)) atts)
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



custom :: Config -> String -> [String] -> [Tag]
custom c "catch" _ | not $ c !? "catch" = []
                   | otherwise = parseTags $
    "<a href='" ++ ndm ++ "catch/'>" ++
        "<img style='border:0;' src='" ++ (c !+ "root") ++ "elements/valid-catch.png' " ++
              "alt='Checked by Catch!' height='31' width='88' />" ++
    "</a>"

custom c "tags" _ = parseTags $ unwords $ map f $ words $ c !+ "tags"
    where f x = "<a href='" ++ urlTag c x ++ "'>" ++ x ++ "</a>"

custom c "email" atts =
    [TagOpen "span" [("class","es_address")]
    ,TagText $ concatMap f $ head atts
    ,TagClose "span"]
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]

custom c "menu" _ = parseTags $ "<ul id='menu'>" ++ concatMap f links ++ "</ul>"
    where
        -- (title, page, gap)
        links = [("","index",False)] ++ pick "admin" ++ gap (pick "popular") ++ [("All pages...","tags",False)]
        gap ((a,b,_):xs) = (a,b,True):xs
        getName page def = if null def then titlePage c page else def

        pick tag = sort [(getName page "",page,False) | x <- configAttribs c, tag `elem` words (x !+ "tags")
                   ,let page = takeBaseName (x !+ "file"), page /= "index"]

        f (title,page,gap) = "<li" ++ (if gap then " style='margin-top:10px'" else "") ++
                             "><a href='" ++ urlPage c page ++ "'>" ++ getName page title ++ "</a></li>"

custom _ name _ = error $ "Custom tag not known, " ++ name

