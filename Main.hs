
module Main(main) where

import System.FilePath
import Website.Driver


main = do
    pages <- getDirWildcards "pages/*.html"
    let outloc x | takeBaseName x == "index" = x
                 | otherwise = takeBaseName x </> "index.html"
    process rewrite [(p, outloc p) | p <- pages]




rewrite :: Config -> String -> IO String
rewrite c s = return s


{-


    createDir "publish"
    pages <- liftM (map dropExtension . filter (\x -> takeExtension x == ".html")) $
                   getDirectoryContents "pages"
    
    args <- getArgs
    bags <- mapM queryPage pages
    let info = Info (map readEq args) (zip pages bags)
    
    copyElements
    copyDownloads
    copyFileBinary "Main.hs" "publish/Main.hs"
    mapM_ (processPage info) pages

    when (getRelease info) $ system "deploy.bat" >> return ()






import System
import Directory
import System.Directory
import System.IO
import System.FilePath
import Monad
import Maybe
import List
import Debug.Trace


loadHtml :: Name -> IO String
loadHtml s = readFile $ "pages" </> s <.> "html"


split :: Char -> String -> [String]
split x xs = case break (== x) xs of
                (a,_:b) -> a : split x b
                _ -> [xs]

readEq :: String -> (String,String)
readEq s = let (a,'=':b) = break (== '=') s in (a,b)


createDir x = do
    b <- doesDirectoryExist x
    when (not b) $ createDirectory x

lookupJust name bag = case lookup name bag of
                          Nothing -> error $ "Key not found, " ++ show name
                          Just x -> x

lookupDef def name bag = case lookup name bag of
                          Nothing -> def
                          Just x -> x

on f g x y = f (g x) (g y)


---------------------------------------------------------------------
-- MAIN DRIVER

main = do
    createDir "publish"
    pages <- liftM (map dropExtension . filter (\x -> takeExtension x == ".html")) $
                   getDirectoryContents "pages"
    
    args <- getArgs
    bags <- mapM queryPage pages
    let info = Info (map readEq args) (zip pages bags)
    
    copyElements
    copyDownloads
    copyFileBinary "Main.hs" "publish/Main.hs"
    mapM_ (processPage info) pages

    when (getRelease info) $ system "deploy.bat" >> return ()

copyDownloads = do
    createDir "publish/downloads"
    items <- getDirectoryContents "downloads"
    items <- return $ filter (flip elem [".bib",".pdf"] . takeExtension) items
    mapM_ (\x -> copyFileBinary ("downloads" </> x) ("publish/downloads" </> x)) items


copyElements = do
    createDir "publish/elements"
    items <- getDirectoryContents "elements"
    items <- return $ filter (not . isPrefixOf ".") items
    mapM_ (\x -> copyFileBinary ("elements" </> x) ("publish/elements" </> x)) items


demand True msg res = res
demand _    msg res = trace msg res


urlTag :: Info -> String -> String
urlTag info x = demand (x `elem` tags) ("Missing tag: " ++ x) res
    where
        res = get "root" info ++ "tags" ++ "/" ++ (if getDebug info then "index.html" else "") ++ "#" ++ x
        tags = concatMap (words . lookupJust "tags" . snd) (infoRest info)
        

urlPage :: Info -> Name -> String
urlPage info x = demand (x `elem` map fst (infoRest info)) ("Missing page: " ++ x) res
    where
        res = get "root" info ++
              (if x == "index" then "" else x ++ "/") ++
              (if getDebug info then "index.html" else "")



---------------------------------------------------------------------
-- PAGE STUFF


queryPage :: String -> IO [(String,String)]
queryPage s = do
    src <- loadHtml s
    let items = map readEq $ takeWhile (not . null) $ lines src
        root = if s == "index" then "" else "../"
        shortname = fromMaybe (lookupJust "title" items) (lookup "shortname" items)

    when (isNothing $ lookup "tags" items) $ error $ "Missing tags in " ++ s

    return $ ("root",root) : ("shortname",shortname) : items



processPage :: Info -> Name -> IO ()
processPage info@(Info base rest) name = do
    putStrLn $ "Processing " ++ name
    src <- liftM (unlines . dropWhile (not . null) . lines) $ loadHtml name
    out <- if name == "index" then return "publish/index.html"
           else createDir ("publish" </> name) >> return ("publish" </> name </> "index.html")
    pre <- readFile "elements/prefix.txt"
    suf <- readFile "elements/suffix.txt"
    
    extraFiles <- liftM (filter (name `isPrefixOf`)) $ getDirectoryContents "pages"
    mapM_ (\x -> do b <- doesFileExist ("pages" </> x)
                    when (b && name /= "index") $ copyFileBinary ("pages" </> x) ("publish" </> name </> x)
          ) extraFiles
    
    let info2 = addInfoBase info (("name",name) : lookupJust name rest)
    
    menu      <- return $ calcMenu info2
    taglist   <- calcTagList info2
    downloadlist <- return $ calcDownloadList info2
    downloads <- return $ calcDownloads info2
    tags      <- return $ calcTags info2
    allpages  <- return $ calcAllPages info2
    catched   <- return $ calcCatch info2
    
    let info3 = addInfoBase info2 [("taglist",taglist),("downloadlist",downloadlist),("catch",catched),
                                   ("downloads",downloads),("menu",menu),("tags",tags),("allpages",allpages)]
        res = processString info3 (pre ++ src ++ suf)
    
    writeFile out res


-- replace [[=value]] with the value from the bag
-- replace [[#tag]] with a link to the tag
-- replace [[item]] with a link to that page
-- replace [[>item]] with a morei link
processString :: Info -> String -> String
processString bag xs | "<pre>" `isPrefixOf` xs = f xs
    where
        f xs | "</pre>" `isPrefixOf` xs = take 6 xs ++ processString bag (drop 6 xs)
        f (x:xs) = x : f xs
        f [] = []
    
processString bag ('[':'[':xs) = processItem bag a ++ processString bag b
    where (a, ']':']':b) = break (== ']') xs
processString bag (x:xs) = x : processString bag xs
processString bag [] = []


processItem :: Info -> String -> String
processItem bag ('=':xs) = get xs bag
processItem bag ('>':xs) = morei (getUrl bag xs)
    where morei x = "<span class=\"more\">(<a href=\"" ++ x ++ "\" class=\"more\">read&nbsp;more</a>)</span>"
processItem bag xs = case break (== '|') xs of
    (a,_:b) -> "<a href=\"" ++ getUrl bag a ++ "\">" ++ b ++ "</a>"
    _ -> "<a href=\"" ++ getUrl bag xs ++ "\">" ++ name ++ "</a>"
        where name = lookupDef xs "shortname" $ lookupDef [] xs $ infoRest bag

getUrl bag ('#':xs) = urlTag bag xs
getUrl bag xs = urlPage bag xs


---------------------------------------------------------------------
-- CALCULATORS FOR PAGE ELEMENTS


calcCatch :: Info -> String
calcCatch i@(Info bag xs) | "catch" `elem` map fst bag =
    "<a href=\"http://www-users.cs.york.ac.uk/~ndm/catch/\">" ++
        "<img style=\"border:0;\" src=\"" ++ get "root" i ++ "elements/valid-catch.png\" alt=\"Checked by Catch!\" height=\"31\" width=\"88\" />" ++
    "</a>"
calcCatch _ = ""


calcTags :: Info -> String
calcTags info = unwords $ map f $ words $ get "tags" info
    where f x = "<a href=\"" ++ urlTag info x ++ "\">" ++ x ++ "</a>"


calcMenu :: Info -> String
calcMenu info =
        "<ul id=\"menu\">" ++
            concatMap (g False) admin ++
            g True (head rest) ++ concatMap (g False) (tail rest) ++
            g False ("tags",("shortname","All pages...") : lookupJust "tags" admin) ++
        "</ul>"
    where
        (admin,rest) = span ((<= 1) . fst . f) items
        items = filter ((/= -1) . fst . f) $ sortBy (compare `on` f) $ infoRest info
    
        f ("index",x) = (0,"")
        f (x,ys) = (if "admin" `elem` tags then 1 else if "popular" `elem` tags then 2 else -1
                   ,lookupJust "shortname" ys)
            where tags = words (lookupJust "tags" ys)

        g space (x,ys) = "<li" ++ (if space then " style=\"margin-top:10px;\"" else "") ++
                         "><a href=\"" ++ urlPage info x ++ "\">" ++
                         lookupJust "shortname" ys ++ "</a></li>"


calcAllPages :: Info -> String
calcAllPages info = "<p>" ++
                    concat (intersperse ", " $ map snd $ sortBy (compare `on` fst) $ map f $ infoRest info) ++
                    "</p>"
    where
        f (name,tags) = (short, "<a href=\"" ++ urlPage info name ++ "\">" ++ short ++ "</a>")
            where short = lookupJust "shortname" tags



calcTagList :: Info -> IO String
calcTagList info = do
        desc <- readFile "tags.txt"
        return $ concatMap (f desc) tags
    where
        tags = sort $ nub $ concatMap snd mp
        mp = [(a, words $ lookupJust "tags" b) | (a,b) <- infoRest info]
        
        f desc tag = demand (not $ null d) ("Missing tag description: " ++ tag) res
            where
                res = "<p><a name=\"" ++ tag ++ "\"></a><b>" ++
                      tag ++ "</b>" ++ (if null d then "" else ": " ++ d) ++ "<br/>" ++
                      concat (intersperse ", " $ map snd $ sortBy (compare `on` fst) $ map g progs) ++ "</p>"
            
                progs = sort [a | (a,b) <- mp, tag `elem` b]
                d = concat [drop (length tag+1) y | y <- lines desc, (tag ++ "=") `isPrefixOf` y]

        g x = (short, "<a href=\"" ++ urlPage info x ++ "\">" ++ short ++ "</a>")
            where short = lookupJust "shortname" (lookupJust x (infoRest info))


showDarcs :: String -> String -> String
showDarcs extra x = "<li class=\"darcs\"><a href=\"http://darcs.net/\">darcs</a> get --partial <a href=\"" ++
                    x ++ "\">" ++ x++ "</a>" ++ extra ++ "</li>"

showHaddock :: String -> String -> String
showHaddock extra x = "<li class=\"haddock\"><a href=\"" ++ x ++ "\">Haddock documentation</a>" ++ extra ++ "</li>"

showRelease :: String -> String -> String
showRelease extra x = "<li class=\"release\"><a href=\"" ++ url ++ "\">Released version " ++ version ++ "</a>" ++ extra ++ "</li>"
    where
        [url,version] = split '|' x

showBlog :: String -> String -> String
showBlog extra x = "<li class=\"blog\"><a href=\"http://neilmitchell.blogspot.com/search/label/" ++
                    x ++ "\">Related Blog posts</a>" ++ extra ++ "</li>"

showManual :: String -> String -> String
showManual extra x = "<li class=\"manual\"><a href=\"" ++ url ++ "\">" ++ name ++ "</a>" ++ extra ++ "</li>"
    where
        [url,name] = split '|' x


showDownload :: Info -> String -> (String,String) -> ((Int,Int,Int), String)
showDownload info extra (typ,value) =
        (date, "<li class=\"" ++ typ ++ "\"><a href=\"../downloads/" ++ typ ++ "-" ++ url ++ "\">" ++
               name ++ "</a>" ++ rest ++ extra ++ "</li>")
    where
        fields = split '|' value
        url = fields !! 0
        name = fields !! 1
        rest = if length fields > 2 then " - " ++ fields !! 2 else ""
        date = parseDate (dropExtension (split '-' url !! 1))

        parseDate :: String -> (Int,Int,Int)
        parseDate x = case split '_' x of
            [day,mon,year] -> (read year,fromJust $ findIndex (== mon) months, read day)
            _ -> (maxBound, maxBound, maxBound)

        months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]



calcDownloads :: Info -> String
calcDownloads info = if null items then ""
                     else "<h2>Downloads</h2><ul>" ++ concat items ++ "</ul>"
    where
        items = darcs ++ manuals ++ releases ++ haddocks ++ blogs ++ rest

        darcs = [showDarcs "" val | ("darcs",val) <- infoBase info]
        haddocks = [showHaddock "" val | ("haddock",val) <- infoBase info]
        releases = [showRelease "" val | ("tarball",val) <- infoBase info]
        blogs = [showBlog "" (lookupJust "name" $ infoBase info) | ("blog",_) <- infoBase info]
        manuals = [showManual " - a manual" val | ("manual",val) <- infoBase info]

        rest = map snd $ reverse $ sortBy (compare `on` fst) xs
            where xs = f ["slides","draft","paper"]

        f xs = [showDownload info "" (a,b) | (a,b) <- infoBase info, a `elem` xs]


calcDownloadList :: Info -> String
calcDownloadList info = f "paper" ++ releases ++ manuals ++ f "draft" ++ f "slides" ++ darcs ++ haddocks ++ blogs
    where
        extra name = " (from <a href=\"" ++ urlPage info name ++ "\">" ++
                     lookupJust "shortname" (lookupJust name $ infoRest info) ++ "</a>)"

        nicenames = [("paper","Papers"),("draft","Draft Papers"),("slides","Slides from Presentations")]

        darcs = g False "Darcs repositories" 
                [(lookupJust "shortname" vals, showDarcs (extra name) repo)
                | (name,vals) <- infoRest info, ("darcs",repo) <- vals]

        releases = g False "Releases"
                [(lookupJust "shortname" vals, showRelease (extra name) repo)
                | (name,vals) <- infoRest info, ("tarball",repo) <- vals]

        haddocks = g False "Haddock documentation" 
                [(lookupJust "shortname" vals, showHaddock (extra name) repo)
                | (name,vals) <- infoRest info, ("haddock",repo) <- vals]

        blogs = g False "Blog Postings"
                [(lookupJust "shortname" vals, showBlog (extra name) name)
                | (name,vals) <- infoRest info, ("blog",_) <- vals]

        manuals = g False "Manuals"
                  [(lookupJust "shortname" vals, showManual (extra name) x)
                  | (name,vals) <- infoRest info, ("manual",x) <- vals]

        f want = g True (lookupJust want nicenames)
                 [showDownload info (extra name) (tag,val)
                 | (name,vals) <- infoRest info, (tag,val) <- vals, tag == want]

        g rev desc [] = []
        g rev desc items = "<h2>" ++ desc ++ "</h2>" ++ extra ++ "<ul>" ++ concat xs ++ "</ul>"
            where
                xs = map snd $ (if rev then reverse else id) $ sortBy (compare `on` fst) items
                extra = if desc /= "Papers" then "" else
                        "<p>There is a <a href=\"neil_mitchell.bib\">BibTeX list</a> of all these papers.</p>"



copyFileBinary :: FilePath -> FilePath -> IO ()
copyFileBinary fromFPath toFPath =
    do readFileBinary fromFPath >>= writeFileBinary toFPath
       return ()

readFileBinary :: FilePath -> IO String
readFileBinary file = do
    h <- openBinaryFile file ReadMode
    hGetContents h

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file str = do
    h <- openBinaryFile file WriteMode
    hPutStr h str
    hClose h
-}
