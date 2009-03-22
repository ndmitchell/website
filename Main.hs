
module Main(main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Safe
import System.Cmd
import System.Exit
import System.FilePath
import System.Environment
import System.IO.Unsafe
import Text.HTML.TagSoup
import Website.Check
import Website.Driver
import Website.Page
import Website.Download


root = "http://community.haskell.org/~ndm/"


known = ["push","build","debug","check"]

main :: IO ()
main = do
    args <- getArgs
    let bad = filter (`notElem` known) args
    when (bad /= []) $
        error $ "Unknown arguments: " ++ unwords bad

    when (null args) $ putStr $ unlines
        ["No command give, expected one of:"
        ," * build - build the website in release mode"
        ," * debug - build the website in debug mode"
        ," * push  - upload the website (do after build)"
        ," * check - run HTML validation (do after push)"
        ]
    let iff x y = when (x `elem` args) (y >> return ())
    
    iff "build" $ generate False
    iff "debug" $ generate True

    iff "push" $ do
        let system_ x = do putStrLn x ; res <- system x; when (res /= ExitSuccess) (error "System command failed")
        system_ "tar --gzip -cf public_html.tar.gz public_html"
        system_ "scp public_html.tar.gz ndm@community.haskell.org:/home/ndm/public_html.tar.gz"
        system_ "ssh ndm@community.haskell.org tar -xf public_html.tar.gz"

    iff "check" $ do
        files <- getDirWildcards "pages/*.html"
        let urls = [root ++ x | x <- "" : map takeBaseName files, x /= "index"]
        check urls


generate :: Bool -> IO ()
generate debug = do
    copy "elements/" "elements/"

    files <- getDirWildcards "pages/*.html"
    
    -- first copy the associated image files
    flip mapM_ files $ \x -> let x2 = takeBaseName x in do
        copy ("pages/" ++ x2 ++ "*.png") (x2 ++ "/")
        copy ("pages/" ++ x2 ++ "*.htm") (x2 ++ "/")

    -- build up the meta data
    prefix <- readFile "elements/prefix.txt"
    suffix <- readFile "elements/suffix.txt"
    pages <- populatePages debug files

    -- output some bibtex
    let dls = concatMap (pgDownloads . snd) pages
    newFile "downloads/neil_mitchell.bib" (showBibtexPapers dls)
    newFile "downloads/neil_mitchell_slides.bib" (showBibtexSlides dls)
    
    -- process the files
    let outloc x | takeBaseName x == "index" = "index.html"
                 | otherwise = takeBaseName x </> "index.html"
    putStr "Processing files  "
    process (rewrite pages prefix suffix) [(p, outloc p) | p <- files]
    putStrLn ""


populatePages :: Bool -> [FilePath] -> IO [(String,Page)]
populatePages debug pages = do
    putStr "Reading meta data "
    downloads <- populateDownloads
    pages <- mapM (\x -> putChar '.' >> readPage debug x) pages
    putStrLn ""
    return $ linkPages pages downloads
    where
        f (y,x) | y `elem` ["url","parent"] = (y, url x)
        f x = x


populateDownloads :: IO [Download]
populateDownloads = do
    let dirs = ["","paper","slides","draft"]
    mapM_ (\x -> copy ("downloads" </> x </> "*.pdf") "downloads/") dirs
    
    let files = ["downloads" </> x </> "metadata.txt" | x <- dirs]
    liftM (map (readDownload . map f) . concat) $ mapM readMetadataFile files
    where
        f (y,x) | y `elem` ["url","parent"] = (y, url x)
        f x = x


url x | "http:" `isPrefixOf` x = x
      | "hackage:" `isPrefixOf` x = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ drop 8 x
      | "darcs:" `isPrefixOf` x = "http://community.haskell.org/~ndm/darcs/" ++ drop 6 x
      | "blog:" `isPrefixOf` x = "http://neilmitchell.blogspot.com/search/label/" ++ drop 5 x
      | "haddock:" `isPrefixOf` x = "http://www.cs.york.ac.uk/fp/haddock/" ++ drop 8 x
      | "bug:" `isPrefixOf` x = "http://code.google.com/p/ndmitchell/issues/list?q=proj:" ++ drop 4 x
      | otherwise = root ++ "downloads/" ++ x



rewrite :: [(String,Page)] -> String -> String -> FilePath -> IO String
rewrite pages prefix suffix file = do
    putChar '.'
    src <- dropMetadataHead file
    let meta = pages !# takeBaseName file
    return $ stream meta $ prefix ++ src ++ suffix


---------------------------------------------------------------------
-- META OPERATIONS

urlTag meta x = urlPage meta "tags" ++ "#" ++ x
urlPage meta x = pgRoot meta ++ pgUrl (meta !## x)


(!##) :: Page -> String -> Page
(!##) p x = pgAll p !# x

args = map (uncurry (++))
args1 = head . args


---------------------------------------------------------------------
-- REWRITE

stream meta ('<':x:xs) | x `elem` ":!" && not (any (`isPrefixOf` xs) ["DOCTYPE","--","["]) =
        (if x == ':' then tag meta name atts else link meta name atts) ++
        stream meta (drop 1 b)
    where
        (a,b) = break (== '>') xs
        (name,atts) = case parseTags ("<" ++ a ++ ">") of
                           [TagOpen name atts] -> (name,atts)
                           _ -> error $ "Can't parse options tag: " ++ show a

stream meta (x:xs) = x : stream meta xs
stream meta [] = []


link meta name atts
    | "more" `elem` args atts = "<span class='more'>(<a href='" ++ url ++
                                "' class='more'>read&nbsp;more</a>)</span>"
    | otherwise = "<a href='" ++ url ++ "'>" ++ text ++ "</a>"
    where
        tag   = ":" `isPrefixOf` name
        title = if tag then tail name else pgTitle $ meta !## name
        url   = if tag then urlTag meta (tail name) else urlPage meta name
        text  = if null atts then title else args1 atts


tag meta "email" a = "<span class='es_address'>" ++ concatMap f (args1 a) ++ "</span>"
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]


tag meta "root" _ = pgRoot meta


tag meta "title" a = pgFulltitle meta


tag meta "show-tags" _ = unwords $ map f $ sort $ pgTags meta
    where f x = "<a href='" ++ urlTag meta x ++ "'>" ++ x ++ "</a>"


tag meta "show-catch" _ | not $ pgAttribs meta !? "catch" = []
                        | otherwise =
    "<a href='" ++ urlPage meta "catch" ++ "'>" ++
        "<img style='border:0;' src='" ++ pgRoot meta ++ "elements/valid-catch.png' " ++
             "alt='Checked by Catch!' height='31' width='88' /></a>"


tag meta "show-menu" _ = "<ul id='menu'>" ++ concatMap f links ++ "</ul>"
    where
        links = [(pgTitle (meta !## "index"), urlPage meta "index",False)] ++
                pick "admin" ++
                gap (pick "popular") ++
                [("All pages...",urlPage meta "tags",False)]
        gap ((a,b,_):xs) = (a,b,True):xs

        pick tag = sort [ (pgTitle p, urlPage meta name, False)
                        | (name,p) <- pgAll meta, tag `elem` pgTags p, name /= "index"]

        -- (title, page, gap)
        f :: (String, URL, Bool) -> String
        f (title,url,gap) = "<li" ++ (if gap then " style='margin-top:10px'" else "") ++ ">" ++
                             "<a href='" ++ url  ++ "'>" ++ title ++ "</a></li>"


tag meta "all-tags" _ = concatMap f tagList
    where
        tagList = sort [(a,b) | x <- lines $ unsafePerformIO $ readFile "tags.txt", let (a,_:b) = break (== '=') x]

        f (tag,desc) = if null items then "" else
                       "<p><b><a name='" ++ tag ++ "'></a>" ++ tag ++ "</b>: " ++ desc ++ "<br/>" ++
                       concat (intersperse ", " items) ++ "</p>"
            where items = concatMap (g tag) $ pgAll meta
        
        g tag (name,page) = ["<a href='" ++ urlPage meta name ++ "'>" ++ pgTitle page ++ "</a>"
                            | tag `elem` pgTags page]


tag meta "all-pages" _ =
        "<p>" ++ concat (intersperse ", " $ map snd $ sortBy (compare `on` fst) $ map f $ filter g $ pgAll meta) ++ "</p>"
    where
        f (name,page) = (map toLower title, "<a href='" ++ urlPage meta name ++ "'>" ++ title ++ "</a>")
            where title = pgTitle page
        g (name,page) = "hide" `notElem` pgTags page


tag meta "downloads" _ | null down = ""
        | otherwise = "<h2>Downloads</h2>" ++ showDownloadTree down
    where
        down = pgDownloads meta 


tag meta "all-downloads" _ = showDownloadGroup $ concatMap f $ pgAll meta
    where
        f (name,page) = map (\dl -> dl{dlText = dlText dl ++ extra}) $ pgDownloads page
            where extra = " (<a href='" ++ urlPage meta name ++ "'>" ++ pgTitle page ++ "</a>)"


tag meta name atts = error $ "Unrecognised tag: " ++ name
