
module Main(main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Safe
import System.FilePath
import System.Environment
import System.IO.Unsafe
import Text.HTML.TagSoup
import Website.Driver

ndm = "http://www-users.cs.york.ac.uk/~ndm/"

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
    prefix <- readFileTree "elements/prefix.txt"
    suffix <- readFileTree "elements/suffix.txt"
    args <- getArgs
    extra <- return [(x, tagStr x "") | x <- args]
    process (reader extra) (rewrite prefix suffix) [(p, outloc p) | p <- pages]


readFileTree x = return . tagTree . parseTags =<< readFile x

reader :: [(String,TagTree)] -> FilePath -> IO ((FilePath,[TagTree]), [(String,TagTree)])
reader extra x = do
    src <- readFileTree x
    return ((x,src), ("file", tagStr "file" x) : extra ++ concatMap f (universeTags src))
    where
        f t@(TagBranch (':':name) _ _) = [(name,t)]
        f t@(TagLeaf (TagOpen (':':name) _)) = [(name,t)]
        f _ = []


rewrite :: [TagTree] -> [TagTree] -> Config TagTree -> (FilePath, [TagTree]) -> IO String
rewrite prefix suffix c (file,body) = putStrLn (takeBaseName file) >> return
        (renderTags $ flattenTree $ transformTags (tree c2) $ prefix ++ body ++ suffix)
    where
        c2 = c += ("root",tagStr "root" root)
        root = if takeBaseName file == "index" then "" else "../"


tagStr key val = TagLeaf $ TagOpen (':':key) [("",val) | not $ null val]

strTag (TagBranch _ atts _) = headDef "" $ args atts
strTag (TagLeaf (TagOpen _ atts)) = headDef "" $ args atts
strTag _ = ""

args = map (uncurry (++))

c !# x = case c !* x of
            x:xs -> strTag x
            _ -> ""

urlTag  c x = (c !# "root") ++ "tags/#" ++ x

-- is idempotent
srcPage x = if x2 == "index" then "pages/index.html" else "pages" </> x2 <.> "html"
    where x2 = takeBaseName x

urlPage c x = concat [c !# "root"
                     ,if x2 == "index" then "" else x2 ++ "/"
                     ,if c !? "debug" then "index.html" else ""]
    where x2 = takeBaseName x

titlePage c x = head $ dropWhile null [a !# "shortname", a !# "title", takeBaseName x]
    where a = c !> srcPage x

getTags c = case c !* "tags" of
                [TagLeaf (TagOpen _ atts)] -> map fst atts
                _ -> []


reform = map TagLeaf . parseTags
deform = renderTags . flattenTree


(TagBranch (':':name) _ _) =? s = name == s
_ =? _ = False


skip = ["title","shortname","tags","catch"]

tree :: Config TagTree -> TagTree -> [TagTree]
tree c (TagBranch (':':name) atts inner) = reform $ tag c name atts inner
tree c (TagLeaf (TagOpen (':':name) atts)) = reform $ tag c name atts []

tree c (TagLeaf (TagOpen ('!':name) atts)) | name /= "DOCTYPE" = reform $
        if ("more","") `elem` atts 
        then "<span class='more'>(<a href='" ++ url ++ "' class='more'>read&nbsp;more</a>)</span>"
        else "<a href='" ++ url ++ "'>" ++ text ++ "</a>"
    where
        tag   = ":" `isPrefixOf` name
        title = if tag then tail name else titlePage c name
        url   = if tag then urlTag c (tail name) else urlPage c name
        text  = if null atts then title else uncurry (++) (head atts)

tree c (TagBranch name atts inner) = [TagBranch name (map f atts) inner]
    where
        f (key,'[':'R':'O':'O':'T':']':val) = (key, (c !# "root") ++ val)
        f x = x
tree c x = [x]



tag :: Config TagTree -> String -> [Attribute] -> [TagTree] -> String
tag c name _ _ | name `elem` skip = []


tag c "get" atts _ = c !# head (args atts)


tag c "show-tags" _ _ = unwords $ map f $ sort $ getTags c
    where f x = "<a href='" ++ urlTag c x ++ "'>" ++ x ++ "</a>"


tag c "show-catch" _ _ | not $ c !? "catch" = ""
                          | otherwise =
    "<a href='" ++ ndm ++ "catch/'>" ++
        "<img style='border:0;' src='" ++ (c !# "root") ++ "elements/valid-catch.png' " ++
              "alt='Checked by Catch!' height='31' width='88' />" ++
    "</a>"


tag c "email" a _ = "<span class='es_address'>" ++ concatMap f (head (args a)) ++ "</span>"
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]


tag c "show-menu" _ _ = "<ul id='menu'>" ++ concatMap f links ++ "</ul>"
    where
        -- (title, page, gap)
        links = [("","index",False)] ++ pick "admin" ++ gap (pick "popular") ++ [("All pages...","tags",False)]
        gap ((a,b,_):xs) = (a,b,True):xs
        getName page def = if null def then titlePage c page else def

        pick tag = sort [(getName page "",page,False) | x <- configKeys c, tag `elem` getTags (c !> x)
                   ,let page = takeBaseName x, page /= "index"]

        f (title,page,gap) = "<li" ++ (if gap then " style='margin-top:10px'" else "") ++
                             "><a href='" ++ urlPage c page ++ "'>" ++ getName page title ++ "</a></li>"


tag c "all-pages" _ _ =
        "<p>" ++ concat (intersperse ", " $ map snd $ sortBy (compare `on` fst) $ map f $ configKeys c) ++ "</p>"
    where
        f file = (map toLower title, "<a href=\"" ++ urlPage c file ++ "\">" ++ title ++ "</a>")
            where title = titlePage c file


tag c "all-tags" _ _ = concatMap f tagList
    where
        tagList = sort [(a,b) | x <- lines $ unsafePerformIO $ readFile "tags.txt", let (a,_:b) = break (== '=') x]

        f (tag,desc) = if null items then "" else
                       "<p><b><a name='" ++ tag ++ "'>" ++ tag ++ "</b></a>: " ++ desc ++ "<br/>" ++
                       concat (intersperse ", " items)
            where items = concatMap (g tag) $ configKeys c
        
        g tag file = ["<a href='" ++ urlPage c file ++ "'>" ++ titlePage c file ++ "</a>"
                     | tag `elem` getTags (c !> file)]


tag c "downloads" _ inner = "<h3>Downloads</h3><ul>" ++ deform inner ++ "</ul>"
tag c name att inner | name `elem` downloads =
    "<li class='" ++ name ++ "'>" ++ download c name (args att) inner ++ "</li>"

tag c "conf" att inner = "<li class='conference'>From <a href='" ++ url ++ "'>" ++ name ++ "</a>" ++
                         "<ul>" ++ deform inner ++ "</ul>"
    where [name,url] = args att

tag c name _ _ = trace ("WARNING: Unhandled, " ++ name) $ "<b>!" ++ name ++ "!</b>"


downloads = ["manual","release","darcs","blog","slides","draft","paper","haddock","video"]

download c "manual" [att] _ = link (getDarcs c ++ getProject c ++ ".htm") att ""

download c "release" u _ = link url "Released version" ""
    where url = head $ u ++ ["http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ getProject c]

download c "darcs" att _ = "<a href='http://darcs.net/'>darcs</a> get --partial <a href='" ++
                           url ++ "'>" ++ url ++ "</a>"
    where url = nowDarcs c (head $ take 1 att ++ [""])

download c "blog" [] _ = "<a href='" ++ url ++ "'>Related blog posts</a>"
    where url = "http://neilmitchell.blogspot.com/search/label/" ++ getProject c

download c "haddock" u _ = "<a href='" ++ url ++ "'>Haddock documentation</a>"
    where url = head $ u ++ ["http://www.cs.york.ac.uk/fp/haddock/" ++ getProject c ++ "/"]

download c "video" [url,name] _ = link url name ""

download c typ (url:title:z) inner = "<a href='" ++ typ ++ "-" ++ url ++ "'>" ++ title ++ "</a>" ++
    if null z && null inner then "" else " - " ++ concat z ++ deform inner

download c typ att _ = error $ "Missed download: " ++ show (typ,att)

getDarcs c = nowDarcs c (c !# "darcs")
nowDarcs c "" = "http://www.cs.york.ac.uk/fp/darcs/" ++ getProject c ++ "/"
nowDarcs c x = x

getProject c = case c !# "shortname" of
                    [] -> takeBaseName $ c !# "file"
                    xs -> map toLower xs

link url title extra = "<a href='" ++ url ++ "'>" ++ title ++ "</a>" ++
                       if null extra then "" else " - " ++ extra
