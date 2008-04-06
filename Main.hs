
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

    -- build up the meta data
    prefix <- liftM parseTags $ readFile "elements/prefix.txt"
    suffix <- liftM parseTags $ readFile "elements/suffix.txt"
    meta <- populateMeta pages
    
    -- process the files
    let outloc x | takeBaseName x == "index" = "index.html"
                 | otherwise = takeBaseName x </> "index.html"
    putStr "Processing files  "
    process (rewrite meta prefix suffix) [(p, outloc p) | p <- pages]


populateMeta pages = do
    putStr "Reading meta data "
    let files = ["downloads" </> x </> "metadata.txt" | x <- ["","paper","slides"]]
    global <- readMetadataGlobal
    meta <- liftM (map (map f) . concat) $ mapM readMetadataFile files
    pages <- mapM g pages
    putStrLn ""
    return $ Metadata global [] meta pages
    where
        f ("url",x) | not $ "http:" `isPrefixOf` x = ("url", ndm ++ "downloads/" ++ x)
        f x = x

        g file = do
            putChar '.'
            let base = takeBaseName file
            res <- readMetadataHead file
            return $ ("name",base)
                   : ("page",file)
                   : ("root",if base == "index" then "" else "../")
                   : res


rewrite :: Metadata -> [Tag] -> [Tag] -> FilePath -> IO String
rewrite meta prefix suffix file = do
    putChar '.'
    src <- liftM parseTags $ dropMetadataHead file
    meta <- return meta{page = meta !> file}
    return $ renderTags $ stream meta $ prefix ++ src ++ suffix


---------------------------------------------------------------------
-- META OPERATIONS

meta ! x = fromMaybe (error $ "Meta! " ++ x) $ lookup x (page meta)

meta !> x = head [p | p <- pages meta, lookup "name" p == Just x2]
    where x2 = takeBaseName x

elemFst x y = x `elem` map fst y

root meta = meta ! "root"

urlTag meta x = root meta ++ "tags/#" ++ x

urlPage meta x = concat [root meta
                        ,if x2 == "index" then "" else x2 ++ "/"
                        ,if "debug" `elemFst` global meta then "index.html" else ""]
    where x2 = takeBaseName x

titlePage meta x = pick "shortname" $ pick "title" $ takeBaseName x
    where 
        pick key def = fromMaybe def $ lookup key a
        a = meta !> x

args = map (uncurry (++))
args1 = head . args


---------------------------------------------------------------------
-- REWRITE

stream meta (TagOpen (':':name) atts:rest) = tag meta name atts ++ stream meta rest

stream meta (TagOpen ('!':name) atts:rest) | name /= "DOCTYPE" =
        (if "more" `elem` args atts
        then [TagOpen "span" [("class","more")], TagText "("
             ,TagOpen "a" [("href",url),("class","more")], TagText "read&nbsp;more"
             ,TagClose "a", TagText ")", TagClose "span"]
        else [TagOpen "a" [("href",url)], TagText text, TagClose "a"])
        ++ stream meta rest
    where
        tag   = ":" `isPrefixOf` name
        title = if tag then tail name else titlePage meta name
        url   = if tag then urlTag meta (tail name) else urlPage meta name
        text  = if null atts then title else args1 atts

stream meta (TagOpen name atts:rest) = TagOpen name (map f atts) : stream meta rest
    where
        f (x,'[':'R':'O':'O':'T':']':y) = (x, root meta ++ y)
        f x = x

stream meta (x:xs) = x : stream meta xs
stream meta [] = []



tag meta "email" a = [TagOpen "span" [("class","es_address")]
                       ,TagText $ concatMap f (args1 a)
                       ,TagClose "span"]
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]


tag meta "root" _ | lookup "name" (page meta) == Just "index" = []
                  | otherwise = [TagOpen "base" [("href","..")], TagClose "base"]


tag meta "get" a = [TagText $ meta ! args1 a]


tag meta "show-tags" _ = concat $ intersperse [TagText " "] $ map f $ sort $ words $ meta ! "tags"
    where f x = [TagOpen "a" [("href",urlTag meta x)], TagText x, TagClose "a"]


tag meta "show-catch" _ | "catch" `elemFst` page meta = []
                          | otherwise =
    [TagOpen "a" [("href",urlPage meta "catch")]
    ,TagOpen "img" [("style","border:0;")
                   ,("src",root meta ++ "elements/valid-catch.png")
                   ,("alt","Checked by Catch!"),("height","31"),("width","88")]
    ,TagClose "img",TagClose "a"]


tag meta name atts = error $ "Unrecognised tag: " ++ name



{-


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
-}
