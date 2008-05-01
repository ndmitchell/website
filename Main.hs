
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
    prefix <- readFile "elements/prefix.txt"
    suffix <- readFile "elements/suffix.txt"
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
        f (y,x) | y `elem` ["url","parent"] = (y, url x)
        f x = x

        g file = do
            putChar '.'
            let base = takeBaseName file
            res <- readMetadataHead file
            return $ ("name",base)
                   : ("page",file)
                   : ("root",if base == "index" then "" else "../")
                   : [("shortname",res !- "title") | not $ "shortname" `elemFst` res]
                   ++ res


url x | "http:" `isPrefixOf` x = x
      | "hackage:" `isPrefixOf` x = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ drop 8 x
      | "darcs:" `isPrefixOf` x = "http://www.cs.york.ac.uk/fp/darcs/" ++ drop 6 x
      | "blog:" `isPrefixOf` x = "http://neilmitchell.blogspot.com/search/label/" ++ drop 5 x
      | otherwise = ndm ++ "downloads/" ++ x



rewrite :: Metadata -> String -> String -> FilePath -> IO String
rewrite meta prefix suffix file = do
    putChar '.'
    src <- dropMetadataHead file
    meta <- return meta{page = meta !> file, global = ("menu",menu meta) : global meta}
    return $ stream meta $ prefix ++ src ++ suffix


menu meta = "<ul id='menu'>" ++ concatMap f links ++ "</ul>"
    where
        -- (title, page, gap)
        links = [(meta !>- ("index","shortname"),"index",False)] ++ pick "admin" ++
                gap (pick "popular") ++ [("All pages...","tags",False)]
        gap ((a,b,_):xs) = (a,b,True):xs

        pick tag = sort [(p !- "shortname",name,False) | p <- pages meta, tag `elem` words (p !- "tags")
                   ,let name = p !- "name", name /= "index"]

        f (title,page,gap) = "<li" ++ (if gap then " style='margin-top:10px'" else "") ++ ">" ++
                             "<a href='" ++ urlPage (noRoot meta) page ++ "'>" ++ title ++ "</a></li>"

---------------------------------------------------------------------
-- META OPERATIONS

meta ! x = page meta !- x
x !- y = fromMaybe (error $ "!- " ++ y ++ "\n" ++ show x) $ lookup y x

meta !> x = headNote ("!>, " ++ x) [p | p <- pages meta, lookup "name" p == Just x2]
    where x2 = takeBaseName x

meta !>- (page,y) = (meta !> page) !- y

elemFst x y = x `elem` map fst y

root meta = meta ! "root"
noRoot meta = meta{page=("root","$"):page meta}
repRoot meta = concatMap f
    where f x = if x == '$' then root meta else [x]

urlTag meta x = root meta ++ "tags/" ++
                (if "debug" `elemFst` global meta then "index.html" else "") ++
                "#" ++ x

urlPage meta x = concat [root meta
                        ,if x2 == "index" then "" else x2 ++ "/"
                        ,if "debug" `elemFst` global meta then "index.html" else ""]
    where x2 = takeBaseName x

args = map (uncurry (++))
args1 = head . args


---------------------------------------------------------------------
-- REWRITE

stream meta ('<':x:xs) | x `elem` ":!" && not ("DOCTYPE" `isPrefixOf` xs || "--" `isPrefixOf` xs) =
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
        title = if tag then tail name else meta !>- (name,"title")
        url   = if tag then urlTag meta (tail name) else urlPage meta name
        text  = if null atts then title else args1 atts


tag meta "email" a = "<span class='es_address'>" ++ concatMap f (args1 a) ++ "</span>"
    where f x = fromMaybe [x] $ lookup x [('@'," AT "),('.'," DOT ")]


tag meta "root" _ = root meta


tag meta "get" a = meta ! args1 a


tag meta "show-tags" _ = unwords $ map f $ sort $ words $ meta ! "tags"
    where f x = "<a href='" ++ urlTag meta x ++ "'>" ++ x ++ "</a>"


tag meta "show-catch" _ | not $ "catch" `elemFst` page meta = []
                        | otherwise =
    "<a href='" ++ urlPage meta "catch" ++ "'>" ++
        "<img style='border:0;' src='" ++ root meta ++ "elements/valid-catch.png' " ++
             "alt='Checked by Catch!' height='31' width='88' /></a>"


tag meta "show-menu" _ = repRoot meta $ global meta !- "menu"


tag meta "all-tags" _ = concatMap f tagList
    where
        tagList = sort [(a,b) | x <- lines $ unsafePerformIO $ readFile "tags.txt", let (a,_:b) = break (== '=') x]

        f (tag,desc) = if null items then "" else
                       "<p><b><a name='" ++ tag ++ "'></a>" ++ tag ++ "</b>: " ++ desc ++ "<br/>" ++
                       concat (intersperse ", " items)
            where items = concatMap (g tag) $ pages meta
        
        g tag page = ["<a href='" ++ urlPage meta (page !- "name") ++ "'>" ++ (page !- "shortname") ++ "</a>"
                     | tag `elem` words (page !- "tags")]


tag meta "all-pages" _ =
        "<p>" ++ concat (intersperse ", " $ map snd $ sortBy (compare `on` fst) $ map f $ pages meta) ++ "</p>"
    where
        f page = (map toLower title, "<a href=\"" ++ urlPage meta (page !- "name") ++ "\">" ++ title ++ "</a>")
            where title = page !- "shortname"


tag meta "downloads" _ | null down = ""
        | otherwise = "<h2>Downloads</h2>" ++ showDownloads (reparentDownloads down)
    where
        name = meta ! "name"
        down = map toDownload $ filter (\x -> name `elem` words (x !- "page")) $ extra meta

tag meta "all-downloads" _ = concatMap g groups 
    where
        groups = sortBy (compare `on` f) $ groupBy ((==) `on` icon) $ sortBy (compare `on` icon) items
        items = map (toDownloadPage meta) $ extra meta
        f (x:_) = fromMaybe maxBound $ findIndex ((==) (icon x) . fst) downloads
        
        downloads = let (*) = (,) in
            ["paper" * "Papers"
            ,"release" * "Releases"
            ,"manual" * "Manuals"
            ,"draft" * "Draft Papers"
            ,"slides" * "Presentation Slides"
            ,"darcs" * "Darcs Repositories"
            ,"haddock" * "Haddock Documentation"
            ,"blog" * "Blog Postings"
            ]

        g xs = "<h2>" ++ downloads !- icon (head xs) ++ "</h2>" ++ showDownloads xs



tag meta name atts = "<h1 style='color:red'>" ++ name ++ "</h1>" -- error $ "Unrecognised tag: " ++ name




-- year month day
type Sort = (Maybe (Int,Int,Int), String)
data Download = Download {key :: Sort, href :: String, parent :: String
                         ,icon :: String, entry :: String, children :: [Download]}
                deriving Show


toDownloadPage meta x = res{entry = entry res ++ " (<a href=\"" ++ url ++ "\">" ++ title ++ "</a>)"}
    where
        res = toDownload x
        url = urlPage meta name
        title = meta !>- (name, "shortname")
        name = head $ words $ x !- "page"

toDownload :: Data -> Download
toDownload x = Download key url (fromMaybe "" $ lookup "parent" x) typ entry []
    where
        url = x !- "url"
        typ = x !- "type"
        key = (liftM dateToSort $ lookup "date" x, typ) 

        entry | typ == "darcs" = "<a href='http://darcs.net/'>darcs</a> get --partial " ++
                                 "<a href='" ++ url ++ "'>" ++ url ++ "</a>"
              | otherwise = "<a href='" ++ url ++ "'>" ++ title ++ "</a>" ++
                            maybe [] (" - from " ++) (lookup "where" x) ++
                            maybe [] (" - " ++) (lookup "note" x)

        title = case lookup "title" x of
                    Just y -> y
                    _ | typ == "release" -> "Released on Hackage"
                      | typ == "blog" -> "Related blog posts"    


dateToSort :: String -> (Int,Int,Int)
dateToSort x = (negate $ read c, negate $ fromJust $ findIndex (== b) months, negate $ read a)
    where [a,b,c] = words x
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]


-- keep two piles, those you have processed looking for children
-- and those you haven't
-- assume only 1 level of nesting (for now)
reparentDownloads :: [Download] -> [Download]
reparentDownloads xs = ins [] xs
    where
        ins done [] = reverse done
        ins done (t:odo) = ins (t{children=d1++t1} : d2) t2
            where
                split = partition (\x -> parent x == href t)
                (d1,d2) = split done
                (t1,t2) = split odo


showDownloads :: [Download] -> String
showDownloads [] = []
showDownloads xs = "<ul>" ++ concatMap f (sortBy (compare `on` key) xs) ++ "</ul>"
    where f x = "<li class='" ++ icon x ++ "'>" ++ entry x ++ showDownloads (children x) ++ "</li>"



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
