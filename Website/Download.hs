
module Website.Download(
    Download(dlText, dlPage), readDownload,
    showDownloadGroup, showDownloadTree,
    showBibtexPapers, showBibtexSlides
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup
import System.FilePath
import Website.Driver


type Date = (Int, Int, Int)

data Download = Download
    {date :: Maybe Date      -- ^ Date on which the item was given
    ,typ :: DownloadType     -- ^ Type of item
    ,url :: URL              -- ^ Where it is
    ,parent :: URL           -- ^ Its parent URL
    ,dlText :: String        -- ^ The text
    ,dlPage :: String        -- ^ The page it is on
    ,abstract :: String      -- ^ The abstract (may be blank)
    ,bibtex :: String        -- ^ The bibtex entry (may be blank)
    ,children :: [Download]  -- ^ Any children
    } deriving Show

instance Eq Download where
    (==) x y = compare x y == EQ

instance Ord Download where
    compare d1 d2 = compare (date d1, typ d1) (date d2, typ d2)


data DownloadType = Paper | Draft | Release | Manual | Slides | Video | Audio | Git | Darcs | Haddock | Blog | Bug
                     deriving (Read, Show, Enum, Ord, Eq, Bounded)

allDownloadType :: [(String, DownloadType)]
allDownloadType = [(map toLower $ show x, x) | x <- [minBound..maxBound]]


readDownloadType :: String -> DownloadType
readDownloadType = (!#) allDownloadType

showDownloadTypeTitle :: DownloadType -> String
showDownloadTypeTitle x = case x of
    Draft   -> "Draft Papers"
    Slides  -> "Presentation Slides"
    Darcs   -> "Darcs Repositories"
    Git     -> "Git Repositories"
    Haddock -> "Haddock Documentation"
    Blog    -> "Blog Postings"
    Bug     -> "Bug Trackers"
    _ -> show x ++ "s"



readDownload :: Data -> Download
readDownload x = Download date typ url parent
                 entry page (fromMaybe "" $ lookup "text" x) bibtex []
    where
        page = x !# "page"
        parent = fromMaybe "" $ lookup "parent" x
        url = x !# "url"
        typ = readDownloadType $ x !# "type"
        date = liftM dateToSort $ lookup "date" x 

        entry | typ == Darcs && "https:" `isPrefixOf` url = "<a href='" ++ url ++ "'>Source code</a>"
              | typ == Darcs = "<a href='http://darcs.net/'>darcs</a> get --lazy " ++
                               "<a href='" ++ url ++ "'>" ++ url ++ "</a>"
              | typ == Git = "git clone " ++
                               "<a href='" ++ url ++ "'>" ++ url ++ "</a>"
              | otherwise = "<a href='" ++ url ++ "'>" ++ title ++ "</a>" ++
                            maybe [] (" - from " ++) (lookup "where" x) ++
                            maybe [] (" - " ++) (lookup "note" x)

        title = case lookup "title" x of
                    Just y -> y
                    _ | typ == Release -> "Released version"
                      | typ == Blog    -> "Related blog posts"
                      | typ == Haddock -> "Haddock documentation"
                      | typ == Bug     -> "Bug tracker"

        bibtex | typ `notElem` [Paper,Manual,Draft,Slides] = ""
               | parent /= "" && typ == Slides = ""
               | otherwise = unlines $ ("@" ++ at ++ "{mitchell:" ++ key) :
                                       map showBibLine items ++ ["}"]
            where
                (at,ex) | typ == Manual = ("manual",[])
                        | typ == Draft = ("unpublished",[("note","Draft" ++ noteText)])
                        | typ == Slides = ("misc",[("note","Presentation" ++ whereText)])
                        | otherwise = (fromMaybe "inproceedings" $ lookup "@at" x, [])
                items = filter (not . null . snd)
                        [("title", capitalise $ x !# "title")
                        ,("author", fromMaybe "Neil Mitchell" $ lookup "author" x)
                        ,("year", maybe "" (\(a,b,c) -> show (negate a)) date)
                        ,("month", maybe "" (\(a,b,c) -> months !! negate b) date)
                        ,("day", maybe "" (\(a,b,c) -> show (negate c)) date)
                        ] ++ ex ++
                        [(a,b) | ('@':a,b) <- x, a /= "at"] ++
                        [("url", "\\verb'" ++ url ++ "'")]

                key = map toLower page ++ keyText ++ keyDate
                keyText = if typ == Manual then "_manual" else ""
                keyDate = maybe "" (\(a,b,c) -> concatMap ((:) '_' . show . negate) [a,b-1,c]) date
                whereText = maybe [] (\x -> " from " ++ innerText (parseTags x)) $ lookup "where" x
                noteText = maybe [] (\x -> ", " ++ innerText (parseTags x)) $ lookup "note" x

showBibLine (a,b) = "    ," ++ a ++ replicate (14 - length a) ' ' ++ " = {" ++ (if a == "pages" then f b else b) ++ "}"
    where
        f (x:'-':y:xs) | isDigit x && isDigit y = x:'-':'-':y : f xs
        f (x:xs) = x : f xs
        f [] = []

-- capitalise the title in some way
capitalise :: String -> String
capitalise str = unwords (f True x : map (f False) xs)
    where
        (x:xs) = words str

        f first (x:xs) | ":" `isSuffixOf` xs = f first (x : take (length xs - 1) xs) ++ ":"
                       | (any isUpper xs && '-' `notElem` xs)
                       || (not first && (x:xs) `elem` names) = "{" ++ x:xs ++ "}"
                       | otherwise = x:xs

names = ["Haskell","Uniplate","Hat","Windows","Pasta"]


showDownloadTree :: [Download] -> String
showDownloadTree = showDownloads . reparentDownloads


showDownloadGroup :: [Download] -> String
showDownloadGroup =
        concatMap f . sortBy (compare `on` (typ . head)) .
        groupBy ((==) `on` typ) . sortBy (compare `on` typ)
    where
        f xs@(x:_) = "<h2>" ++ showDownloadTypeTitle (typ x) ++ "</h2>" ++ showDownloads xs


-- year month day

dateToSort :: String -> (Int,Int,Int)
dateToSort x = (negate $ read c, negate $ fromJust $ findIndex (isPrefixOf b) months, negate $ read a)
    where [a,b,c] = words x
months = ["January","February","March","April","May","June"
         ,"July","August","September","October","November","December"]


-- keep two piles, those you have processed looking for children
-- and those you haven't
-- assume only 1 level of nesting (for now)
reparentDownloads :: [Download] -> [Download]
reparentDownloads xs = ins [] xs
    where
        ins done [] = reverse done
        ins done (t:odo) = ins (t{children=d1++t1} : d2) t2
            where
                split = partition (\x -> parent x == url t)
                (d1,d2) = split done
                (t1,t2) = split odo


showDownloads :: [Download] -> String
showDownloads [] = []
showDownloads xs = "<ul>" ++ concatMap f (sort xs) ++ "</ul>"
    where f x = "<li class='" ++ map toLower (show (typ x)) ++ "'>" ++
                dlText x ++ showAbstract x ++ showBibtex x ++
                showDownloads (children x)++ "</li>"


showAbstract :: Download -> String
showAbstract x | null $ abstract x = ""
               | otherwise = " " ++
    showHide "abstract" uid ++
    "<div id='abstract_text_" ++ uid ++ "' class='abstract' style='display:none;'>" ++
    f (abstract x) ++ "</div>"
    where
        uid = urlToId (url x)

        f ('$':'\\':'b':'o':'t':'$':xs) = "_|_" ++ f xs
        f ('\n':'\n':xs) = "<br/><br/>" ++ f xs
        f (x:xs) = x : f xs
        f [] = []


showBibtex :: Download -> String
showBibtex x | null $ bibtex x = ""
             | otherwise = " " ++
    showHide "bibtex" uid ++
    "<div id='bibtex_text_" ++ uid ++ "' class='bibtex' style='display:none;'>" ++
    f (bibtex x) ++ "</div>"
    where
        uid = urlToId (url x)

        f (' ':' ':xs) = "&nbsp;" ++ f (' ':xs)
        f ('\n':xs) = "<br/>" ++ f xs
        f (x:xs) = x : f xs
        f [] = []


showHide item uid = f True ++ f False
    where
        f b = "<span class='more'" ++
              (if b then "" else " style='display:none'") ++
              " id='" ++ item ++ "_" ++ name ++ "_" ++ uid ++ "'" ++
              ">(<a class='more'" ++
              " href=\"javascript:" ++ item ++ "_" ++ name ++ "('" ++ uid ++ "')\">" ++
              (if b then item else "hide&nbsp;" ++ item) ++
              "</a>)</span>"
            where name = if b then "show" else "hide"


urlToId :: URL -> String
urlToId = map f . takeBaseName
    where
        f x | isAlphaNum x = x
            | otherwise = '_'


showBibtexPapers, showBibtexSlides :: [Download] -> String
showBibtexPapers = bibtexFile (/= Slides)
showBibtexSlides = bibtexFile (== Slides)


bibtexFile test = unlines . map bibtex . sort . filter (not . null . bibtex) . filter (test . typ)
