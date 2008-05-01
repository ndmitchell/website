
module Website.Download(
    Download(dlEntry, dlPage), readDownload,
    showDownloadGroup, showDownloadTree
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Safe
import Website.Driver


type Date = (Int, Int, Int)

data Download = Download
    {date :: Maybe Date      -- ^ Date on which the item was given
    ,typ :: DownloadType     -- ^ Type of item
    ,url :: URL              -- ^ Where it is
    ,parent :: URL           -- ^ Its parent URL
    ,dlEntry :: String       -- ^ The text
    ,dlPage :: String        -- ^ The page it is on
    ,children :: [Download]  -- ^ Any children
    } deriving Show

instance Eq Download where
    (==) x y = compare x y == EQ

instance Ord Download where
    compare d1 d2 = compare (date d1, typ d1) (date d2, typ d2)


data DownloadType = Paper | Release | Manual | Draft | Slides | Video | Darcs | Haddock | Blog
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
    Haddock -> "Haddock Documentation"
    Blog    -> "Blog Postings"
    _ -> show x ++ "s"



readDownload :: Data -> Download
readDownload x = Download date typ url (fromMaybe "" $ lookup "parent" x) entry (x !# "page") []
    where
        url = x !# "url"
        typ = readDownloadType $ x !# "type"
        date = liftM dateToSort $ lookup "date" x 

        entry | typ == Darcs = "<a href='http://darcs.net/'>darcs</a> get --partial " ++
                               "<a href='" ++ url ++ "'>" ++ url ++ "</a>"
              | otherwise = "<a href='" ++ url ++ "'>" ++ title ++ "</a>" ++
                            maybe [] (" - from " ++) (lookup "where" x) ++
                            maybe [] (" - " ++) (lookup "note" x)

        title = case lookup "title" x of
                    Just y -> y
                    _ | typ == Release -> "Released version"
                      | typ == Blog    -> "Related blog posts"
                      | typ == Haddock -> "Haddock documentation"


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
                split = partition (\x -> parent x == url t)
                (d1,d2) = split done
                (t1,t2) = split odo


showDownloads :: [Download] -> String
showDownloads [] = []
showDownloads xs = "<ul>" ++ concatMap f (sort xs) ++ "</ul>"
    where f x = "<li class='" ++ map toLower (show (typ x)) ++ "'>" ++
                dlEntry x ++ showDownloads (children x) ++ "</li>"


