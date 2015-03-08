{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Control.Monad
import System.IO.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Char


main :: IO ()
main = do
    template <- readFile' "template.html"
    metadata <- readFile' "metadata.txt"
    let download = unlines $
            ("<p>All papers and talks are listed below, most recent first. " ++
             "Show all <a href=\"javascript:showAbstracts()\">abstracts</a> or " ++
             "<a href=\"javascript:showCitations()\">citations</a>.</p>") :
            concat (zipWith renderMetadata [1..] $ checkMetadata $ parseMetadata metadata)
    let reps = [("#{" ++ lower proj ++ "}", "<a href=\"" ++ url ++ "\">" ++ proj ++ "</a>") | (proj,url) <- projects]
    let res = replaces (("#{download}",download):reps) template
    when ("#{" `isInfixOf` res) $ error $ "Missed a replacement, " ++ take 20 (snd $ breakOn "#{" res) ++ "..."
    writeFile "index.html" res
    putStrLn "Generated to index.html"

projects :: [(String, String)]
projects =
    [("Shake","http://shakebuild.com/")
    ,("Hoogle","http://hoogle.haskell.org/")] ++
    [(x, "https://github.com/ndmitchell/" ++ lower x)
    | x <- words "HLint Supero Derive Firstify Catch Uniplate"]

replaces :: Eq a => [([a], [a])] -> [a] -> [a]
replaces reps x = foldl (\x (from,to) -> replace from to x) x reps

renderMetadata :: Int -> [(String, String)] -> [String]
renderMetadata unique xs =
        [""
        ,"<h3>" ++ typ ++ ": " ++ at "title" ++ "</h3>"
        ,"<p class=\"info\">" ++ intercalate ", " parts ++ (if null $ at "where" then "" else " from " ++ at "where") ++ ", " ++ at "date" ++ ".</p>"
        ,"<p id=\"citation" ++ show unique ++ "\" class=\"citation\">@citation goes here{}</p>"] ++
        ["<p id=\"abstract" ++ show unique ++ "\" class=\"abstract\"><b>Abstract:</b> " ++ replace "\n" "<br/><br/>" abstract ++ "</p>" | abstract /= ""] ++
        ["<p class=\"text\">" ++ at "text" ++ "</p>"]
    where
        typ = if ("@at","phdthesis") `elem` xs then "Thesis"
              else if "paper" `elem` keys then "Paper"
              else if "slides" `elem` keys then "Talk"
              else "Other"
        parts = [ "<a href=\"" ++ v ++ "\">" ++ (if i == 0 then toUpper (head k) : tail k else k) ++ "</a>"
                | (i,(k,v)) <- zip [0..] $ filter (not . null . snd) $ map (id &&& at) $ words "paper slides video audio"] ++
                [ "<a href=\"javascript:showCitation(" ++ show unique ++ ")\">citation</a>"] ++
                [ "<a href=\"javascript:showAbstract(" ++ show unique ++ ")\">abstract</a>" | abstract /= ""]

        at x = unwords $ map snd $ filter ((==) x . fst) xs
        abstract = at "abstract"
        keys = map fst xs


checkMetadata :: [[(String, String)]] -> [[(String, String)]]
checkMetadata xs | all (checkFields . map fst) xs = reverse $ sortOn date xs
    where
        checkFields xs | bad:_ <- xs \\ nub xs = error $ "Duplicate field, " ++ bad
                       | bad:_ <- filter (not . isPrefixOf "@") xs \\ (required ++ optional) = error $ "Unknown field, " ++ bad
                       | bad:_ <- required \\ xs = error $ "Missing field, " ++ bad
                       | otherwise = True
        date = parseDate . fromJust . lookup "date"

        required = words "title date text"
        optional = words "paper slides video audio where author abstract"


parseDate :: String -> (Int, Int, Int)
parseDate x | [a,b,c] <- words x = (read c, 1 + fromMaybe (error $ "bad month, " ++ x) (elemIndex b months), read a)
    where months = words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"


parseMetadata :: String -> [[(String, String)]]
parseMetadata = map (map f) . wordsBy null . rejoin . map trimEnd . lines . replace "\t" "    "
    where
        f = second (trim . drop 1) . breakOn ":"

        rejoin (x:"":(' ':y):zs) = rejoin $ (x ++ "\n" ++ trim y) : zs
        rejoin (x:(' ':y):zs) = rejoin $ (x ++ " " ++ trim y) : zs
        rejoin (x:xs) = x : rejoin xs
        rejoin [] = []
