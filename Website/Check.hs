
module Website.Check(check) where

import Control.Monad
import Data.List
import Text.HTML.Download
import Text.HTML.TagSoup


-- list all the pages that aren't valid
check :: [String] -> IO ()
check xs = do
    failed <- liftM sum $ mapM checkOne xs
    if failed == 0 then
        putStrLn $ "\nSuccess, no invalid pages"
     else
        putStrLn $ "\nFailure, " ++ show failed ++ " invalid pages"


checkOne :: String -> IO Int
checkOne x = do
    putChar '.'
    src <- openURL $ "http://validator.w3.org/check?uri=" ++ x
    let text = words $ innerText $ take 1 $ drop 1 $ dropWhile (~/= "<title>") $ parseTags src
        failed = not $ ["[Valid]"] `isPrefixOf` text
    when failed $
        putStrLn $ "\nInvalid: " ++ x
    return $ if failed then 1 else 0
