
module Website.Wildcard(expandWildcards) where

import Control.Monad
import System.Directory
import System.FilePath


expandWildcards :: (FilePath,FilePath) -> IO [(FilePath,FilePath)]

expandWildcards (lhs,rhs) | '*' `elem` takeFileName lhs = do
    let (ldir,lnam) = splitFileName lhs
    files <- getDirContents ldir
    files <- return [(x,m) | x <- files, Just m <- [matchWildcard lnam x]]

    let subst x m = if '*' `elem` rhs then replaceWildcard rhs m else rhs </> x
    concatMapM (\(x,m) -> expandWildcards (ldir </> x, subst x m)) files

expandWildcards (lhs,rhs) = do
    lDir <- isDirectory lhs
    if lDir then do
        s <- getDirContents lhs
        concatMapM (\x -> expandWildcards (lhs </> x, rhs </> x)) s
     else do
        rDir <- isDirectory rhs
        let dest = if rDir then rhs </> takeFileName lhs else rhs
        return [(lhs,dest)]


---------------------------------------------------------------------
-- WILDCARD MATCHING

matchWildcard :: String -> String -> Maybe String
matchWildcard (x:xs) (y:ys) | x == y = matchWildcard xs ys
matchWildcard [] [] = Just ""

matchWildcard ('*':xs) ys | xs == reverse a = Just $ reverse b
    where (a,b) = splitAt (length xs) $ reverse ys
matchWildcard _ _ = Nothing


replaceWildcard :: String -> String -> String
replaceWildcard x with = concatMap (\y -> if y == '*' then with else [y]) x

---------------------------------------------------------------------
-- AUXILIARY UTILITIES

isDirectory x = if hasTrailingPathSeparator x
                then return True
                else doesDirectoryExist x

getDirContents x = do
    s <- getDirectoryContents x
    return $ filter (`notElem` [".",".."]) s

getDirContentsFull x = liftM (map (x </>)) $ getDirContents x

concatMapM f x = liftM concat $ mapM f x
