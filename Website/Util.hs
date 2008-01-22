
module Website.Util where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO


isDirectory x = if hasTrailingPathSeparator x
                then return True
                else doesDirectoryExist x

getDirContents x = do
    s <- getDirectoryContents $ dropTrailingPathSeparator x
    return $ filter (`notElem` [".",".."]) s

getDirContentsFull x = liftM (map (x </>)) $ getDirContents x

concatMapM f x = liftM concat $ mapM f x

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

split :: Char -> String -> [String]
split x xs = case break (== x) xs of
                (a,_:b) -> a : split x b
                _ -> [xs]

on f g x y = f (g x) (g y)
