
module Website.Util where

import Control.Monad
import System.Directory
import System.FilePath


isDirectory x = if hasTrailingPathSeparator x
                then return True
                else doesDirectoryExist x

getDirContents x = do
    s <- getDirectoryContents x
    return $ filter (`notElem` [".",".."]) s

getDirContentsFull x = liftM (map (x </>)) $ getDirContents x

concatMapM f x = liftM concat $ mapM f x

