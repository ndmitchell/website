
module Website.Driver(
    copy, process, newFile,
    module Website.Wildcard,
    module Website.Metadata,
    module Website.Util
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import Website.Wildcard
import Website.Util
import Website.Metadata

outdir = "web"

copy :: FilePath -> FilePath -> IO ()
copy x y = do
    cp <- expandWildcards (x, outdir </> y)
    flip mapM_ cp $ \(x,y) -> do
        let action = do
            createDirectoryIfMissing True (takeDirectory y)
            copyFileBinary x y
        
        existY <- doesFileExist y
        if not existY then action else do
            timeX <- getModificationTime x
            timeY <- getModificationTime y
            when (timeX > timeY) action


newFile :: FilePath -> String -> IO ()
newFile file src = do
    let dest = outdir </> file
    createDirectoryIfMissing True (takeDirectory dest)
    writeFileBinary dest src


process :: (FilePath -> IO String) -> [(FilePath,FilePath)] -> IO ()
process rewrite xs = do
    xs <- concatMapM (\(x,y) -> expandWildcards (x, outdir </> y)) xs
    mapM_ f xs
    where
        f (from,to) = do
            res <- rewrite from
            createDirectoryIfMissing True (takeDirectory to)
            writeFileBinary to res
