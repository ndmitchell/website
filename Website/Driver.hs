
module Website.Driver(
    copy, process,
    module Website.Attrib,
    module Website.Wildcard
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import Website.Attrib
import Website.Wildcard
import Website.Util


outdir = "publish"

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


process :: (Config -> String -> IO String) -> [(FilePath, FilePath)] -> IO ()
process action xs = do
    xs <- concatMapM (\(x,y) -> expandWildcards (x, outdir </> y)) xs
    atts <- readFilesAttribs (map fst xs)
    mapM_ (f atts) xs
    where
        f atts (x,y) = do
            src <- readFileContents x
            src <- action (promoteConfig atts x += ("source",x)) src
            createDirectoryIfMissing True (takeDirectory y)
            writeFileBinary y src
