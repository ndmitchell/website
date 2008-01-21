
module Website.Driver(
    copy, process,
    module Website.Attrib,
    module Website.Wildcard,
    module Website.Util
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


process :: 
    (FilePath -> IO (a, [(String,b)])) ->
    (Config b -> a -> IO String) -> 
    [(FilePath,FilePath)] -> IO ()
process reader action xs = do
    xs <- concatMapM (\(x,y) -> expandWildcards (x, outdir </> y)) xs
    xs <- mapM (\(from,to) -> do (src,atts) <- reader from ; return (from,to,src,atts)) xs
    atts <- return $ config [(from, atts) | (from,_,_,atts) <- xs]
    mapM_ (f atts) xs
    where
        f atts (from,to,src,_) = do
            src <- action (configWith atts (atts !> from)) src
            createDirectoryIfMissing True (takeDirectory to)
            writeFileBinary to src
