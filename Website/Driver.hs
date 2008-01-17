
module Website.Driver(
    copy, process,
    module Website.Attrib,
    module Website.Wildcard
    ) where

import System.Directory
import System.FilePath
import Website.Attrib
import Website.Wildcard
import Website.Util


outdir = "publish"

copy :: FilePath -> FilePath -> IO ()
copy x y = do
    cp <- expandWildcards (x, outdir </> y)
    mapM_ (uncurry copyFile) cp


process :: (Config -> String -> IO String) -> [(FilePath, FilePath)] -> IO ()
process action xs = do
    xs <- concatMapM (\(x,y) -> expandWildcards (x, outdir </> y)) xs
    atts <- readFilesAttribs (map fst xs)
    mapM_ (f atts) xs
    where
        f atts (x,y) = do
            src <- readFile x
            src <- action (promoteConfig atts x) src
            writeFile y src
