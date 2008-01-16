
module Website.Driver(
    copy, process
    ) where

import Website.Attrib


copy :: FilePath -> FilePath -> IO ()
copy x y = error "Website.Driver.copy"


process :: (Config -> String -> IO String) -> [(FilePath, FilePath)] -> IO ()
process x y = error "Website.Driver.process"
