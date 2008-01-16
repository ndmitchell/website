
module Website.Driver(
    copy, process, execute
    ) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import Website.Attrib


data Action = Copy FilePath FilePath
            | Process FilePath FilePath
            deriving Show


-- stored in reverse
{-# NOINLINE todo #-}
todo :: IORef [Action]
todo = unsafePerformIO $ newIORef []


addTodo x = modifyIORef todo (x:)


copy :: FilePath -> FilePath -> IO ()
copy x y = addTodo $ Copy x y


process :: FilePath -> FilePath -> IO ()
process x y = addTodo $ Process x y


execute :: (Config -> String -> IO String) -> IO ()
execute f = do
    t <- liftM reverse $ readIORef todo
    error $ show t


