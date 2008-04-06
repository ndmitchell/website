
module Website.Metadata(
    Metadata(..),
    readMetadataGlobal, readMetadataFile,
    readMetadataHead, dropMetadataHead
    ) where

import Control.Arrow
import Data.Char
import System.Environment


data Metadata = Metadata {global :: Data, page :: Data, meta :: [Data], pages :: [Data]}
type Data = [(String,String)]


readMetadataGlobal :: IO Data
readMetadataGlobal = do
    xs <- getArgs
    return $ map divide xs

divide = (trim *** trim . drop 1) . break (== '=')



data Line = Attrib String String | Blank | Indent String


readMetadataFile :: FilePath -> IO [Data]
readMetadataFile file = do
    src <- readFile file
    return $ filter (not . null) $ f $ map classify $ lines src
    where
        classify xs | all isSpace xs = Blank
        classify (x:xs) | isSpace x = Indent (trim xs)
        classify xs = uncurry Attrib (divide xs)

        f (Attrib x y:Indent z:rest) = f (Attrib x (y ++ "\n" ++ z) : rest)
        f (Attrib x y:rest) = let (r:rs) = f b in ((x,a):r) : rs
            where (a,b) = g rest
        f (Blank:rest) = [] : f rest
        f [] = [[]]

        g (Indent x:Blank:Indent y:rest) = g $ Indent (x ++ "\n\n" ++ y) : rest
        g (Indent x:Indent y:rest) = g $ Indent (x ++ "\n" ++ y) : rest
        g (Indent x:rest) = ("\n" ++ x,rest)
        g rest = ("",rest)


readMetadataHead :: FilePath -> IO Data
readMetadataHead file = do
    src <- readFile file
    return $ map divide $ takeWhile (not . null) $ lines src


dropMetadataHead :: FilePath -> IO String
dropMetadataHead file = do
    src <- readFile file
    return $ unlines $ dropWhile null $ dropWhile (not . null) $ lines src


trim = ltrim . rtrim
ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
