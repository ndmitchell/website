
module Website.Metadata(
    Metadata(..), Data,
    readMetadataGlobal, readMetadataFile,
    readMetadataHead, dropMetadataHead
    ) where

import Control.Arrow
import Data.Char
import System.Environment


data Metadata = Metadata {global :: Data, page :: Data, extra :: [Data], pages :: [Data]}
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
    return $ collate [] $ join $ map classify $ lines src
    where
        -- classify each line by what it is
        classify xs | all isSpace xs = Blank
        classify ('%':xs) = Blank
        classify (x:xs) | isSpace x = Indent (trim xs)
        classify xs = uncurry Attrib (divide xs)

        -- joint Attrib:Indent and Indent:Blank:Indent
        join (a:Indent b:Indent c:rest) = join (a:Indent (b ++ "\n" ++ c):rest)
        join (a:Indent b:Blank:Indent c:rest) = join (a:Indent (b ++ "\n\n" ++ c):rest)
        join (Attrib a b:Indent c:rest) = Attrib a (b ++ "\n" ++ c) : join rest
        join (x:xs) = x : join xs
        join [] = [Blank]

        collate seen (Blank:xs) = [reverse $ seen | not $ null seen] ++ collate [] xs
        collate seen (Attrib a b:xs) = collate ((a,b):seen) xs
        collate [] [] = []


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
