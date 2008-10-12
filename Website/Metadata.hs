
module Website.Metadata(
    Metadata(..), Data,
    readMetadataFile,
    readMetadataHead, dropMetadataHead
    ) where

import Control.Arrow
import Data.Char


data Metadata = Metadata {global :: Data, page :: Data, extra :: [Data], pages :: [Data]}
type Data = [(String,String)]


divide = (trim *** trim . drop 1) . break (== '=')



data Line = Attrib String String | Blank | Indent String


readMetadataFile :: FilePath -> IO [Data]
readMetadataFile file = do
    src <- readFile file
    return $ collate [] [] $ join $ map classify $ lines src
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

        collate def seen (Blank:xs) = [def ++ reverse seen | not $ null seen] ++ collate def [] xs
        collate def seen (Attrib ('*':a) b:xs) = collate (addDef def a b) seen xs
        collate def seen (Attrib a b:xs) = collate def ((a,b):seen) xs
        collate def [] [] = []

        addDef def a b = filter ((/=) a . fst) def ++ [(a,b) | b /= ""]


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
