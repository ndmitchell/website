
module Website.Attrib(
    Attribs, Config,
    (!*), (!+), (!?), (!>),
    readFileAttribs, getArgsAttribs,
    configAttribs
    ) where

import qualified Data.Map as Map
import System.Environment


data Attribs = Attribs (Map.Map String [String])

data Config = Config (Map.Map FilePath Attribs) Attribs



class GetAttribs a where
    getAttribs :: a -> Attribs

instance GetAttribs Attribs where getAttribs = id
instance GetAttribs Config where getAttribs (Config _ x) = x


-- | Get all the associated attributes
(!*) :: GetAttribs a => a -> String -> [String]
a !* s = Map.findWithDefault [] s x
    where Attribs x = getAttribs a

-- | Get one associated attribute (empty string on failure)
(!+) :: GetAttribs a => a -> String -> String
a !+ s = head $ (a !* s) ++ [""]

-- | Does a particular attribute exist
(!?) :: GetAttribs a => a -> String -> Bool
a !? s = Map.member s x
    where Attribs x = getAttribs a


-- | Follow into the general attributes of a particular file
(!>) :: Config -> FilePath -> Attribs
(Config x _) !> s = Map.findWithDefault (Attribs Map.empty) s x


configAttribs :: Config -> [Attribs]
configAttribs (Config x _) = Map.elems x


readFileAttribs :: FilePath -> IO Attribs
readFileAttribs s = return . readAttribs . takeWhile (not . null) . lines =<< readFile s


skipFileAttribs :: String -> String
skipFileAttribs = unlines . dropWhile null . dropWhile (not . null) . lines


getArgsAttribs :: IO Attribs
getArgsAttribs = return . readAttribs =<< getArgs


readAttribs :: [String] -> Attribs
readAttribs = Attribs . foldl f Map.empty . map readAttrib
    where f mp (a,b) = Map.insertWith (++) a [b] mp


readAttrib :: String -> (String,String)
readAttrib s = (a, drop 1 b)
    where (a,b) = break (== '=') s

