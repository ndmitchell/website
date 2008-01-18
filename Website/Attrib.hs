
module Website.Attrib(
    Attribs, Config,
    (!*), (!+), (!?), (!>),
    (+=),
    readFilesAttribs, readFileAttribs, readFileContents,
    configAttribs,
    promoteConfig
    ) where

import Control.Monad
import qualified Data.Map as Map
import System.Environment
import System.FilePath


data Attribs = Attribs (Map.Map String [String])
               deriving Show

data Config = Config (Map.Map FilePath Attribs) Attribs



class FindAttribs a where
    getAttribs :: a -> Attribs
    setAttribs :: a -> Attribs ->  a

instance FindAttribs Attribs where
    getAttribs = id
    setAttribs _ = id

instance FindAttribs Config where
    getAttribs (Config _ x) = x
    setAttribs (Config a _) x = Config a x


-- | Get all the associated attributes
(!*) :: FindAttribs a => a -> String -> [String]
a !* s = Map.findWithDefault [] s x
    where Attribs x = getAttribs a

-- | Get one associated attribute (empty string on failure)
(!+) :: FindAttribs a => a -> String -> String
a !+ s = head $ (a !* s) ++ [""]

-- | Does a particular attribute exist
(!?) :: FindAttribs a => a -> String -> Bool
a !? s = Map.member s x
    where Attribs x = getAttribs a


-- | Follow into the general attributes of a particular file
(!>) :: Config -> FilePath -> Attribs
(Config x _) !> s = Map.findWithDefault (Attribs Map.empty) (normalise s) x


-- | Add an extra value, onto the end
(+=) :: FindAttribs a => a -> (String,String) -> a
(+=) a (k,v) = setAttribs a $ Attribs $ Map.insertWith (flip (++)) k [v] x
    where Attribs x = getAttribs a


configAttribs :: Config -> [Attribs]
configAttribs (Config x _) = Map.elems x


promoteConfig :: Config -> FilePath -> Config
promoteConfig c@(Config x _) s = Config x (c !> s)


readFilesAttribs :: [FilePath] -> IO Config
readFilesAttribs files = do
    res <- mapM readFileAttribs files
    return $ Config
        (Map.fromList $ zip (map normalise files) res)
        (error "You must promote a Config before using it")

readFileContents :: FilePath -> IO String
readFileContents = liftM skipFileAttribs . readFile


readFileAttribs :: FilePath -> IO Attribs
readFileAttribs s = do
    r <- addArgsAttribs .  readAttribs . takeWhile (not . null) . lines =<< readFile s
    return $ r += ("file",normalise s)


skipFileAttribs :: String -> String
skipFileAttribs = unlines . dropWhile null . dropWhile (not . null) . lines


getArgsAttribs :: IO Attribs
getArgsAttribs = liftM readAttribs getArgs


addArgsAttribs :: Attribs -> IO Attribs
addArgsAttribs (Attribs orig) = do
    Attribs new <- getArgsAttribs
    return $ Attribs $ Map.unionWith (++) new orig


readAttribs :: [String] -> Attribs
readAttribs = Attribs . foldl f Map.empty . map readAttrib
    where f mp (a,b) = Map.insertWith (++) a [b] mp


readAttrib :: String -> (String,String)
readAttrib s = (a, drop 1 b)
    where (a,b) = break (== '=') s

