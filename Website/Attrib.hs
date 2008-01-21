
module Website.Attrib(
    Attribs, Config, FindAttrib,
    (!*), (!?), (!>), configAttribs,
    (+=), config, attribs, configWith
    ) where

import qualified Data.Map as Map
import System.FilePath


---------------------------------------------------------------------
-- DATA TYPE AND CLASSES

type Attrib a = Map.Map String [a]
data Attribs a = Attribs {fromAttribs :: Attrib a}
                 deriving Show

data Config a = Config (Map.Map FilePath (Attribs a)) (Attribs a)
                deriving Show



class FindAttrib a where
    getAttrib :: a v -> Attrib v
    setAttrib :: a v -> Attrib v ->  a v

instance FindAttrib Attribs where
    getAttrib = fromAttribs
    setAttrib _ = Attribs

instance FindAttrib Config where
    getAttrib (Config a b) = getAttrib b
    setAttrib (Config a b) x = Config a (setAttrib b x)


---------------------------------------------------------------------
-- CREATION

config :: [(FilePath,[(String,v)])] -> Config v
config xs = Config (Map.fromList [(a, attribs b) | (a,b) <- xs]) attribsEmpty


-- some results may be duplicated, in this case
-- the earliest ones should be first
attribs :: [(String,v)] -> Attribs v
attribs xs = foldl (+=) attribsEmpty xs


attribsEmpty = Attribs Map.empty


-- | Add an extra value, onto the end
(+=) :: FindAttrib a => a v -> (String,v) -> a v
(+=) a (k,v) = setAttrib a $ Map.insertWith (flip (++)) k [v] $ getAttrib a


configWith :: Config v -> Attribs v -> Config v
configWith (Config x _) a = Config x a


---------------------------------------------------------------------
-- QUERY


-- | Get all the associated attributes
(!*) :: FindAttrib a => a v -> String -> [v]
a !* s = Map.findWithDefault [] s $ getAttrib a


-- | Does a particular attribute exist
(!?) :: FindAttrib a => a v -> String -> Bool
a !? s = Map.member s $ getAttrib a


-- | Follow into the general attributes of a particular file
(!>) :: Config v -> FilePath -> Attribs v
(Config x _) !> s = Map.findWithDefault attribsEmpty (normalise s) x


configAttribs :: Config v -> [Attribs v]
configAttribs (Config x _) = Map.elems x
