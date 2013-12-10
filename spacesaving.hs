module Main where
import qualified Data.Map.Lazy as M
import Data.Char (toLower)
import Data.List (minimumBy, maximumBy)

data Entry = Entry {
  entryValue :: Integer,
  entryError :: Integer
  }
  deriving (Show)

instance Ord Entry where
  e1 `compare` e2 = entryValue e1 `compare` entryValue e2

instance Eq Entry where
  e1 == e2 = entryValue e1 == entryValue e2

spaceSaving :: (Ord a) 
            => Int              
            -> [a]              
            -> M.Map a Entry

spaceSaving k = spaceSave M.empty 
  where
    spaceSave m [] = m
    spaceSave m (s:ss) 
      | M.member s m = update $ M.adjust (\e -> e {entryValue = entryValue e + 1}) s m
      | M.size m < k = update $ M.insert s (Entry 1 0) m
      | otherwise    = update $ updateLowestKey m s
     where
      update f = spaceSave f ss


updateLowestKey :: (Ord k) 
                 => M.Map k Entry -- Input map
                 -> k             -- Input value
                 -> M.Map k Entry -- Updated map

updateLowestKey m s = M.insert s (Entry (lowestValue + 1) lowestValue) lowestKeyRemoved
  where
    (lowestKey, Entry lowestValue _) = head . M.toList . minimum' $ m
    lowestKeyRemoved = M.delete lowestKey m
   
minimum' :: (Ord a) => M.Map k a -> M.Map k a
minimum' = toSingleton . minimumBy comp . M.toList
  where
    comp (_, x) (_, y) = compare x y
    toSingleton (k,a) = M.singleton k a

maximum' :: (Ord a) => M.Map k a -> M.Map k a
maximum' = toSingleton . maximumBy comp . M.toList
  where
    comp (_, x) (_, y) = compare x y
    toSingleton (k,a) = M.singleton k a

main :: IO ()
main = do
  contents <- getContents
  let pFile = words . map toLower $ contents
  print . maximum' $ spaceSaving 100 pFile

