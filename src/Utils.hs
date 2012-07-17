module Utils ( revertMap, listMap ) where
import Data.Map as M hiding ( map, foldl )


-- | Reverts the Map so that elements point to keys, duplicate elements with different keys get lost, 
--   e.g. { a-> 1, b -> 2} to { 1-> a, 2 -> b}
revertMap :: Ord b => Map a b -> Map b a
revertMap = fromList . map flipElems . toList
        where flipElems (a,b) = (b,a)


-- | Converts an associative 'List' (list of pairs) to a 'Map' with a 'List' of elements, 
--   e.g. [(a,0), (b,1),(b,2)] to {a -> [0], b -> [1,2]} 
listMap :: Ord k => [(k, a)] -> Map k [a]
listMap pairs =
    foldl insertPair empty (reverse pairs)
    where insertPair m (k,a) = insert k (a : findWithDefault [] k m) m