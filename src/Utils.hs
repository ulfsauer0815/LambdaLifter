-- | This module contains utility functions that are not application-specific.
module Utils ( revertMap, listMap, returnWhen ) where
import           Data.Map as M hiding (foldl, map)
import           Control.Monad (MonadPlus, guard)


-- | Reverts the Map so that elements point to keys, duplicate elements with different keys get lost.
--
-- >>> revertMap $ fromList [('a',1), ('b', 2)]
-- fromList [(1,'a'), (2, 'b')]
revertMap :: Ord b => Map a b -> Map b a
revertMap = fromList . map flipElems . toList
        where flipElems (a,b) = (b,a)


-- | Converts an associative 'List' (list of pairs) to a 'Map' with a 'List' of elements,
--
-- >>> listMap [('a',0), ('b',1),('b',2)]
-- fromList [('a',[0]), ('b', [1,2])]
listMap :: Ord k => [(k, a)] -> Map k [a]
listMap pairs =
    foldl insertPair empty (reverse pairs)
    where insertPair m (k,a) = insert k (a : findWithDefault [] k m) m

-- | Returns the value when the condition is met.
returnWhen :: MonadPlus m => Bool -> t -> m t
returnWhen cond val = guard cond >> return val