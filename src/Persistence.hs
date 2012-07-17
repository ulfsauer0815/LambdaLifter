module Persistence ( readLevelFile ) where
import Data.Map         as M ( Map, empty, fromList, insert )
import Prelude          as P hiding ( lookup )
import Data.List        as L ( isPrefixOf )

import Game

readLevelFile :: FilePath -> IO Level
readLevelFile f = do
        c  <- readFile f
        let (lvlString, lvlMetadata) = splitLevelAndMetadataString . lines $ c
        let lMap = levelStringToMap lvlString
        let lTrampolines = extractTrampolinesFromMetadata empty lvlMetadata

        return Level { lvMap = lMap
                     , lvTrampolines = lTrampolines}
        where
        splitLevelAndMetadataString :: [String] -> ([String], [String])
        splitLevelAndMetadataString s = (tFst, P.filter (/= "") tSnd)
                where (tFst, tSnd) = span (/= "") s
        -- TODO: quick and dirty parsing :(
        extractTrampolinesFromMetadata :: Map Object Object -> [String] -> Map Object Object
        extractTrampolinesFromMetadata tMap []           = tMap
        extractTrampolinesFromMetadata tMap (l:ls) =
                if "Trampoline " `isPrefixOf` l
                  then
                        extractTrampolinesFromMetadata (insert (Trampoline trampo) (Target target) tMap) ls
                  else
                        tMap
                where
                (trampo:target:_) = P.filter (`elem` ['A' .. 'I'] ++ ['0'..'9']) l


levelStringToMap :: [String] -> Map Position Object
levelStringToMap sl = fromList . concatMap (\(y, s) -> zip [(x,y) | x <- [1..]] (map charToObject s)) $ zip [1..] (reverse sl)