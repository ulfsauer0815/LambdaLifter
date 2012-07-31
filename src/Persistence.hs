module Persistence ( readLevelFile ) where
import Data.Map         as M ( Map, empty, fromList, insert, filter, size )
import Prelude          as P hiding ( lookup )
import Data.List        as L ( isPrefixOf )

import Game ( LevelMap, Level(..), Object(..), charToObject, ObjectInitValues(..), LevelValues(..), defaultLevelValues, isLambdaLike)
import Data.Char (isDigit)


readLevelFile :: FilePath -> IO Level
readLevelFile f = do
        c  <- readFile f
        let (lvlString, lvlMetadata)    = splitLevelAndMetadataString . lines $ c
        -- Process metadata
        let lBeardGrowthRate            = extractBeardGrowthRateFromMetadata    (leBeardGrowthRate defaultLevelValues)  lvlMetadata
        let objectInitVals              = ObjectInitValues { oiBeardGrowthRate = lBeardGrowthRate }
        -- Process level-string
        let lMap                        = levelStringToMap objectInitVals lvlString
        return Level { lvMap            = lMap
                     , lvTrampolines    = extractTrampolinesFromMetadata        empty                                   lvlMetadata
                     , lvGrowthRate     = lBeardGrowthRate
                     , lvRazors         = extractRazorsFromMetadata             (leRazors          defaultLevelValues)  lvlMetadata
                     , lvFlooding       = extractFloodingFromMetadata           (leFlooding        defaultLevelValues)  lvlMetadata
                     , lvWater          = extractWaterFromMetadata              (leWater           defaultLevelValues)  lvlMetadata
                     , lvWaterproof     = extractWaterproofFromMetadata         (leWaterproof      defaultLevelValues)  lvlMetadata
                     , lvLambdas        = M.size . M.filter isLambdaLike $ lMap }
        where
        splitLevelAndMetadataString :: [String] -> ([String], [String])
        splitLevelAndMetadataString s = (tFst, P.filter (/= "") tSnd)
                where (tFst, tSnd) = span (/= "") s
        
        
-- TODO: quick and dirty parsing :(
extractValueFromMetadata :: (line -> Bool) -> (value -> line -> value) -> value -> [line] -> value
extractValueFromMetadata lineCondition extractFromLine = foldl (\d l -> if lineCondition l then extractFromLine d l else d )

extractBeardGrowthRateFromMetadata :: Int -> [String] -> Int
extractBeardGrowthRateFromMetadata  = extractValueFromMetadata ("Growth " `isPrefixOf`) (\_ -> read . P.filter isDigit)

extractRazorsFromMetadata :: Int -> [String] -> Int
extractRazorsFromMetadata  = extractValueFromMetadata ("Razors " `isPrefixOf`) (\_ -> read . P.filter isDigit)

extractWaterFromMetadata :: Int -> [String] -> Int
extractWaterFromMetadata  = extractValueFromMetadata ("Water " `isPrefixOf`) (\_ -> read . P.filter isDigit)

extractFloodingFromMetadata :: Int -> [String] -> Int
extractFloodingFromMetadata  = extractValueFromMetadata ("Flooding " `isPrefixOf`) (\_ -> read . P.filter isDigit)

extractWaterproofFromMetadata :: Int -> [String] -> Int
extractWaterproofFromMetadata  = extractValueFromMetadata ("Waterproof " `isPrefixOf`) (\_ -> read . P.filter isDigit)

extractTrampolinesFromMetadata :: Map Object Object -> [String] -> Map Object Object
extractTrampolinesFromMetadata = extractValueFromMetadata ("Trampoline " `isPrefixOf`) extract
        where
        extract m l = insert (Trampoline trampo) (Target target) m
                where (trampo:target:_) = P.filter (`elem` ['A' .. 'I'] ++ ['0'..'9']) l


levelStringToMap :: ObjectInitValues -> [String] -> LevelMap
levelStringToMap oiv sl = fromList . concatMap (\(y, s) -> zip [(x,y) | x <- [1..]] (map (charToObject oiv) s)) $ zip [1..] (reverse sl)