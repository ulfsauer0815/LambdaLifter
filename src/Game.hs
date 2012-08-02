module Game ( Object(..), RockType(..), Position, LevelMap, Level(..), GameProgress(..), LossReason(..), GameState(..)
            , isTrampoline, isTarget, isBeard, isRock, isLambda, isHigherOrderRock, isSimpleRock
            , isLambdaLike, isEmpty, isWall, isEarth, isLiftOpen, isLiftClosed, isRazor
            , charToObject, objectToChar, objectColor, printLevel
            , sortForTraversal
            , ObjectInitValues(..), LevelValues(..), defaultLevelValues)
where

import Data.List (sortBy)
import Data.Map as M (Map, null, toList, insert)
import Control.Monad
import System.Console.ANSI ( setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..) )

import Input ( UserInput )

-- Data structures

data Object
        = Robot
        | Wall
        | Rock RockType
        | Lambda
        | LiftOpen
        | LiftClosed
        | Earth
        | Trampoline Char
        | Target Char
        | Beard Int
        | Razor
        | Empty
        deriving (Eq, Ord)


data RockType
        = Simple
        | HigherOrder
        deriving (Eq, Ord)


        
type Position = (Int, Int)
type LevelMap = Map Position Object

data Level = Level
        { lvMap         :: LevelMap
        , lvTrampolines :: Map Object Object -- Trampoline -> Target
        , lvGrowthRate  :: Int
        , lvRazors      :: Int
        , lvLambdas     :: Int
        , lvWater       :: Int
        , lvFlooding    :: Int
        , lvWaterproof  :: Int
        }

data LevelValues = LevelValues
        { leBeardGrowthRate     :: Int
        , leRazors              :: Int
        , leFlooding            :: Int
        , leWater               :: Int
        , leWaterproof          :: Int
        }

defaultLevelValues :: LevelValues
defaultLevelValues = LevelValues
        { leBeardGrowthRate     = 25
        , leRazors              =  0
        , leFlooding            =  0
        , leWater               =  0
        , leWaterproof          = 10
        }


data GameProgress
        = Running
        | Win
        | Loss LossReason
        | Abort
        | Restart
        | Skip
        deriving Eq


data LossReason
        = FallingRock
        | Drowning
        deriving Eq


data GameState = GameState
        { gsLevel               :: Level
        , gsRobotPosition       :: Position
        , gsLiftPosition        :: Position
        , gsTick                :: Int
        , gsAirLeft             :: Int
        
        , gsTargets             :: Map Object Position   -- Target -> Position of Target
        , gsTargetSources       :: Map Object [Position] -- Target -> [Position of Trampoline]
        
        , gsProgress            :: GameProgress
        , gsLambdasCollected    :: Int
        , gsMoves               :: Int
        , gsMoveHistory         :: [UserInput]
        }


data ObjectInitValues = ObjectInitValues
        { oiBeardGrowthRate     :: Int
        }


instance Show Object where
        show = (:[]). objectToChar

-- Functions

isEmpty :: Object -> Bool
isEmpty Empty                   = True
isEmpty _                       = False

isWall :: Object -> Bool
isWall Wall                     = True
isWall _                        = False

isEarth :: Object -> Bool
isEarth Earth                   = True
isEarth _                       = False

isLiftOpen :: Object -> Bool
isLiftOpen LiftOpen             = True
isLiftOpen _                    = False

isLiftClosed :: Object -> Bool
isLiftClosed LiftClosed         = True
isLiftClosed _                  = False

isTrampoline :: Object -> Bool
isTrampoline (Trampoline _)     = True
isTrampoline _                  = False

isTarget :: Object -> Bool
isTarget (Target _)             = True
isTarget _                      = False

isBeard :: Object -> Bool
isBeard (Beard _)               = True
isBeard _                       = False

isRazor :: Object -> Bool
isRazor Razor                   = True
isRazor _                       = False

isRock :: Object -> Bool
isRock (Rock _)                 = True
isRock _                        = False

isLambda :: Object -> Bool
isLambda Lambda                 = True
isLambda _                      = False

isHigherOrderRock :: Object -> Bool
isHigherOrderRock (Rock HigherOrder)    = True
isHigherOrderRock _                     = False

isLambdaLike :: Object -> Bool
isLambdaLike o = isLambda o || isHigherOrderRock o

isSimpleRock :: Object -> Bool
isSimpleRock (Rock Simple)              = True
isSimpleRock _                          = False


objectToChar :: Object -> Char
objectToChar o
        = case o of
                Robot           -> 'R'
                Wall            -> '#'
                Rock Simple     -> '*'
                Rock HigherOrder-> '@'
                Lambda          -> '\\'
                LiftClosed      -> 'L'
                LiftOpen        -> 'O'
                Earth           -> '.'
                Trampoline c    -> c
                Target c        -> c
                Beard _         -> 'W'
                Razor           -> '!'
                Empty           -> ' '


charToObject :: ObjectInitValues -> Char -> Maybe Object
charToObject oiv c
        = case c of
                'R'                     -> return Robot
                '#'                     -> return Wall
                '*'                     -> return $ Rock Simple
                '@'                     -> return $ Rock HigherOrder
                '\\'                    -> return Lambda
                'L'                     -> return LiftClosed
                'O'                     -> return LiftOpen
                '.'                     -> return Earth
                ' '                     -> return Empty
                a | a `elem` ['A'..'I'] -> return $ Trampoline a
                  | a `elem` ['0'..'9'] -> return $ Target a
                'W'                     -> return $ Beard $ oiBeardGrowthRate oiv
                '!'                     -> return Razor
                _                       -> Nothing


objectColor :: Object -> [SGR]
objectColor o
        = case o of
                Robot           -> return $ SetColor Foreground Dull Blue
                Wall            -> []
                Rock _          -> return $ SetColor Foreground Vivid Red
                Lambda          -> return $ SetColor Foreground Dull Cyan
                LiftClosed      -> return $ SetColor Foreground Dull Green
                LiftOpen        -> return $ SetColor Foreground Vivid Green
                Earth           -> []
                Trampoline _    -> return $ SetColor Foreground Vivid Magenta
                Target _        -> return $ SetColor Foreground Dull Magenta
                Beard _         -> []
                Razor           -> []
                Empty           -> []


waterColor :: [SGR]
waterColor = return $ SetColor Foreground Vivid Blue

-- Print functions for data structures

printLevel :: GameState -> IO ()
printLevel gs = do
        unless (M.null trams) $ do
                putStrLn "Trampolines:"
                mapM_ (putStrLn . show') $ toList trams
        when (airLeft < lvWaterproof l) $
                putStrLn $ "Air: " ++ show airLeft
        when (razors > 0) $
                putStrLn $ "Razors: " ++ show razors
        printLevelMap l
        where
        l                       = (gsLevel gs) { lvMap = insert (gsRobotPosition gs) Robot (lvMap . gsLevel $ gs) }
        trams                   = lvTrampolines l
        airLeft                 = gsAirLeft gs
        razors                  = lvRazors l
        show' (tram, targ)      = show tram ++ " -> " ++ show targ


printLevelMap :: Level -> IO ()
printLevelMap l = (sequence_ . printAList . levelToSortedAList) l >> setSGR [ Reset ]
        where
        print' o y = printNoNl' o y >> putStrLn ""
        printNoNl' o y = do
                if y > lvWater l then setSGR (objectColor o) else setSGR waterColor 
                putChar . objectToChar $ o
        
        printAList :: [(Position, Object)] -> [IO ()]
        printAList ls@(((x0,y0),o):((x1,_),_):_)
                | x1 < x0 = print' o y0 : (printAList . tail) ls
                | otherwise = printNoNl' o y0 : (printAList . tail) ls
        printAList (((_,y0),o):xs) = printNoNl' o y0 : printAList xs
        printAList [] = [putStrLn ""]

        levelToSortedAList :: Level -> [(Position, Object)]
        levelToSortedAList = sortBy levelMapOutputSort . toList . lvMap
        
        levelMapOutputSort (pos0,_) (pos1,_) = compareForLevelOutput pos0 pos1


compareForLevelOutput :: Position -> Position -> Ordering
compareForLevelOutput (x0,y0) (x1,y1)
        | y0 < y1       = GT
        | y0 > y1       = LT
        | x0 > x1       = GT
        | x0 < x1       = LT
        | otherwise     = EQ


compareForLevelTraversal :: Position -> Position -> Ordering
compareForLevelTraversal (x0,y0) (x1,y1)
        | y0 < y1       = LT
        | y0 > y1       = GT
        | x0 < x1       = LT
        | x0 > x1       = GT
        | otherwise     = EQ


sortForTraversal :: [Position] -> [Position]
sortForTraversal = sortBy compareForLevelTraversal