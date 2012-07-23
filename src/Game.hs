module Game ( Object(..), Position, LevelMap, Level(..), GameProgress(..), GameState(..)
            , isTrampoline, isTarget, isBeard
            , charToObject, objectToChar, objectColor, printLevel
            , ObjectInitValues(..) )
where

import Data.List (sortBy)
import Data.Map as M (Map, null, toList, insert)
import Control.Monad
import System.Console.ANSI ( setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..) )

-- Data structures

data Object
        = Robot
        | Wall
        | Rock
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

        
type Position = (Int, Int)
type LevelMap = Map Position Object

data Level = Level
        { lvMap         :: LevelMap
        , lvTrampolines :: Map Object Object -- Trampoline -> Target
        , lvGrowthRate  :: Int
        , lvRazors      :: Int
        }


data GameProgress
        = Running
        | Win
        | Loss
        | Abort
        | Restart
        | Skip
        deriving Eq


data GameState = GameState
        { gsLevel               :: Level
        , gsLevelDimensions     :: Position -- TODO: necessary?
        , gsRobotPosition       :: Position
        , gsLiftPosition        :: Position
        
        , gsTargets             :: Map Object Position   -- Target -> Position of Target
        , gsTargetSources       :: Map Object [Position] -- Target -> [Position of Trampoline]
        
        , gsProgress            :: GameProgress
        , gsLambdasCollected    :: Int
        , gsMoves               :: Int
        }


data ObjectInitValues = ObjectInitValues
        { oiBeardGrowthRate     :: Int
        }


instance Show Object where
        show = (:[]). objectToChar

-- Functions


isTrampoline :: Object -> Bool
isTrampoline (Trampoline _)     = True
isTrampoline _                  = False

isTarget :: Object -> Bool
isTarget (Target _)             = True
isTarget _                      = False

isBeard :: Object -> Bool
isBeard (Beard _)              = True
isBeard _                      = False

objectToChar :: Object -> Char
objectToChar o
        = case o of
                Robot           -> 'R'
                Wall            -> '#'
                Rock            -> '*' 
                Lambda          -> '\\'
                LiftClosed      -> 'L'
                LiftOpen        -> 'O'
                Earth           -> '.'
                Trampoline c    -> c
                Target c        -> c
                Beard _         -> 'W'
                Razor           -> '!'
                Empty           -> ' '


charToObject :: ObjectInitValues -> Char -> Object
charToObject oiv c
        = case c of
                'R'                     -> Robot
                '#'                     -> Wall
                '*'                     -> Rock
                '\\'                    -> Lambda
                'L'                     -> LiftClosed
                'O'                     -> LiftOpen
                '.'                     -> Earth
                ' '                     -> Empty
                a | a `elem` ['A'..'I'] -> Trampoline a
                  | a `elem` ['0'..'9'] -> Target a
                'W'                     -> Beard $ oiBeardGrowthRate oiv
                '!'                     -> Razor
                a   -> error $ "Cannot convert \"" ++ a : "\" to Object: no mapping found" ++ [a] ++ "aa"


objectColor :: Object -> [SGR]
objectColor o
        = case o of
                Robot           -> return $ SetColor Foreground Dull Blue
                Wall            -> []
                Rock            -> return $ SetColor Foreground Vivid Red 
                Lambda          -> return $ SetColor Foreground Dull Cyan
                LiftClosed      -> return $ SetColor Foreground Dull Green
                LiftOpen        -> return $ SetColor Foreground Vivid Green
                Earth           -> []
                Trampoline _    -> return $ SetColor Foreground Vivid Magenta
                Target _        -> return $ SetColor Foreground Dull Magenta
                Beard _         -> []
                Razor           -> []
                Empty           -> []


-- Print functions for data structures

printLevel :: GameState -> IO ()
printLevel gs = do
        unless (M.null trams) $ do
                putStrLn "Trampolines:"
                mapM_ (putStrLn . show') $ toList trams
        printLevelMap l
        where
        l = (gsLevel gs) { lvMap = insert (gsRobotPosition gs) Robot (lvMap . gsLevel $ gs) }
        trams = lvTrampolines l
        show' (tram, targ) = show tram ++ " -> " ++ show targ


printLevelMap :: Level -> IO ()
printLevelMap l = (sequence_ . printAList . levelToSortedAList) l >> setSGR [ Reset ]
        where
        print' o = printNoNl' o >> putStrLn ""
        printNoNl' o = do
                setSGR (objectColor o)
                putChar . objectToChar $ o
        
        printAList :: [(Position, Object)] -> [IO ()]
        printAList ls@(((x0,_),o):((x1,_),_):_)
                | x1 < x0 = print' o : (printAList . tail) ls
                | otherwise = printNoNl' o : (printAList . tail) ls
        printAList (((_,_),o):xs) = printNoNl' o : printAList xs
        printAList [] = [putStrLn ""]

        levelToSortedAList :: Level -> [(Position, Object)]
        levelToSortedAList = sortBy levelMapOutputSort . toList . lvMap
        
        levelMapOutputSort ((x0,y0),_) ((x1,y1),_)
                | y0 < y1       = GT
                | x0 > x1       = GT
                | otherwise     = LT