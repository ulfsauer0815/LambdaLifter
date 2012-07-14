module Main where

import Prelude hiding (lookup, filter)
import Data.List hiding (lookup, insert, filter)
import Data.Map hiding (map)
import Control.Monad
import Control.Monad.State
import Control.Concurrent (threadDelay)
import System.IO (BufferMode(NoBuffering),stdin, hSetBuffering, hSetEcho)

{--
TODOs:
 - check ending conditions
 - read levels from files
 - validate levels (1 robot, 1 lift, ...)
 - use state monad (maybe GameSate = (Level, CollectedLambdas, isFinished, hasWon/isDead))
--}

-- Data structures

data Object
        = Robot
        | Wall
        | Rock
        | Lambda
        | LiftOpen
        | LiftClosed
        | Earth
        | Empty
        deriving Eq

instance Show Object where
        show = (:[]). objectToChar

type Position = (Integer, Integer)
type Level = Map Position Object


data Movement
        = MvLeft
        | MvRight
        | MvUp
        | MvDown
        | MvWait
        | MvAbort
        deriving Eq


-- Levels / Maps

lvl1s :: [String]
lvl1s =
        [ "######"
        , "#. *R#"
        , "#  \\.#"
        , "#\\ * #"
        , "L  .\\#"
        , "######" ]

lvl1 :: Level
lvl1 = levelStringToMap lvl1s

lvl0s :: [String]
lvl0s =
        [ "#* *#"
        , "#* *#"
        , "#####" ]

lvl0 :: Level
lvl0 = levelStringToMap lvl0s


-- Functions

objectToChar :: Object -> Char
objectToChar o
        = case o of
                Robot -> 'R'
                Wall -> '#'
                Rock -> '*' 
                Lambda -> '\\'
                LiftClosed -> 'L'
                LiftOpen -> 'O'
                Earth -> '.'
                Empty -> ' ' -- TODO: eww? see note below


charToObject :: Char -> Object
charToObject c
        = case c of
                'R'  -> Robot
                '#'  -> Wall
                '*'  -> Rock
                '\\' -> Lambda
                'L'  -> LiftClosed
                'O'  -> LiftOpen
                '.'  -> Earth
                ' '  -> Empty
                _    -> error "Invalid map sequence"


levelStringToMap :: [String] -> Level
levelStringToMap sl = fromList . concatMap (\(y, s) -> zip [(x,y) | x <- [1..]] (map charToObject s)) $ zip [1..] (reverse sl)


printLevel :: Level -> IO ()
printLevel = sequence_ . printAList . levelToSortedAList
        where
        printAList :: [(Position, Object)] -> [IO ()]
        printAList ls@(((x0,_),o):((x1,_),_):_)
                | x1 < x0 = print o : (printAList . tail) ls
                | otherwise = (putChar . objectToChar) o : (printAList . tail) ls
        printAList (((_,_),o):xs) = (putChar . objectToChar) o : printAList xs
        printAList [] = [putStrLn ""]

        levelToSortedAList :: Level -> [(Position, Object)]
        levelToSortedAList = sortBy levelMapOutputSort . toList
        
        levelMapOutputSort ((x0,y0),_) ((x1,y1),_)
                | y0 < y1       = GT
                | x0 > x1       = GT
                | otherwise     = LT


updateLevel :: Level -> Level
updateLevel l = execState (updateLevel' keysToUpdate l) l
        where
        updateLevel' :: [Position] -> Level -> State Level Level
        updateLevel' [] _ = get
        updateLevel' (pos:poss) lvl = do
                modify $ flip (updateLevelByPosition lvl) pos
                updateLevel' poss lvl
        keysToUpdate = [(x,y) | y <- [1..maxY], x <- [1..maxX]] -- TODO: ewww, wenn optimiert
        ((maxX, maxY), _) = findMax l

        
updateLevelByPosition :: Level -> Level -> Position -> Level
updateLevelByPosition lvl lvl' pos
        = case lookup pos lvl of
                Nothing -> lvl'
                Just e -> processObject lvl lvl' e pos


processObject :: Level -> Level -> Object -> Position -> Level
processObject lvl lvl' o (x,y)
        = case o of
                Rock    | lookup (x, y-1)     lvl == Just Empty
                        -> insert (x, y-1) Rock . insert (x,y) Empty $ lvl'
                Rock    | lookup (x, y-1)     lvl == Just Rock &&
                          lookup (x+1, y)     lvl == Just Empty &&
                          lookup (x+1, y-1)   lvl == Just Empty
                        -> insert (x+1, y-1) Rock . insert (x,y) Empty $ lvl'
                Rock    | lookup (x, y-1)     lvl == Just Rock &&
                          ( lookup (x+1, y)   lvl /= Just Empty ||
                            lookup (x+1, y-1) lvl /= Just Empty
                          ) &&
                          lookup (x-1, y)     lvl == Just Empty &&
                          lookup (x-1, y-1)   lvl == Just Empty
                        -> insert (x-1, y-1) Rock . insert (x,y) Empty $ lvl'
                Rock    | lookup (x, y-1)     lvl == Just Lambda &&
                          lookup (x+1, y)     lvl == Just Empty &&
                          lookup (x+1, y-1)   lvl == Just Empty
                        -> insert (x+1, y-1) Rock . insert (x,y) Empty $ lvl'
                LiftClosed | foldr' ((&&) . (/=Lambda)) True lvl
                        -> insert (x,y) LiftOpen lvl'
                _       -> lvl'


playLevel :: Level -> IO ()
playLevel lvl = do
        printLevel lvl
        dir <- getInput
        unless (dir == MvAbort) $
                case moveRobot lvl dir of
                        Nothing   -> playLevel $ updateLevel lvl
                        Just lvl' -> do
                                printLevel lvl'
                                let lvl'' = updateLevel lvl'
                                threadDelay 250000 -- TODO: softcode
                                playLevel lvl''


getInput :: IO Movement
getInput = do
        c <- getChar
        return $ processInput c


processInput :: Char -> Movement
processInput c = case c of
        'w' -> MvUp
        'a' -> MvLeft
        's' -> MvDown
        'd' -> MvRight
        'q' -> MvAbort
        _   -> MvWait


moveRobot :: Level -> Movement -> Maybe Level
moveRobot lvl dir = do
        nrp <- newRobotPosition
        field <- lookup nrp lvl
        case field of
                Robot           -> return lvl
                Empty           -> return . insert nrp Robot . insert orp Empty $ lvl
                Earth           -> return . insert nrp Robot . insert orp Empty $ lvl
                Lambda          -> return . insert nrp Robot . insert orp Empty $ lvl -- TODO: collect lambda
                LiftOpen        -> return . insert nrp Robot . insert orp Empty $ lvl
                Rock    |  dir == MvRight
                        && lookup (rX+2, rY) lvl == Just Empty
                                -> return . insert nrp Robot . insert (rX+2, rY) Rock . insert orp Empty $ lvl
                Rock    |  dir == MvLeft
                        && lookup (rX-2, rY) lvl == Just Empty
                                -> return . insert nrp Robot . insert (rX-2, rY) Rock . insert orp Empty  $ lvl
                _               -> Nothing
        where
        newRobotPosition= case dir of
                MvUp    -> Just (rX   ,rY+1)
                MvLeft  -> Just (rX-1 ,rY)
                MvDown  -> Just (rX   ,rY-1)
                MvRight -> Just (rX+1 ,rY)
                MvWait  -> Just (rX   ,rY)
                _       -> Nothing
        orp@(rX, rY) = fst . elemAt 0 . filter (== Robot) $ lvl -- TODO: eww and error-prone


-- Main

main :: IO ()
main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        playLevel lvl1