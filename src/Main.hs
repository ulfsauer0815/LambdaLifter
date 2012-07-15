module Main where

import Prelude hiding (lookup, filter)
import Data.List hiding (lookup, insert, filter)
import Data.Map hiding (map)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import System.IO (BufferMode(NoBuffering),stdin, hSetBuffering, hSetEcho)
import System.Environment (getArgs)
import Data.Char (toLower)

{--
TODOs: 
 - use state monad
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
        | MvRestart
        deriving Eq

data GameProgress
        = Running
        | Win
        | Loss
        | Abort
        | Restart
        deriving Eq


data GameState = GameState
        { gsLevel               :: Level
        , gsLevelDimensions     :: Position -- TODO: necessary?
        , gsRobotPosition       :: Position
        , gsLiftPosition        :: Position
        
        , gsProgress            :: GameProgress
        , gsLambdasCollected    :: Integer
        , gsMoves               :: Integer
        }


-- Functions

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
                Empty           -> ' ' -- TODO: eww? see note below


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
                _    -> error $ "Cannot convert \"" ++ c : "\" to Object: no mapping found"


readLevelFromFile :: FilePath -> IO Level
readLevelFromFile f = do
        c  <- readFile f
        return . levelStringToMap . lines $ c


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


createGame :: Level -> Maybe GameState
createGame lvl = do
        roboPos <- if size robos == 1
                     then return . fst . elemAt 0 $ robos
                     else Nothing
        liftPos <- if size lifts == 1
                     then return . fst . elemAt 0 $ lifts
                     else Nothing
        return GameState
                { gsLevel               = lvl
                , gsLevelDimensions     = lvlDims
                , gsRobotPosition       = roboPos
                , gsLiftPosition        = liftPos
                
                , gsProgress            = Running
                , gsLambdasCollected    = 0
                , gsMoves               = 0
                }
        where
        robos = filter (== Robot)      lvl
        lifts = filter (== LiftClosed) lvl
        lvlDims = (maximum xs, maximum ys)
        (xs,ys) = unzip . keys $ lvl


updateGameState :: GameState -> GameState
updateGameState gs = execState (updateLevel' keysToUpdate gs) gs
        where
        updateLevel' :: [Position] -> GameState -> State GameState GameState
        updateLevel' [] _ = get
        updateLevel' (pos:poss) lvl = do
                modify $ flip (updateLevelByPosition lvl) pos
                updateLevel' poss lvl
        keysToUpdate = [(x,y) | y <- [1..maxY], x <- [1..maxX]] -- TODO: ewww, wenn optimiert
        (maxX, maxY) = gsLevelDimensions gs

       
updateLevelByPosition :: GameState -> GameState -> Position -> GameState
updateLevelByPosition gs gs' pos
        = case lookup pos (gsLevel gs) of
                Nothing -> gs'
                Just e -> processObject gs gs' e pos


processObject :: GameState -> GameState -> Object -> Position -> GameState
processObject gs gs' o (x,y)
        = gs'   { gsLevel = case o of
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
                }
        where
        lvl = gsLevel gs
        lvl' = gsLevel gs'


playGame :: Int -> GameState -> IO GameState
playGame updateDelay game = do
        printLevel . gsLevel $ game
        if gsProgress game == Running
          then do
                dir <- getInput
                case dir of
                        MvAbort         -> return game {gsProgress = Abort}
                        MvRestart       -> return game {gsProgress = Restart}
                        _               -> 
                                case moveRobot game dir of
                                        Nothing   -> playGame updateDelay game
                                        Just game' -> do
                                                printLevel . gsLevel $ game'
                                                let game''  = updateGameState game'
                                                let game''' = checkIfRobotGotCrushed game' game''
                                                threadDelay updateDelay
                                                playGame updateDelay game'''
          else return game


checkIfRobotGotCrushed :: GameState -> GameState -> GameState
checkIfRobotGotCrushed oldGs newGs
        = if aboveOld /= Just Rock && aboveNew == Just Rock
            then
                newGs {gsProgress = Loss}
            else
                newGs
        where
        aboveOld        = lookup (rX,rY+1) $ gsLevel oldGs
        aboveNew        = lookup (rX,rY+1) $ gsLevel newGs
        (rX,rY)         = gsRobotPosition oldGs


getInput :: IO Movement
getInput = liftM processInput getChar


processInput :: Char -> Movement
processInput c = case toLower c of
        'w' -> MvUp
        'a' -> MvLeft
        's' -> MvDown
        'd' -> MvRight
        'q' -> MvAbort
        'r' -> MvRestart
        _   -> MvWait


moveRobot :: GameState -> Movement -> Maybe GameState -- TODO: Maybe unnecessary
moveRobot game dir = do
        let lvl = gsLevel game
        nrp   <- newRobotPosition
        field <- lookup nrp lvl
        case field of
                Robot   | dir == MvWait
                                -> return game
                Robot   | dir == MvAbort
                                -> return game  { gsProgress = Abort}
                Empty           -> return game  { gsLevel = insert nrp Robot . insert orp Empty $ lvl
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Earth           -> return game  { gsLevel = insert nrp Robot . insert orp Empty $ lvl
                                                , gsRobotPosition = nrp, gsMoves = gsMoves game + 1}
                Lambda          -> return game  { gsLevel = insert nrp Robot . insert orp Empty $ lvl
                                                , gsLambdasCollected = gsLambdasCollected game + 1
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                LiftOpen        -> return game  { gsLevel = insert nrp Robot . insert orp Empty $ lvl
                                                , gsProgress = Win
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == MvRight
                        && lookup (rX+2, rY) lvl == Just Empty
                                -> return game  { gsLevel = insert nrp Robot . insert (rX+2, rY) Rock . insert orp Empty $ lvl
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == MvLeft
                        && lookup (rX-2, rY) lvl == Just Empty
                                -> return game  { gsLevel = insert nrp Robot . insert (rX-2, rY) Rock . insert orp Empty  $ lvl
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                _               -> Nothing
        where
        newRobotPosition= case dir of
                MvUp            -> Just (rX   ,rY+1)
                MvLeft          -> Just (rX-1 ,rY  )
                MvDown          -> Just (rX   ,rY-1)
                MvRight         -> Just (rX+1 ,rY  )
                MvWait          -> Just (rX   ,rY  )
                MvRestart       -> Just (rX   ,rY  )
                MvAbort         -> Just (rX   ,rY  ) -- use Nothing to produce an invalid GameState
        orp@(rX, rY) = gsRobotPosition game


startGame :: Int -> GameState -> IO ()
startGame updateDelay game = do
        game' <- playGame updateDelay game
        
        putStrLn $ "Lambdas collected: " ++ show (gsLambdasCollected game')
        putStrLn $ "Moves: "             ++ show (gsMoves game')
        
        case gsProgress game' of
                Restart   -> startGame updateDelay game
                Loss      -> do
                                putStrLn "You got crushed by rocks! :("
                                putStrLn "Press r to restart"
                                mv <- getInput
                                when (mv == MvRestart) $
                                        startGame updateDelay game
                Win       -> putStrLn "You won! Congratulations!"
                Abort     -> putStrLn "You abandoned Marvin! :'("
                Running   -> error "Invalid state"
                

-- Main

main :: IO ()
main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        
        args <- getArgs
        lvl  <- readLevelFromFile . concat $ args
        let game = createGame lvl
        maybe (putStrLn "Invalid level") (startGame updateDelay) game
         
        where
        updateDelay = 125000