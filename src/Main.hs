module Main where

import Prelude          as P hiding ( lookup )
import Data.Map         as M ( toList, keys, size, filter, lookup, insert, elemAt, foldr' )
import Control.Monad.State
import Control.Concurrent ( threadDelay )
import System.IO ( BufferMode(NoBuffering), stdin, hSetBuffering, hSetEcho )
import System.Environment ( getArgs )
import System.Console.ANSI --(clearScreen, hideCursor, showCursor)

import Utils
import Game
import Input
import Persistence

{--
TODOs: 
 - add (global) scoring
 - use state monad
--}



data Delays = Delays
        { deMapUpdate          :: Int
        }


defaultDelays :: Delays
defaultDelays = Delays
        { deMapUpdate          = 12500
        }


-- Functions

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
                
                , gsTargets             = revertMap targs
                , gsTargetSources       = targetSourcePositions
                
                , gsProgress            = Running
                , gsLambdasCollected    = 0
                , gsMoves               = 0
                }
        where
        lmap   = lvMap lvl
        robos = M.filter (== Robot)      lmap
        trams = M.filter isTrampoline    lmap
        targs = M.filter isTarget        lmap
        lifts = M.filter (== LiftClosed) lmap
        lvlDims = (maximum xs, maximum ys)
        (xs,ys) = unzip . keys $ lmap
        isTrampoline (Trampoline _)     = True
        isTrampoline _                  = False
        isTarget (Target _)             = True
        isTarget _                      = True
        targetSourcePositions = listMap . filterMaybeTuples1 . map (\(pos,obj) -> (pos, lookup obj (revertMap trams)) ) . toList $ lvTrampolines lvl
        filterMaybeTuples1 ls = [(x,y) | (x, Just y) <- ls]

       
startGames :: Delays -> [GameState] -> IO ()
startGames _ [] = putStrLn "You finished all levels! :)"
startGames delays games@(game:nextGames) = do
        game' <- playGame delays game
        
        putStrLn $ "Lambdas collected: " ++ show (gsLambdasCollected game')
        putStrLn $ "Moves: "             ++ show (gsMoves game')
        
        case gsProgress game' of
                Restart   -> restartLevel
                Loss      -> do
                                putStrLn "You got crushed by rocks! :("
                                askForContinue_ restartLevel
                Win       -> do
                                putStrLn "You won! Congratulations!"
                                askForContinue_ (startGames delays nextGames)
                Skip      -> startGames delays (nextGames ++ [game])
                Abort     -> putStrLn "You abandoned Marvin! :'("
                Running   -> error "Invalid state"
        where
        restartLevel = startGames delays games


playGame :: Delays -> GameState -> IO GameState
playGame delays game = do
        clearScreen
        printLevel . gsLevel $ game
        if gsProgress game == Running
          then do
                dir <- getInput
                case dir of
                        UiAbort         -> return game {gsProgress = Abort}
                        UiRestart       -> return game {gsProgress = Restart}
                        UiSkip          -> return game {gsProgress = Skip}
                        _               -> 
                                case moveRobot game dir of
                                        Nothing   -> playGame delays game
                                        Just game' -> do
                                                clearScreen
                                                printLevel . gsLevel $ game'
                                                let game''  = updateGameState game'
                                                let game''' = checkIfRobotGotCrushed game' game''
                                                threadDelay $ deMapUpdate delays
                                                playGame delays game'''
          else return game


moveRobot :: GameState -> UserInput -> Maybe GameState -- TODO: Maybe unnecessary
moveRobot game dir = do
        let lvl = gsLevel game
        nrp   <- newRobotPosition
        field <- lookup nrp $ lvMap lvl
        let lmap = lvMap lvl
        case field of
                Robot   | dir == UiWait
                                -> return game
                Robot   | dir == UiAbort
                                -> return game  { gsProgress = Abort}
                Empty           -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert orp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Earth           -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert orp Empty $ lmap}
                                                , gsRobotPosition = nrp, gsMoves = gsMoves game + 1}
                Lambda          -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert orp Empty $ lmap}
                                                , gsLambdasCollected = gsLambdasCollected game + 1
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                LiftOpen        -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert orp Empty $ lmap}
                                                , gsProgress = Win
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == UiRight
                        && lookup (rX+2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert (rX+2, rY) Rock . insert orp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == UiLeft
                        && lookup (rX-2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert (rX-2, rY) Rock . insert orp Empty  $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                tc@(Trampoline _)
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert roboPosTrampoline Robot . insert nrp Empty. insert orp Empty $ lmap}
                                                , gsRobotPosition = roboPosTrampoline
                                                , gsMoves = gsMoves game + 1}
                        where
                        roboPosTrampoline = unMaybe $ do
                                targ <- lookup tc (lvTrampolines (gsLevel game))
                                lookup targ (gsTargets game)
                _               -> Nothing
        where
        unMaybe (Just a) = a
        unMaybe _        = error "unexpected Nothing value -> Trampoline may not have a Target" -- TODO: better error handling
        newRobotPosition= case dir of
                UiUp            -> Just (rX   ,rY+1)
                UiLeft          -> Just (rX-1 ,rY  )
                UiDown          -> Just (rX   ,rY-1)
                UiRight         -> Just (rX+1 ,rY  )
                _               -> Just (rX   ,rY  ) -- use Nothing to produce an invalid GameState
        orp@(rX, rY) = gsRobotPosition game


checkIfRobotGotCrushed :: GameState -> GameState -> GameState
checkIfRobotGotCrushed oldGs newGs
        = if aboveOld /= Just Rock && aboveNew == Just Rock
            then
                newGs {gsProgress = Loss}
            else
                newGs
        where
        aboveOld        = lookup (rX,rY+1) $ lvMap . gsLevel $ oldGs
        aboveNew        = lookup (rX,rY+1) $ lvMap . gsLevel $ newGs
        (rX,rY)         = gsRobotPosition oldGs


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
        updateLevelByPosition g g' pos
                = case lookup pos (lvMap . gsLevel $ g) of
                        Nothing -> g'
                        Just e -> processObject g g' e pos


processObject :: GameState -> GameState -> Object -> Position -> GameState
processObject gs gs' o (x,y)
        = gs'   { gsLevel = case o of
                Rock    | lookup (x, y-1)     lvl == Just Empty
                        -> (gsLevel gs') { lvMap = insert (x, y-1) Rock . insert (x,y) Empty $ lvl'}
                Rock    | lookup (x, y-1)     lvl == Just Rock &&
                          lookup (x+1, y)     lvl == Just Empty &&
                          lookup (x+1, y-1)   lvl == Just Empty
                        -> (gsLevel gs') { lvMap = insert (x+1, y-1) Rock . insert (x,y) Empty $ lvl'}
                Rock    | lookup (x, y-1)     lvl == Just Rock &&
                          ( lookup (x+1, y)   lvl /= Just Empty ||
                            lookup (x+1, y-1) lvl /= Just Empty
                          ) &&
                          lookup (x-1, y)     lvl == Just Empty &&
                          lookup (x-1, y-1)   lvl == Just Empty
                        -> (gsLevel gs') { lvMap = insert (x-1, y-1) Rock . insert (x,y) Empty $ lvl'}
                Rock    | lookup (x, y-1)     lvl == Just Lambda &&
                          lookup (x+1, y)     lvl == Just Empty &&
                          lookup (x+1, y-1)   lvl == Just Empty
                        -> (gsLevel gs') { lvMap = insert (x+1, y-1) Rock . insert (x,y) Empty $ lvl'}
                LiftClosed | foldr' ((&&) . (/=Lambda)) True lvl
                        -> (gsLevel gs') { lvMap = insert (x,y) LiftOpen lvl'}
                _       -> (gsLevel gs') { lvMap = lvl'}
                }
        where
        lvl  = lvMap . gsLevel $ gs
        lvl' = lvMap . gsLevel $ gs'


-- Main

main :: IO ()
main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hideCursor
            
        args <- getArgs
        lvls  <- mapM readLevelFile args
        let gamesM = mapM createGame lvls
        case gamesM of
                Nothing -> putStrLn "Error loading levels..." -- TODO: verbose error msgs
                Just gs -> do
                        clearScreen
                        putStrLn "Welcome to LambdaLifter (alpha)"
                        putStrLn ""
                        printControls
                        askForContinue_ (startGames delays gs)
        
        showCursor
        where
        delays = defaultDelays