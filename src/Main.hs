module Main where

import Prelude          as P hiding ( lookup )
import Data.Map         as M ( toList, keys, size, filter, lookup, insert, elemAt, foldr', delete )
import Control.Monad.State
import Control.Concurrent ( threadDelay )
import System.IO ( BufferMode(NoBuffering), stdin, hSetBuffering, hSetEcho )
import System.Environment ( getArgs )
import System.Console.ANSI (clearScreen, hideCursor, showCursor)

import Utils
import Game
import Input
import Persistence

{--
TODOs: 
 - add (global) scoring
 - add feature to save the game
        - probably needed: currentLevel(Map, Trampolines) + RobotPos
        - extra: score/stats, loaded maps / finished maps
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
                { gsLevel               = lvl { lvMap = insert roboPos Empty (lvMap lvl)} -- delete robot start position 
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
        printLevel game
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
                                                printLevel game'
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
                Empty   |  dir == UiUseRazor
                    && lvRazors lvl > 0
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insertIntoAdjacentNonBeardCells nrp Empty lmap
                                                                           , lvRazors = lvRazors lvl - 1}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                }
                Empty           -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Earth           -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsRobotPosition = nrp, gsMoves = gsMoves game + 1}
                Lambda          -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsLambdasCollected = gsLambdasCollected game + 1
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                LiftOpen        -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsProgress = Win
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == UiRight
                        && lookup (rX+2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert (rX+2, rY) Rock . insert nrp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == UiLeft
                        && lookup (rX-2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert (rX-2, rY) Rock . insert nrp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                tc@(Trampoline _)
                                -> return game  { gsLevel = (gsLevel game) { lvMap = removeTargetIfNoOtherTrampsTargetIt . insert nrp Empty $ lmap
                                                                           , lvTrampolines = lvlTrampsNew} -- TODO: update 2 remaining maps?
                                                , gsRobotPosition = roboPosTrampoline
                                                , gsMoves = gsMoves game + 1
                                                }
                        where
                        lvlTramps = lvTrampolines lvl
                        lvlTrampsNew = delete tc lvlTramps
                        target = unMaybe . lookup tc $ lvlTramps
                        removeTargetIfNoOtherTrampsTargetIt = if (>=1) . size . M.filter (==target) $ lvlTrampsNew then id else insert roboPosTrampoline Empty
                        roboPosTrampoline = unMaybe $ do
                                targ <- lookup tc lvlTramps
                                lookup targ (gsTargets game)
                
                Razor
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap
                                                                           , lvRazors = lvRazors lvl + 1}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                }
                                                
                _   | dir == UiWait
                                -> return game
                _   | dir == UiAbort
                                -> return game  { gsProgress = Abort}
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
        (rX, rY) = gsRobotPosition game
        
        -- TODO: code duplication :( see below -> insertIntoAdjacentCells
        insertIntoAdjacentNonBeardCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentNonBeardCells (x,y) obj lvlMap = insertObjectIntoPositions obj lvlMap (adjacentPositions x y)
        
        insertObjectIntoPositions ::  Object -> LevelMap -> [Position] -> LevelMap 
        insertObjectIntoPositions obj = foldl (flip (flip insertIfBeard obj))
        
        insertIfBeard :: Position -> Object -> LevelMap -> LevelMap
        insertIfBeard (x,y) obj lmap = if isBeard' $ lookup (x,y) lmap then insert (x,y) obj lmap else lmap
        isBeard' = maybe False isBeard
        

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
                = case lookup pos lvlMap of
                        Nothing -> g'
                        Just e -> processObject g g' e pos
                where lvlMap = insert (gsRobotPosition gs) Robot . lvMap . gsLevel $ g


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
                Beard g | g > 0
                        -> (gsLevel gs') { lvMap = insert (x, y) (Beard $ g-1) lvl'}
                Beard _
                        -> (gsLevel gs') { lvMap = insert (x,y) beardInit . insertIntoAdjacentCells (x, y) beardInit $ lvl'}
                _       -> (gsLevel gs') { lvMap = lvl'}
                }
        where
        lvl  = insert (gsRobotPosition gs) Robot . lvMap . gsLevel $ gs
        lvl' = lvMap . gsLevel $ gs'
        beardInit = Beard $ (lvGrowthRate . gsLevel) gs -1
        
        insertIntoAdjacentCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentCells (x',y') obj lvlMap = insertObjectIntoPositions obj lvlMap (adjacentPositions x' y')
        
        insertObjectIntoPositions ::  Object -> LevelMap -> [Position] -> LevelMap 
        insertObjectIntoPositions obj = foldl (flip (flip insertIfEmpty obj))
        
        insertIfEmpty :: Position -> Object -> LevelMap -> LevelMap
        insertIfEmpty (x',y') obj lmap = if lookup (x',y') lvl == Just Empty then insert (x',y') obj lmap else lmap -- note that lvl not lmap is used


adjacentPositions :: Int -> Int -> [(Int,Int)]
adjacentPositions x y = [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1]]


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