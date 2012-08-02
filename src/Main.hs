module Main where

import Prelude          as P hiding ( lookup )
import Data.Map         as M ( fromList, toList, keys, size, filter, lookup, insert, elemAt, delete, keys )
import Data.Maybe ( fromMaybe )
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
 - highscores
        - stats directory with map files containing the score and the corresponding moves 
 - add feature to save the game
        - probably needed: currentLevel(Map, Trampolines) + RobotPos
        - extra: score/stats, loaded maps / finished maps
 - use state monad
--}


data Delays = Delays
        { deMapUpdate           :: Int
        , deMove                :: Int
        }

defaultDelays :: Delays
defaultDelays = Delays
        { deMapUpdate           = 12500
        , deMove                = 30000
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
                , gsRobotPosition       = roboPos
                , gsLiftPosition        = liftPos
                , gsTick                = 0
                , gsAirLeft             = lvWaterproof lvl
                
                , gsTargets             = revertMap targs
                , gsTargetSources       = targetSourcePositions
                
                , gsProgress            = Running
                , gsLambdasCollected    = 0
                , gsMoves               = 0
                , gsMoveHistory         = [] -- head is the most recent move
                }
        where
        lmap   = lvMap lvl
        robos = M.filter (== Robot)      lmap
        trams = M.filter isTrampoline    lmap
        targs = M.filter isTarget        lmap
        lifts = M.filter (== LiftClosed) lmap
        targetSourcePositions = listMap . filterMaybeTuples1 . map (\(pos,obj) -> (pos, lookup obj (revertMap trams)) ) . toList $ lvTrampolines lvl
        filterMaybeTuples1 ls = [(x,y) | (x, Just y) <- ls]

       
startGames :: Delays -> [GameState] -> IO ()
startGames _ [] = putStrLn "You finished all levels! :)"
startGames delays games@(game:nextGames) = do
        game' <- playGame Nothing delays game
        
        let replay      = playGame (Just $ reverse (gsMoveHistory game')) delays game

        case gsProgress game' of
                Restart         -> restartLevel
                Loss reason     -> do
                                        let msg = case reason of
                                                        FallingRock     -> "You got crushed by rocks! :("
                                                        Drowning        -> "You drowned! :("
                                        putStrLn msg
                                        printStats game'
                                        askForContinue_ restartLevel
                Win             -> do
                                        putStrLn "You won! Congratulations!"
                                        printStats game'
                                        let ask = askForAction
                                                (fromList [ (UiContinue , continueGames)
                                                          , (UiRestart  , replay >>= printStats >> ask)
                                                          , (UiAbort    , return ())
                                                          ])
                                                $  "Press " ++ showKeyMapping UiRestart ++ " to view replay\n"
                                                ++ "Press " ++ showKeyMapping UiContinue ++ " to continue"
                                        ask
                Skip            -> startGames delays (nextGames ++ [game])
                Abort           -> do
                                        putStrLn "You abandoned Marvin! :'("
                                        printStats game'
                
                Running         -> error "Invalid state" -- should not happen, playGame loops until progress != Running
        where
        restartLevel    = startGames delays games
        continueGames   = startGames delays nextGames
        printStats g= do
                putStrLn $ "Points: "     ++ show            (calculatePoints g)
                putStrLn $ "Your route: " ++ showMoveHistory (gsMoveHistory g)


calculatePoints :: GameState -> Int
calculatePoints gs
        = lambdas             * 25
        + moves               * (-1)
        + isWon     * lambdas * 50
        + isAborted * lambdas * 25
        where
        isWon           = fromEnum $ gsProgress gs == Win
        isAborted       = fromEnum $ gsProgress gs == Abort
        moves           = gsMoves gs
        lambdas         = gsLambdasCollected gs


playGame :: Maybe [UserInput] -> Delays -> GameState -> IO GameState
playGame (Just []) _   game = return game {gsProgress = if gsProgress game == Win then Win else Abort}
playGame mMoves delays game = do
        clearScreen
        printLevel game
        if gsProgress game == Running
          then do
                dir <- case mMoves of
                  Nothing       -> getInput
                  Just (x:_)    -> threadDelay (deMove delays) >> return x
                  Just []       -> error "playGame with Just [] - cannot happen" -- cannot happen, see first line
                case dir of
                        UiAbort         -> return game {gsProgress = Abort}
                        UiRestart       -> return game {gsProgress = Restart}
                        UiSkip          -> return game {gsProgress = Skip}
                        _               -> 
                                case moveRobot game dir of
                                        Nothing   -> playGame mMoves delays game
                                        Just game' -> do
                                                clearScreen
                                                printLevel game'
                                                -- no update after a win
                                                --    otherwise a rock may fall onto the OpenLift the Robot just stepped into, or
                                                --    or the Robot may drown after stepping into the OpenLift 
                                                if gsProgress game' == Win
                                                  then playGame tailMoves delays game'
                                                  else do
                                                        let game''   = updateGameState game'
                                                        threadDelay $ deMapUpdate delays
                                                        playGame tailMoves delays game''
          else return game
        
        where
        tailMoves = case mMoves of
                Just (_:xs) -> Just xs
                Just []     -> Just []
                Nothing     -> Nothing


moveRobot :: GameState -> UserInput -> Maybe GameState
moveRobot game dir = do
        let lvl = gsLevel game
        nrp   <- newRobotPosition
        field <- lookup nrp $ lvMap lvl
        let lmap = lvMap lvl 
        case field of
                Empty   |  dir == UiUseRazor
                    && lvRazors lvl > 0
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insertIntoAdjacentBeardCells nrp Empty lmap
                                                                           , lvRazors = lvRazors lvl - 1}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                Empty   | dir /= UiUseRazor
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                Earth           -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                Lambda          -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsLambdasCollected = gsLambdasCollected game + 1
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                LiftOpen        -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap}
                                                , gsProgress = Win
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                Rock rt |  dir == UiRight
                        && lookup (rX+2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert (rX+2, rY) (Rock rt) . insert nrp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                Rock rt |  dir == UiLeft
                        && lookup (rX-2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert (rX-2, rY) (Rock rt) . insert nrp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                tc@(Trampoline _)
                                -> return game  { gsLevel = (gsLevel game) { lvMap = removeTargetIfNoOtherTrampsTargetIt . insert nrp Empty $ lmap
                                                                           , lvTrampolines = lvlTrampsNew} -- TODO: update 2 remaining maps?
                                                , gsRobotPosition = roboPosTrampoline
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                        where
                        lvlTramps       = lvTrampolines lvl
                        lvlTrampsNew    = delete tc lvlTramps
                        removeTargetIfNoOtherTrampsTargetIt
                                = maybe
                                        id -- No Target for Trampoline found
                                        (\target -> if (>=1) . size . M.filter (==target) $ lvlTrampsNew then id else insert roboPosTrampoline Empty)
                                        (lookup tc lvlTramps)
                        roboPosTrampoline
                                = fromMaybe
                                        nrp
                                        (do -- Maybe monad
                                                targ <- lookup tc lvlTramps
                                                lookup targ (gsTargets game))
                
                Razor
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Empty lmap
                                                                           , lvRazors = lvRazors lvl + 1}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1
                                                , gsMoveHistory = addMove dir}
                                                
                _   | dir == UiWait
                                -> return game
                _   | dir == UiAbort
                                -> return game  { gsProgress = Abort}
                _               -> Nothing
        where
        newRobotPosition= case dir of
                UiUp            -> Just (rX   ,rY+1)
                UiLeft          -> Just (rX-1 ,rY  )
                UiDown          -> Just (rX   ,rY-1)
                UiRight         -> Just (rX+1 ,rY  )
                _               -> Just (rX   ,rY  ) -- use Nothing to produce an invalid GameState
        (rX, rY) = gsRobotPosition game
        
        insertIntoAdjacentBeardCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentBeardCells (x,y) obj lvlMap = insertObjectIntoPositions (conditionalInsert isBeard) obj lvlMap (adjacentPositions x y)
        
        addMove m = m : gsMoveHistory game


updateGameState :: GameState -> GameState
updateGameState gs
        = updatedGS
                { gsTick        = thisTick
                , gsProgress    = case () of -- dirty multi-way if, can't wait for ghc 7.5
                                        _ | gsAirLeft updatedGS < 0             -> Loss Drowning        -- robot may drown
                                          | robotGotCrushed gs updatedGS        -> Loss FallingRock     -- robot may get crushed by a rock
                                          | otherwise                           -> gsProgress updatedGS -- no change
                , gsLevel = (gsLevel updatedGS) { lvWater = newWater }
                }
        where
        thisTick     = gsTick updatedGS + 1
        --gs'          = gs { gsLevel = (gsLevel gs) { lvWater = newWater } }
        updatedGS    = execState (updateLevel keysToUpdate gs) gs -- gs instead of gs' -> robot underwater after map update instead of instantly
        updatedLvl   = gsLevel updatedGS
        water        = lvWater updatedLvl
        newWater     = water + if flooding /= 0 && thisTick `mod` flooding == 0 then 1 else 0
        flooding     = lvFlooding updatedLvl
        
        updateLevel :: [Position] -> GameState -> State GameState GameState
        updateLevel [] _ = get
        updateLevel (pos:poss) lvl = do
                modify $ flip (updateLevelByPosition lvl) pos
                updateLevel poss lvl
        
        keysToUpdate = sortForTraversal . keys . lvMap . gsLevel $ gs
        
        updateLevelByPosition :: GameState -> GameState -> Position -> GameState
        updateLevelByPosition g g' pos
                = case lookup pos lvlMap of
                        Nothing -> g'
                        Just e -> processObject g g' e pos
                where lvlMap = insert (gsRobotPosition gs) Robot . lvMap . gsLevel $ g

        robotGotCrushed oldGs newGs
                = not (isRockOrLambda' aboveOld)
                     && isRockOrLambda' aboveNew
                where
                isRockOrLambda' = maybe False (\o -> isRock o || isLambda o)
                aboveOld        = lookup (rX,rY+1) $ lvMap . gsLevel $ oldGs
                aboveNew        = lookup (rX,rY+1) $ lvMap . gsLevel $ newGs
                (rX,rY)         = gsRobotPosition oldGs


processObject :: GameState -> GameState -> Object -> Position -> GameState
processObject gs gs' o (x,y)
        = case o of
                Rock rt |  lookup (x, y-1)     lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x, y-1) rt lvl . insert (x,y) Empty $ lvl'
                Rock rt |  (isRock' . lookup (x, y-1)) lvl
                        && lookup (x+1, y)     lvl == Just Empty
                        && lookup (x+1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x+1, y-1) rt lvl . insert (x,y) Empty $ lvl'
                Rock rt |  (isRock' . lookup (x, y-1)) lvl
                        && (  lookup (x+1, y)   lvl /= Just Empty
                           || lookup (x+1, y-1) lvl /= Just Empty
                           )
                        && lookup (x-1, y)     lvl == Just Empty
                        && lookup (x-1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insert (x-1, y-1) (Rock rt) . insert (x,y) Empty $ lvl'
                Rock rt |  lookup (x, y-1)     lvl == Just Lambda
                        && lookup (x+1, y)     lvl == Just Empty
                        && lookup (x+1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x+1, y-1) rt lvl . insert (x,y) Empty $ lvl'
                LiftClosed | gsLambdasCollected gs == (lvLambdas . gsLevel) gs 
                        -> modifyLevelMap $ insert (x,y) LiftOpen lvl'
                -- Beards and Razors extension
                Beard g | g > 0
                        -> modifyLevelMap $ insert (x, y) (Beard $ g-1) lvl'
                Beard _
                        -> modifyLevelMap $ insert (x,y) beardInit . insertIntoAdjacentEmptyCells (x, y) beardInit $ lvl'
                -- Flooding extension
                Robot   -> gs' { gsAirLeft = if (lvWater . gsLevel) gs >= y then airLeft - 1 else (lvWaterproof . gsLevel) gs}

                _       -> modifyLevelMap lvl'
        where
        modifyLevelMap lvlMap = gs' { gsLevel = (gsLevel gs') { lvMap = lvlMap}}
        
        insertFallingRock :: Position -> RockType -> LevelMap -> LevelMap -> LevelMap
        insertFallingRock pos@(rX,rY) rt l l'
                = case lookup (rX,rY-1) l of
                        Just Empty      -> insert pos (Rock rt) l'
                        _               -> case rt of
                                                Simple          -> insert pos (Rock rt) l'
                                                HigherOrder     -> insert pos Lambda    l'
        
        airLeft = gsAirLeft gs'
        isRock' = maybe False isRock
        lvl  = insert (gsRobotPosition gs) Robot . lvMap . gsLevel $ gs
        lvl' = lvMap . gsLevel $ gs'
        beardInit = Beard $ (lvGrowthRate . gsLevel) gs -1
        
        insertIntoAdjacentEmptyCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentEmptyCells (x',y') obj lvlMap = insertObjectIntoPositions insertIfEmpty obj lvlMap (adjacentPositions x' y')

        insertIfEmpty :: Position -> Object -> LevelMap -> LevelMap
        insertIfEmpty (x',y') obj lmap = if lookup (x',y') lvl == Just Empty then insert (x',y') obj lmap else lmap -- note that lvl not lmap is used



insertObjectIntoPositions :: (Position -> Object -> LevelMap -> LevelMap) -> Object -> LevelMap -> [Position] -> LevelMap
insertObjectIntoPositions condInsert obj = foldl (flip (flip condInsert obj))


conditionalInsert :: (Object -> Bool) -> Position -> Object -> LevelMap -> LevelMap
conditionalInsert cond (x,y) obj lmap = if cond' $ lookup (x,y) lmap then insert (x,y) obj lmap else lmap
        where cond' = maybe False cond

adjacentPositions :: Int -> Int -> [(Int,Int)]
adjacentPositions x y = [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1]]


-- Main

main :: IO ()
main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hideCursor
            
        args <- getArgs
        lvlsM  <- mapM readLevelFile args
        
        case sequence lvlsM of
                Nothing ->
                        putStrLn "Error loading maps, invalid map format"
                Just lvls -> do
                        let gamesM = mapM createGame lvls
                        case gamesM of
                                Nothing -> putStrLn "Error creating levels, invalid properties"
                                Just gs -> do
                                        clearScreen
                                        putStrLn "Welcome to LambdaLifter (alpha)"
                                        putStrLn ""
                                        printControls
                                        askForContinue_ (startGames delays gs)
        
        showCursor
        where
        delays = defaultDelays