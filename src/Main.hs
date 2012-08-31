module Main where

import           Data.Lens.Lazy
import           Control.Concurrent  (threadDelay)
import           Control.Monad.State
import           Control.Monad.Error (throwError)
import           Data.Map            as M (delete, elemAt, filter, fromList, insert, keys, keys, lookup,
                                           size, toList)
import           Data.Maybe          (fromMaybe)
import           Prelude             as P hiding (lookup)
import           System.Console.ANSI (clearScreen, hideCursor, showCursor)
import           System.Environment  (getArgs)
import           System.IO           (BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin)

import           Game
import           Input
import           Persistence
import           Utils

{--
TODOs:
 - highscores
        - stats directory with map files containing the score and the corresponding moves
 - add feature to save the game
        - probably needed: currentLevel(Map, Trampolines) + RobotPos
        - extra: score/stats, loaded maps / finished maps
 - use state monad
--}


-- | Delays for printing the level
data Delays = Delays
        { deMapUpdate           :: Int
        , deMove                :: Int
        }

-- | The default delays for printing the level etc.
defaultDelays :: Delays
defaultDelays = Delays
        { deMapUpdate           = 12500
        , deMove                = 30000
        }


-- Functions

-- | Creates a game state from a level - or fails if the level description is invalid
createGame :: Level -> Result GameState
createGame lvl = do
        roboPos <- if size robos == 1
                     then return . fst . elemAt 0 $ robos
                     else throwError $ LevelError (lvl^.name) "level does not contain exactly one robot"
        liftPos <- if size lifts == 1
                     then return . fst . elemAt 0 $ lifts
                     else throwError $ LevelError (lvl^.name) "level does not contain exactly one lift"
        return GameState
                { _level               = levelMap ^= insert roboPos Empty (lvl^.levelMap) $ lvl -- delete robot start position
                , _robotPosition       = roboPos
                , _liftPosition        = liftPos
                , _tick                = 0
                , _airLeft             = lvl^.waterproof

                , _targets             = revertMap targs
                , _targetSources       = targetSourcePositions

                , _progress            = Running
                , _lambdasCollected    = 0
                , _moves               = 0
                , _moveHistory         = [] -- head is the most recent move
                }
        where
        lmap   = lvl^.levelMap
        robos = M.filter (== Robot)      lmap
        trams = M.filter isTrampoline    lmap
        targs = M.filter isTarget        lmap
        lifts = M.filter isLiftClosed    lmap
        targetSourcePositions = listMap . filterMaybeTuples1 . map (\(pos,obj) -> (pos, lookup obj (revertMap trams)) ) . toList $ lvl^.trampolines
        filterMaybeTuples1 ls = [(x,y) | (x, Just y) <- ls]


-- | Starts a list of Games, expects delays for printing
startGames :: Delays -> [GameState] -> IO ()
startGames _ [] = putStrLn "You finished all levels! :)"
startGames delays games@(game:nextGames) = do
        game' <- playGame Nothing delays game

        let replay      = playGame (Just $ reverse (game'^.moveHistory)) delays game

        case game'^.progress of
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
                putStrLn $ "Your route: " ++ showMoveHistory (g^.moveHistory)


-- | Calculated the points of the current game state as defined in the ICFP specification
calculatePoints :: GameState -> Int
calculatePoints gs
        = lambdaCount               * 25
        + moveCount                 * (-1)
        + isWon       * lambdaCount * 50
        + isAborted   * lambdaCount * 25
        where
        isWon           = fromEnum $ gs^.progress == Win
        isAborted       = fromEnum $ gs^.progress == Abort
        moveCount       = gs^.moves
        lambdaCount     = gs^.lambdasCollected


-- | plays the game interactively or plays a replay if a user input is given
playGame :: Maybe [UserInput] -> Delays -> GameState -> IO GameState
playGame (Just []) _   game = return $ progress ^%= (\p -> if p == Win then Win else Abort) $ game
playGame mMoves delays game = do
        clearScreen
        printLevel game
        if game^.progress == Running
          then do
                dir <- case mMoves of
                  Nothing       -> getInput
                  Just (x:_)    -> threadDelay (deMove delays) >> return x
                  Just []       -> error "playGame with Just [] - cannot happen" -- cannot happen, see first line pattern match
                case dir of
                        UiAbort         -> return $ progress ^= Abort   $ game
                        UiRestart       -> return $ progress ^= Restart $ game
                        UiSkip          -> return $ progress ^= Skip    $ game
                        _               ->
                                case moveRobot game dir of
                                        Nothing   -> playGame mMoves delays game
                                        Just game' -> do
                                                clearScreen
                                                printLevel game'
                                                -- no update after a win
                                                --    otherwise a rock may fall onto the OpenLift the Robot just stepped into, or
                                                --    or the Robot may drown after stepping into the OpenLift
                                                if game'^.progress == Win
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


-- | Moves the robot depending on the users input and produces a resulting GameState if the user didn't quit
moveRobot :: GameState -> UserInput -> Maybe GameState
moveRobot game dir = do
        let lvl = game^.level
        nrp   <- newRobotPosition
        let defaultChanges = (robotPosition     ^= nrp) .
                             (moves             ^%= (+1)) .
                             (moveHistory       ^%= (dir:))
        let clearPosition  = level             ^= (levelMap ^%= insert nrp Empty) (game ^. level)
        let lmap = lvl^.levelMap
        field <- lookup nrp lmap
        case field of
                Empty   |  dir == UiUseRazor
                    && lvl^.razors > 0
                                -> return $     (level             ^=
                                                        ((levelMap  ^%= insertIntoAdjacentBeardCells nrp Empty) .
                                                         (razors    ^%= flip (-) 1)
                                                        $ game^.level)) .
                                                defaultChanges
                                                $ game
                Empty   | dir /= UiUseRazor
                                -> return $     defaultChanges
                                                  game
                Earth           -> return $     clearPosition .
                                                defaultChanges
                                                $ game
                Lambda          -> return $     clearPosition .
                                                (lambdasCollected  ^%= (+1)) .
                                                defaultChanges
                                                $ game
                (Lift Open)     -> return $     clearPosition .
                                                (progress          ^= Win) .
                                                defaultChanges
                                                $ game
                Rock rt |  dir == UiRight
                        && lookup (rX+2, rY) (lvl^.levelMap) == Just Empty
                                -> return $     (level             ^= (levelMap ^%= insert (rX+2, rY) (Rock rt) . insert nrp Empty) (game ^. level)) .
                                                defaultChanges
                                                $ game
                Rock rt |  dir == UiLeft
                        && lookup (rX-2, rY) (lvl^.levelMap) == Just Empty
                                -> return $     (level             ^= (levelMap ^%= insert (rX-2, rY) (Rock rt) . insert nrp Empty) (game ^. level)) .
                                                defaultChanges
                                                $ game
                tc@(Trampoline _)
                                -> return $     (level             ^=
                                                        ((levelMap  ^%= removeTargetIfNoOtherTrampsTargetIt . insert nrp Empty) .
                                                         (trampolines ^%= delete tc) -- TODO: update 2 remaining maps?
                                                        $ game^.level)) .
                                                (robotPosition     ^= roboPosTrampoline) . -- "overwrites" default change of robot position
                                                defaultChanges
                                                $ game
                        where
                        lvlTramps       = lvl^.trampolines
                        lvlTrampsNew    = delete tc lvlTramps
                        removeTargetIfNoOtherTrampsTargetIt
                                = maybe
                                        id -- No Target for Trampoline found
                                        (\target -> if (>=1) . size . M.filter (==target) $ lvlTrampsNew then id else insert roboPosTrampoline Empty) -- TODO: update 2 remaining maps?
                                        (lookup tc lvlTramps)
                        roboPosTrampoline
                                = fromMaybe
                                        nrp
                                        (do -- Maybe monad
                                                targ <- lookup tc lvlTramps
                                                lookup targ (game^.targets))

                Razor
                                -> return $     (level ^= (razors ^%= (+ 1)) (clearPosition game ^. level)) .
                                                defaultChanges
                                                $ game

                _   | dir == UiWait
                                -> return game
                _   | dir == UiAbort
                                -> return $ (progress ^= Abort) game
                _               -> Nothing
        where
        newRobotPosition= case dir of
                UiUp            -> Just (rX   ,rY+1)
                UiLeft          -> Just (rX-1 ,rY  )
                UiDown          -> Just (rX   ,rY-1)
                UiRight         -> Just (rX+1 ,rY  )
                _               -> Just (rX   ,rY  ) -- use Nothing to produce an invalid GameState
        (rX, rY) = game^.robotPosition

        insertIntoAdjacentBeardCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentBeardCells (x,y) obj lvlMap = insertObjectIntoPositions (conditionalInsert isBeard) obj lvlMap (adjacentPositions x y)


-- | The map update phase as described in the ICFP specification
updateGameState :: GameState -> GameState
updateGameState gs
        = (tick         ^= thisTick) .
          (progress     ^= case () of -- dirty multi-way if, can't wait for ghc 7.5
                                        _ | updatedGS^.airLeft  < 0             -> Loss Drowning           -- robot may drown
                                          | robotGotCrushed gs updatedGS        -> Loss FallingRock        -- robot may get crushed by a rock
                                          | otherwise                           -> updatedGS^.progress) .  -- no change)
          (level        ^= ( water ^= newWater $ updatedGS^.level))
          $ updatedGS
        where
        thisTick     = updatedGS^.tick + 1
        --gs'          = gs { gsLevel = (gsLevel gs) { lvWater = newWater } }
        updatedGS    = execState (updateLevel keysToUpdate gs) gs -- gs instead of gs' -> robot underwater after map update instead of instantly
        updatedLvl   = updatedGS^.level
        newWater     = updatedLvl^.water + if floodingRate /= 0 && thisTick `mod` floodingRate == 0 then 1 else 0
        floodingRate = updatedLvl^.flooding

        updateLevel :: [Position] -> GameState -> State GameState GameState
        updateLevel [] _ = get
        updateLevel (pos:poss) lvl = do
                modify $ flip (updateLevelByPosition lvl) pos
                updateLevel poss lvl

        keysToUpdate = sortForTraversal . keys $ gs^.level^.levelMap

        updateLevelByPosition :: GameState -> GameState -> Position -> GameState
        updateLevelByPosition g g' pos
                = case lookup pos lvlMap of
                        Nothing -> g'
                        Just e -> processObject g g' e pos
                where lvlMap = insert (gs^.robotPosition) Robot $ g^.level^.levelMap

        robotGotCrushed oldGs newGs
                = not (isRockOrLambda' aboveOld)
                     && isRockOrLambda' aboveNew
                where
                isRockOrLambda' = maybe False (\o -> isRock o || isLambda o)
                aboveOld        = lookup (rX,rY+1) $ oldGs^.level^.levelMap
                aboveNew        = lookup (rX,rY+1) $ newGs^.level^.levelMap
                (rX,rY)         = oldGs^.robotPosition


-- | Cell-wise update of the level(map) as described in the icfp specification
processObject :: GameState -> GameState -> Object -> Position -> GameState
processObject gs gs' o (x,y)
        = case o of
                Rock rt |  lookup (x, y-1)     lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x, y-1) rt lvl . insert (x,y) Empty
                Rock rt |  (isRock' . lookup (x, y-1)) lvl
                        && lookup (x+1, y)     lvl == Just Empty
                        && lookup (x+1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x+1, y-1) rt lvl . insert (x,y) Empty
                Rock rt |  (isRock' . lookup (x, y-1)) lvl
                        && (  lookup (x+1, y)   lvl /= Just Empty
                           || lookup (x+1, y-1) lvl /= Just Empty
                           )
                        && lookup (x-1, y)     lvl == Just Empty
                        && lookup (x-1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insert (x-1, y-1) (Rock rt) . insert (x,y) Empty
                Rock rt |  lookup (x, y-1)     lvl == Just Lambda
                        && lookup (x+1, y)     lvl == Just Empty
                        && lookup (x+1, y-1)   lvl == Just Empty
                        -> modifyLevelMap $ insertFallingRock (x+1, y-1) rt lvl . insert (x,y) Empty
                (Lift Closed) | gs^.lambdasCollected == gs^.level^.lambdas
                        -> modifyLevelMap $ insert (x,y) (Lift Open)
                -- Beards and Razors extension
                Beard g | g > 0
                        -> modifyLevelMap $ insert (x, y) (Beard $ g-1)
                Beard _
                        -> modifyLevelMap $ insert (x,y) beardInit . insertIntoAdjacentEmptyCells (x, y) beardInit
                -- Flooding extension
                Robot   -> (airLeft ^= if gs^.level^.water >= y then air - 1 else gs^.level^.waterproof) gs'

                _       -> gs'
        where
        modifyLevelMap f = (level ^=
                                   (levelMap ^%= f) (gs ^. level)
                                ) gs'

        insertFallingRock :: Position -> RockType -> LevelMap -> LevelMap -> LevelMap
        insertFallingRock pos@(rX,rY) rt l l'
                = case lookup (rX,rY-1) l of
                        Just Empty      -> insert pos (Rock rt) l'
                        _               -> case rt of
                                                Simple          -> insert pos (Rock rt) l'
                                                HigherOrder     -> insert pos Lambda    l'

        air = gs'^.airLeft
        isRock' = maybe False isRock
        lvl  = insert (gs^.robotPosition) Robot $ gs^.level^.levelMap
        beardInit = Beard $ gs^.level^.growthRate -1

        insertIntoAdjacentEmptyCells :: Position -> Object -> LevelMap -> LevelMap
        insertIntoAdjacentEmptyCells (x',y') obj lvlMap = insertObjectIntoPositions insertIfEmpty obj lvlMap (adjacentPositions x' y')

        insertIfEmpty :: Position -> Object -> LevelMap -> LevelMap
        insertIfEmpty (x',y') obj lmap = if lookup (x',y') lvl == Just Empty then insert (x',y') obj lmap else lmap -- note that lvl not lmap is used


-- | Insers an object into the specified positions if certain condidions are met (as defined in the "conditional insert function")
insertObjectIntoPositions :: (Position -> Object -> LevelMap -> LevelMap) -> Object -> LevelMap -> [Position] -> LevelMap
insertObjectIntoPositions condInsert obj = foldl (flip (flip condInsert obj))


-- | "Generic" conditional insert function
conditionalInsert :: (Object -> Bool) -> Position -> Object -> LevelMap -> LevelMap
conditionalInsert cond (x,y) obj lmap = if cond' $ lookup (x,y) lmap then insert (x,y) obj lmap else lmap
        where cond' = maybe False cond

-- | Computes all adjacent position for a given position
adjacentPositions :: Int -> Int -> [(Int,Int)]
adjacentPositions x y = [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1]]


-- Main

main :: IO ()
main = do
        hSetEcho stdin False                    -- no echoing of characters on input
        hSetBuffering stdin NoBuffering         -- disable buffering of input
        hideCursor                              -- hide the cursor

        args <- getArgs
        lvlsM  <- mapM readLevelFile args

        case sequence lvlsM of
                Left err ->
                        print err  -- Error loading maps, invalid map format
                Right lvls -> do
                        let gamesM = mapM createGame lvls
                        case gamesM of
                                Left err -> print err -- error loading level
                                Right gs -> do
                                        clearScreen
                                        putStrLn "Welcome to LambdaLifter"
                                        putStrLn ""
                                        printControls
                                        askForContinue_ (startGames delays gs)

        showCursor                              -- reset the cursor
        where
        delays = defaultDelays