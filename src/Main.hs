module Main where

import Prelude hiding (lookup, filter, null)
import qualified Prelude as P
import Data.List hiding (lookup, insert, filter, null)
import Data.Map hiding (map, foldl)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import System.IO (BufferMode(NoBuffering),stdin, hSetBuffering, hSetEcho)
import System.Environment (getArgs)
import Data.Char (toLower)

{--
TODOs: 
 - add (global) scoring
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
        | Trampoline Char
        | Target Char
        | Empty
        deriving (Eq, Ord)

instance Show Object where
        show = (:[]). objectToChar

type Position = (Integer, Integer)

data Level = Level
        { lvMap :: Map Position Object
        , lvTrampolines :: Map Object Object -- Trampoline -> Target
        }

data Movement
        = MvLeft
        | MvRight
        | MvUp
        | MvDown
        | MvWait
        | MvAbort
        | MvRestart
        | MvSkip
        deriving Eq

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
                Trampoline c    -> c
                Target c        -> c
                Empty           -> ' ' -- TODO: eww? see note below


charToObject :: Char -> Object
charToObject c
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
                a   -> error $ "Cannot convert \"" ++ a : "\" to Object: no mapping found"

readLevelFromFile :: FilePath -> IO Level
readLevelFromFile f = do
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
{-
        extractTrampolinesFromMetadata tMap (l:ls) =
                if "Trampoline " `isPrefixOf` l
                  then
                        extractTrampolinesFromMetadata (foldl (flip (insert trampo)) tMap targets) ls
                  else
                        tMap
                where
                (trampo:targets) = P.filter (`elem` ['A' .. 'I'] ++ ['0'..'9']) l
-}


levelStringToMap :: [String] -> Map Position Object
levelStringToMap sl = fromList . concatMap (\(y, s) -> zip [(x,y) | x <- [1..]] (map charToObject s)) $ zip [1..] (reverse sl)


printLevel :: Level -> IO ()
printLevel l = do
        unless (null trams) $ do
                putStrLn "Trampolines:"
                mapM_ (putStrLn . show') $ toList trams
        printLevelMap l
        where
        trams = lvTrampolines l
        show' (tram, targ) = show tram ++ " -> " ++ show targ

printLevelMap :: Level -> IO ()
printLevelMap = sequence_ . printAList . levelToSortedAList
        where
        printAList :: [(Position, Object)] -> [IO ()]
        printAList ls@(((x0,_),o):((x1,_),_):_)
                | x1 < x0 = print o : (printAList . tail) ls
                | otherwise = (putChar . objectToChar) o : (printAList . tail) ls
        printAList (((_,_),o):xs) = (putChar . objectToChar) o : printAList xs
        printAList [] = [putStrLn ""]

        levelToSortedAList :: Level -> [(Position, Object)]
        levelToSortedAList = sortBy levelMapOutputSort . toList . lvMap
        
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
                
                , gsTargets             = revertMap targs
                , gsTargetSources       = targetSourcePositions
                
                , gsProgress            = Running
                , gsLambdasCollected    = 0
                , gsMoves               = 0
                }
        where
        lmap   = lvMap lvl
        robos = filter (== Robot)      lmap
        trams = filter isTrampoline    lmap
        targs = filter isTarget        lmap
        lifts = filter (== LiftClosed) lmap
        lvlDims = (maximum xs, maximum ys)
        (xs,ys) = unzip . keys $ lmap
        isTrampoline (Trampoline _)     = True
        isTrampoline _                  = False
        isTarget (Target _)             = True
        isTarget _                      = True
        targetSourcePositions = listMap . filterMaybes . map (\(pos,obj) -> (pos, lookup obj (revertMap trams)) ) . toList $ lvTrampolines lvl


revertMap :: Ord b => Map a b -> Map b a
revertMap = fromList . map flipElems . toList
        where flipElems (a,b) = (b,a)


listMap :: Ord k => [(k, a)] -> Map k [a]
listMap pairs =
    foldl insertPair empty (reverse pairs)
    where insertPair m (k,a) = insert k (a : findWithDefault [] k m) m


filterMaybes :: [(a, Maybe b)] -> [(a, b)]
filterMaybes ls = [(x,y) | (x, Just y) <- ls]


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
        = case lookup pos (lvMap . gsLevel $ gs) of
                Nothing -> gs'
                Just e -> processObject gs gs' e pos


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


playGame :: Int -> GameState -> IO GameState
playGame updateDelay game = do
        printLevel . gsLevel $ game
        if gsProgress game == Running
          then do
                dir <- getInput
                case dir of
                        MvAbort         -> return game {gsProgress = Abort}
                        MvRestart       -> return game {gsProgress = Restart}
                        MvSkip       -> return game {gsProgress = Skip}
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
        aboveOld        = lookup (rX,rY+1) $ lvMap . gsLevel $ oldGs
        aboveNew        = lookup (rX,rY+1) $ lvMap . gsLevel $ newGs
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
        'n' -> MvSkip
        _   -> MvWait


moveRobot :: GameState -> Movement -> Maybe GameState -- TODO: Maybe unnecessary
moveRobot game dir = do
        let lvl = gsLevel game
        nrp   <- newRobotPosition
        field <- lookup nrp $ lvMap lvl
        let lmap = lvMap lvl
        case field of
                Robot   | dir == MvWait
                                -> return game
                Robot   | dir == MvAbort
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
                Rock    |  dir == MvRight
                        && lookup (rX+2, rY) (lvMap lvl) == Just Empty
                                -> return game  { gsLevel = (gsLevel game) { lvMap = insert nrp Robot . insert (rX+2, rY) Rock . insert orp Empty $ lmap}
                                                , gsRobotPosition = nrp
                                                , gsMoves = gsMoves game + 1}
                Rock    |  dir == MvLeft
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
                MvUp            -> Just (rX   ,rY+1)
                MvLeft          -> Just (rX-1 ,rY  )
                MvDown          -> Just (rX   ,rY-1)
                MvRight         -> Just (rX+1 ,rY  )
                _               -> Just (rX   ,rY  ) -- use Nothing to produce an invalid GameState
        orp@(rX, rY) = gsRobotPosition game
        

startGames :: Int -> [GameState] -> IO ()
startGames _ [] = putStrLn "You finished all levels! :)"
startGames updateDelay games@(game:nextGames) = do
        game' <- playGame updateDelay game
        
        putStrLn $ "Lambdas collected: " ++ show (gsLambdasCollected game')
        putStrLn $ "Moves: "             ++ show (gsMoves game')
        
        case gsProgress game' of
                Restart   -> restartLevel
                Loss      -> do
                                putStrLn "You got crushed by rocks! :("
                                restartLevel
                Win       -> do
                                putStrLn "You won! Congratulations!"
                                startGames updateDelay nextGames
                Skip      -> startGames updateDelay (nextGames ++ [game])
                Abort     -> putStrLn "You abandoned Marvin! :'("
                Running   -> error "Invalid state"
        where
        restartLevel = startGames updateDelay games


printControls :: IO ()
printControls = do
        putStrLn "Controls: "
        putStrLn "  WASD to move"
        putStrLn "  E to wait"
        putStrLn "  R to restart"
        putStrLn "  N to skip the level"
        putStrLn "  Q to quit"
        putStrLn ""

-- Main

main :: IO ()
main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        
        args <- getArgs
        lvls  <- mapM readLevelFromFile args
        let gamesM = mapM createGame lvls
        case gamesM of
                Nothing -> putStrLn "Error loading levels..." -- TODO: verbose error msgs
                Just gs -> do
                        putStrLn "Welcome to LambdaLifter (alpha)"
                        printControls
                        startGames updateDelay gs
        
        where
        updateDelay = 125000