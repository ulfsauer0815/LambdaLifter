{-# LANGUAGE TemplateHaskell #-}
module Game --( Object(..), RockType(..), LiftState(..), Position, LevelMap, Level(..), GameProgress(..), LossReason(..), GameState(..)
            --, isTrampoline, isTarget, isBeard, isRock, isLambda, isHigherOrderRock, isSimpleRock
            --, isLambdaLike, isEmpty, isWall, isEarth, isLiftOpen, isLiftClosed, isRazor
            --, charToObject, objectToChar, objectColor, printLevel
            --, sortForTraversal
            --, ObjectInitValues(..), LevelValues(..), defaultLevelValues
            --, Result, GameError(..))
where

import           Data.Lens.Lazy
import           Data.Lens.Template (makeLenses)
import           Control.Monad
import           Control.Monad.Error
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Map            as M (Map, insert, null, toList)
import           System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGR)

import           Input               (UserInput)

-- Data structures

-- | Level objects
data Object
        = Robot
        | Wall
        | Rock RockType
        | Lambda
        | Lift LiftState
        | Earth
        | Trampoline Char
        | Target Char
        | Beard Int
        | Razor
        | Empty
        deriving (Eq, Ord)

-- | Lift can be open or closed
data LiftState
        = Open
        | Closed
        deriving (Eq, Ord)

-- | Rock level object
data RockType
        = Simple
        | HigherOrder
        deriving (Eq, Ord)


-- | Level position
type Position = (Int, Int)
-- | Level as a map of positions to objects 
type LevelMap = Map Position Object

-- | Level, containing the LevelMap and extension infos
data Level = Level
        { _name        :: String               -- ^ Name of the level
        , _levelMap    :: LevelMap             -- ^ 2D-Level description
        , _trampolines :: Map Object Object    -- ^ Trampoline to target mapping: Trampoline -> Target
        , _growthRate  :: Int                  -- ^ Beard growth rate
        , _razors      :: Int                  -- ^ Number of razors available to the user
        , _lambdas     :: Int                  -- ^ Number of lambdas that have to be collected
        , _water       :: Int                  -- ^ Water-level
        , _flooding    :: Int                  -- ^ Flooding rate
        , _waterproof  :: Int                  -- ^ Number of steps the robot can survive underwater
        }

-- | LevelValues to initialize the Level with, like the beard growth rate etc. 
data LevelValues = LevelValues
        { leBeardGrowthRate     :: Int
        , leRazors              :: Int
        , leFlooding            :: Int
        , leWater               :: Int
        , leWaterproof          :: Int
        }

-- | the default level values
defaultLevelValues :: LevelValues
defaultLevelValues = LevelValues
        { leBeardGrowthRate     = 25
        , leRazors              =  0
        , leFlooding            =  0
        , leWater               =  0
        , leWaterproof          = 10
        }


-- | Current progress of the game
data GameProgress
        = Running
        | Win
        | Loss LossReason
        | Abort
        | Restart
        | Skip
        deriving Eq


-- | GameProgress -> Reason why the user lost the game
data LossReason
        = FallingRock
        | Drowning
        deriving Eq


-- | (Current) game state, inclusing the Level(map), position of the robot etc.
data GameState = GameState
        { _level               :: Level                        -- ^ Level
        , _robotPosition       :: Position                     -- ^ Position of the robot in the level(map)
        , _liftPosition        :: Position                     -- ^ Position of the lift in the level(map)
        , _tick                :: Int                          -- ^ Current game tick / game updates passed
        , _airLeft             :: Int                          -- ^ Number of steps the robot has left underwater

        , _targets             :: Map Object Position          -- ^ Mapping a target ot the position in the level(map): Target -> Position of Target
        , _targetSources       :: Map Object [Position]        -- ^ Mapping of a target to a list of trampolines: Target -> [Position of Trampoline]

        , _progress            :: GameProgress                 -- ^ Game progress, e.g. Running, Win, Loss
        , _lambdasCollected    :: Int                          -- ^ Number of lambdas the user collected
        , _moves               :: Int                          -- ^ Number of moves made by the user
        , _moveHistory         :: [UserInput]                  -- ^ the moves made by the user
        }

$( makeLenses [''GameState, ''Level] )

-- | Object values needed at level construction time
data ObjectInitValues = ObjectInitValues
        { oiBeardGrowthRate     :: Int
        }


instance Show Object where
        show = (:[]). objectToChar




-- Error

-- | Error in the game
data GameError
        = RuntimeError String                           -- ^ Generic runtime error
        | LevelError String String                      -- ^ Error in the level description: levelname -> message
        | InvalidCharacterError Char                    -- ^ Unknown character (in level description): character
        | InvalidLevelCharacterError String Char        -- ^ Unknown character in level description: level -> character

instance Show GameError where
        show (RuntimeError msg)                 = "Error: " ++ msg
        show (LevelError lvl msg)               = "Error loading level \"" ++ lvl ++ "\":" ++ msg
        show (InvalidCharacterError c)          = "Error: Invalid character encountered: \"" ++ c : "\""
        show (InvalidLevelCharacterError lvl c) = "Error loading level \"" ++ lvl ++ "\", invalid character encountered: \"" ++ c : "\""

instance Error GameError where
        noMsg   = RuntimeError "Unknown error"
        strMsg  = RuntimeError

-- | The Result type - either a error or the "real" value
type Result = Either GameError


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
isLiftOpen (Lift Open)          = True
isLiftOpen _                    = False

isLiftClosed :: Object -> Bool
isLiftClosed (Lift Closed)      = True
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


-- | Converts an object to the corresponding character
objectToChar :: Object -> Char
objectToChar o
        = case o of
                Robot           -> 'R'
                Wall            -> '#'
                Rock Simple     -> '*'
                Rock HigherOrder-> '@'
                Lambda          -> '\\'
                Lift Closed     -> 'L'
                Lift Open       -> 'O'
                Earth           -> '.'
                Trampoline c    -> c
                Target c        -> c
                Beard _         -> 'W'
                Razor           -> '!'
                Empty           -> ' '


-- | Converts a character to the corresponding object,
--   expects ObjectInitValues for e.g. the beard growth rate
charToObject :: ObjectInitValues -> Char -> Result Object
charToObject oiv c
        = case c of
                'R'                     -> return Robot
                '#'                     -> return Wall
                '*'                     -> return $ Rock Simple
                '@'                     -> return $ Rock HigherOrder
                '\\'                    -> return Lambda
                'L'                     -> return $ Lift Closed
                'O'                     -> return $ Lift Open
                '.'                     -> return Earth
                ' '                     -> return Empty
                a | a `elem` ['A'..'I'] -> return $ Trampoline a
                  | a `elem` ['0'..'9'] -> return $ Target a
                'W'                     -> return $ Beard $ oiBeardGrowthRate oiv
                '!'                     -> return Razor
                _                       -> throwError $ InvalidCharacterError c


-- | Mapping of colors/styles to objects
objectColor :: Object -> [SGR]
objectColor o
        = case o of
                Robot           -> return $ SetColor Foreground Dull Blue
                Wall            -> []
                Rock _          -> return $ SetColor Foreground Vivid Red
                Lambda          -> return $ SetColor Foreground Dull Cyan
                Lift Closed     -> return $ SetColor Foreground Dull Green
                Lift Open       -> return $ SetColor Foreground Vivid Green
                Earth           -> []
                Trampoline _    -> return $ SetColor Foreground Vivid Magenta
                Target _        -> return $ SetColor Foreground Dull Magenta
                Beard _         -> []
                Razor           -> []
                Empty           -> []


-- | the water color used for objects underwater
waterColor :: [SGR]
waterColor = return $ SetColor Foreground Vivid Blue

-- Print functions for data structures

-- | Prints the level - 2D level grid and metadata if necessary
printLevel :: GameState -> IO ()
printLevel gs = do
        unless (M.null trams) $ do
                putStrLn "Trampolines:"
                mapM_ (putStrLn . show') $ toList trams
        when (air < (lvl^.waterproof)) $
                putStrLn $ "Air: " ++ show air
        when (razorCount > 0) $
                putStrLn $ "Razors: " ++ show razorCount
        printLevelMap lvl
        where
        lvl                     = levelMap ^%= insert (gs^.robotPosition) Robot $ gs^.level
        trams                   = lvl^.trampolines 
        air                     = gs^.airLeft
        razorCount              = lvl^.razors
        show' (tram, targ)      = show tram ++ " -> " ++ show targ


-- | prints the 2D level(map)
printLevelMap :: Level -> IO ()
printLevelMap l = (sequence_ . printAList . levelToSortedAList) l >> setSGR [ Reset ]
        where
        print' o y = printNoNl' o y >> putStrLn ""
        printNoNl' o y = do
                if y > l^.water then setSGR (objectColor o) else setSGR waterColor
                putChar . objectToChar $ o

        printAList :: [(Position, Object)] -> [IO ()]
        printAList ls@(((x0,y0),o):((x1,_),_):_)
                | x1 < x0 = print' o y0 : (printAList . tail) ls
                | otherwise = printNoNl' o y0 : (printAList . tail) ls
        printAList (((_,y0),o):xs) = printNoNl' o y0 : printAList xs
        printAList [] = [putStrLn ""]

        levelToSortedAList :: Level -> [(Position, Object)]
        levelToSortedAList lvl = sortBy (compareForLevelOutput `on` fst) . toList $ levelMap ^$ lvl -- XXX: point-free?
        
        -- Comparison function for printing the level(map)
        compareForLevelOutput :: Position -> Position -> Ordering
        compareForLevelOutput (x0,y0) (x1,y1)
                | y0 < y1       = GT
                | y0 > y1       = LT
                | x0 > x1       = GT
                | x0 < x1       = LT
                | otherwise     = EQ


-- | Sorting function for updating the level
sortForTraversal :: [Position] -> [Position]
sortForTraversal = sortBy compareForLevelTraversal
        where
        -- Comparison function for updating the gamestate
        compareForLevelTraversal :: Position -> Position -> Ordering
        compareForLevelTraversal (x0,y0) (x1,y1)
                | y0 < y1       = LT
                | y0 > y1       = GT
                | x0 < x1       = LT
                | x0 > x1       = GT
                | otherwise     = EQ
