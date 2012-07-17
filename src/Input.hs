module Input ( UserInput(..), getInput, showKeyMapping, printControls ) where

import Control.Monad ( liftM )
import Data.Char ( toLower )

data UserInput
        = MvLeft
        | MvRight
        | MvUp
        | MvDown
        | MvWait
        | MvAbort
        | MvRestart
        | MvSkip
        deriving Eq

-- Input processing
getInput :: IO UserInput
getInput = liftM processInput getChar


showKeyMapping :: UserInput -> String
showKeyMapping a = case a of
        MvUp      -> "W"
        MvLeft    -> "A"
        MvDown    -> "S" 
        MvRight   -> "D" 
        MvAbort   -> "Q" 
        MvRestart -> "R" 
        MvSkip    -> "N"
        MvWait    -> "E" 


processInput :: Char -> UserInput
processInput c = case toLower c of
        'w' -> MvUp
        'a' -> MvLeft
        's' -> MvDown
        'd' -> MvRight
        'q' -> MvAbort
        'r' -> MvRestart
        'n' -> MvSkip
        _   -> MvWait


printControls :: IO ()
printControls = do
        putStrLn "Controls: "
        putStrLn $ "  " ++ foldr (\e l -> showKeyMapping e ++ l) "" [MvUp,MvLeft,MvDown,MvRight]  ++ " to move"
        putStrLn $ "  " ++ showKeyMapping MvWait                                                  ++ " to wait"
        putStrLn $ "  " ++ showKeyMapping MvRestart                                               ++ " to restart"
        putStrLn $ "  " ++ showKeyMapping MvSkip                                                  ++ " to skip the level"
        putStrLn $ "  " ++ showKeyMapping MvAbort                                                 ++ " to quit"
        putStrLn ""