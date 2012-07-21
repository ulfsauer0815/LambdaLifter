module Input ( UserInput(..), getInput, showKeyMapping, printControls, askForContinue, askForContinue_) where

import Control.Monad ( liftM )
import Data.Char ( toLower )
import System.Console.ANSI ( SGR(..), BlinkSpeed(..), setSGR )

data UserInput
        = MvLeft
        | MvRight
        | MvUp
        | MvDown
        | MvWait
        | MvAbort
        | MvRestart
        | MvSkip
        | MvContinue
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
        MvContinue-> "SPACE"  


processInput :: Char -> UserInput
processInput c = case toLower c of
        'w' -> MvUp
        'a' -> MvLeft
        's' -> MvDown
        'd' -> MvRight
        'q' -> MvAbort
        'r' -> MvRestart
        'n' -> MvSkip
        ' ' -> MvContinue
        _   -> MvWait


askForContinue :: IO () -> IO () -> IO ()
askForContinue actionAbort actionContinue = do
        setSGR [SetBlinkSpeed SlowBlink]
        putStrLn $ "Press " ++ showKeyMapping MvContinue ++ " to continue."
        askForContinue'
        setSGR [Reset]
        where
        askForContinue' = do
                a <- getInput
                case a of
                        MvContinue -> actionContinue
                        MvAbort    -> actionAbort
                        _          -> askForContinue'


askForContinue_ :: IO () -> IO ()
askForContinue_ = askForContinue (return ())


printControls :: IO ()
printControls = do
        putStrLn "Controls: "
        putStrLn $ "  " ++ foldr (\e l -> showKeyMapping e ++ l) "" [MvUp,MvLeft,MvDown,MvRight]  ++ " to move"
        putStrLn $ "  " ++ showKeyMapping MvWait                                                  ++ " to wait"
        putStrLn $ "  " ++ showKeyMapping MvRestart                                               ++ " to restart"
        putStrLn $ "  " ++ showKeyMapping MvSkip                                                  ++ " to skip the level"
        putStrLn $ "  " ++ showKeyMapping MvAbort                                                 ++ " to quit"
        putStrLn ""