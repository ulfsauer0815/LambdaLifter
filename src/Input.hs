module Input ( UserInput(..), getInput, showKeyMapping, printControls, askForContinue, askForContinue_) where

import Control.Monad ( liftM )
import Data.Char ( toLower )
import System.Console.ANSI ( SGR(..), BlinkSpeed(..), setSGR )

data UserInput
        = UiLeft
        | UiRight
        | UiUp
        | UiDown
        | UiWait
        | UiAbort
        | UiRestart
        | UiSkip
        | UiContinue
        deriving Eq

-- Input processing
getInput :: IO UserInput
getInput = liftM processInput getChar


showKeyMapping :: UserInput -> String
showKeyMapping a = case a of
        UiUp      -> "W"
        UiLeft    -> "A"
        UiDown    -> "S" 
        UiRight   -> "D" 
        UiAbort   -> "Q" 
        UiRestart -> "R" 
        UiSkip    -> "N"
        UiWait    -> "E"
        UiContinue-> "SPACE"  


processInput :: Char -> UserInput
processInput c = case toLower c of
        'w' -> UiUp
        'a' -> UiLeft
        's' -> UiDown
        'd' -> UiRight
        'q' -> UiAbort
        'r' -> UiRestart
        'n' -> UiSkip
        ' ' -> UiContinue
        _   -> UiWait


askForContinue :: IO () -> IO () -> IO ()
askForContinue actionAbort actionContinue = do
        setSGR [SetBlinkSpeed SlowBlink]
        putStrLn $ "Press " ++ showKeyMapping UiContinue ++ " to continue."
        askForContinue'
        setSGR [Reset]
        where
        askForContinue' = do
                a <- getInput
                case a of
                        UiContinue -> actionContinue
                        UiAbort    -> actionAbort
                        _          -> askForContinue'


askForContinue_ :: IO () -> IO ()
askForContinue_ = askForContinue (return ())


printControls :: IO ()
printControls = do
        putStrLn "Controls: "
        putStrLn $ "  " ++ foldr (\e l -> showKeyMapping e ++ l) "" [UiUp,UiLeft,UiDown,UiRight]  ++ " to move"
        putStrLn $ "  " ++ showKeyMapping UiWait                                                  ++ " to wait"
        putStrLn $ "  " ++ showKeyMapping UiRestart                                               ++ " to restart"
        putStrLn $ "  " ++ showKeyMapping UiSkip                                                  ++ " to skip the level"
        putStrLn $ "  " ++ showKeyMapping UiAbort                                                 ++ " to quit"
        putStrLn ""