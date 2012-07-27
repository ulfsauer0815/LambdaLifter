module Input ( UserInput(..), getInput, showKeyMapping, showMoveHistory, printControls, askForContinue, askForContinue_) where

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
        | UiUseRazor
        deriving Eq

-- Input processing
getInput :: IO UserInput
getInput = do
        c <- getChar
        let input = processInput c
        case input of
                Nothing -> getInput
                Just input' -> return input'


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
        UiUseRazor-> "X"
        UiContinue-> "SPACE"


showKeyMappingOfficial :: UserInput -> Char
showKeyMappingOfficial a = case a of
        UiUp      -> 'U'
        UiLeft    -> 'L'
        UiDown    -> 'D'
        UiRight   -> 'R'
        UiWait    -> 'W'
        UiAbort   -> 'A'
        UiUseRazor-> 'S'
        
        -- TODO: check
        UiRestart -> 'a'
        UiSkip    -> 'a'
        UiContinue-> 'a'

showMoveHistory :: [UserInput] -> String
showMoveHistory = map showKeyMappingOfficial  . reverse


processInput :: Char -> Maybe UserInput
processInput c = case toLower c of
        'w' -> return UiUp
        'a' -> return UiLeft
        's' -> return UiDown
        'd' -> return UiRight
        'e' -> return UiWait
        'q' -> return UiAbort
        'r' -> return UiRestart
        'n' -> return UiSkip
        ' ' -> return UiContinue
        'x' -> return UiUseRazor
        _   -> Nothing


askForContinue :: IO () -> IO () -> IO ()
askForContinue actionAbort actionContinue = do
        setSGR [SetBlinkSpeed SlowBlink]
        putStrLn $ "Press " ++ showKeyMapping UiContinue ++ " to continue."
        setSGR [Reset]
        askForContinue'
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
        putStrLn $ "  " ++ showKeyMapping UiUseRazor                                              ++ " to use the razor"
        putStrLn $ "  " ++ showKeyMapping UiWait                                                  ++ " to wait"
        putStrLn $ "  " ++ showKeyMapping UiRestart                                               ++ " to restart"
        putStrLn $ "  " ++ showKeyMapping UiSkip                                                  ++ " to skip the level"
        putStrLn $ "  " ++ showKeyMapping UiAbort                                                 ++ " to quit"
        putStrLn ""