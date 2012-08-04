module Input ( UserInput(..), getInput, showKeyMapping, showMoveHistory, printControls, askForAction, askForContinue, askForContinue_) where

import           Data.Char           (toLower)
import           Data.Map            (Map, findWithDefault, fromList)
import           Data.Maybe          (mapMaybe)
import           System.Console.ANSI (BlinkSpeed(..), SGR(..), setSGR)

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
        deriving (Eq, Ord)

instance Show UserInput where
        show = showKeyMapping


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


showKeyMappingOfficial :: UserInput -> Maybe Char
showKeyMappingOfficial a = case a of
        UiUp      -> return 'U'
        UiLeft    -> return 'L'
        UiDown    -> return 'D'
        UiRight   -> return 'R'
        UiWait    -> return 'W'
        UiAbort   -> return 'A'
        UiUseRazor-> return 'S'

        UiRestart -> Nothing
        UiSkip    -> Nothing
        UiContinue-> Nothing

showMoveHistory :: [UserInput] -> String
showMoveHistory = mapMaybe showKeyMappingOfficial  . reverse


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
askForContinue actionAbort actionContinue
        = askForAction
                (fromList [(UiContinue, actionContinue), (UiAbort, actionAbort)])
                $ "Press " ++ showKeyMapping UiContinue ++ " to continue."


-- (Map input (action to perform)) -> question to ask
askForAction :: Map UserInput (IO ()) -> String -> IO ()
askForAction actionMap text = do
        setSGR [SetBlinkSpeed SlowBlink]
        putStrLn text
        setSGR [Reset]
        getInput'
        where
        getInput' = do
                a <- getInput
                findWithDefault getInput' a actionMap


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
