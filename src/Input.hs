-- | This module contains the input related functions.
module Input ( UserInput(..), getInput, showKeyMapping, showMoveHistory, printControls, askForAction, askForContinue, askForContinue_) where

import           Data.Char           (toLower)
import           Data.Map            (Map, findWithDefault, fromList)
import           Data.Maybe          (mapMaybe)
import           System.Console.ANSI (BlinkSpeed(..), SGR(..), setSGR)


-- | The actions available to a user.
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

-- | Reads the users keystokes until a valid action is found.
getInput :: IO UserInput
getInput = do
        c <- getChar
        let input = processInput c
        case input of
                Nothing -> getInput
                Just input' -> return input'
        where
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


-- | The keymappings that can be printed in help messages.
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


-- | The "official" keymapping defined in the ICFP task specification.
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


-- | Generates a String of the moves made with the official keymappings.
--   Can be used to verify routes with the official validator
showMoveHistory :: [UserInput] -> String
showMoveHistory = mapMaybe showKeyMappingOfficial  . reverse


-- | Asks the User if he wants to continue and executes the corresponding IO actions.
askForContinue :: IO () -> IO () -> IO ()
askForContinue actionAbort actionContinue
        = askForAction
                (fromList [(UiContinue, actionContinue), (UiAbort, actionAbort)])
                $ "Press " ++ showKeyMapping UiContinue ++ " to continue."


-- | Prints the text to the user and executes the IO actions depending on which one the user chose (defined in the map).
--   (Map input (action to perform)) -> question to ask
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


-- | Default "Press space to continue" question.
askForContinue_ :: IO () -> IO ()
askForContinue_ = askForContinue (return ())


-- | Prints the available user controls.
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