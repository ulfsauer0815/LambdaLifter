module Main where

import Data.List
import Data.Map (Map,fromList,toList)


-- Data tyes

data Object
        = Robot
        | Wall
        | Rock
        | Lambda
        | LiftOpen
        | LiftClosed
        | Earth
        | Empty

instance Show Object where
        show = (:[]). objectToChar


type Level = Map (Integer, Integer) Object


data Movement
        = Left
        | Right
        | Up
        | Down
        | Wait
        | Abort


-- Levels

lvl1s :: [String]
lvl1s =
        [ "######"
        , "#. *R#"
        , "#  \\.#"
        , "#\\ * #"
        , "L  .\\#"
        , "######" ]

lvl1 :: Level
lvl1 = levelStringToMap lvl1s


-- Functions

objectToChar :: Object -> Char
objectToChar o
        = case o of
                Robot -> 'R'
                Wall -> '#'
                Rock -> '*' 
                Lambda -> '\\'
                LiftClosed -> 'L'
                LiftOpen -> 'O'
                Earth -> '.'
                Empty -> ' '



charToObject :: Char -> Object
charToObject c
        = case c of
                'R'  -> Robot
                '#'  -> Wall
                '*'  -> Rock
                '\\' -> Lambda
                'L'  -> LiftClosed
                'O'  -> LiftOpen
                '.'  -> Earth
                ' '  -> Empty
                _    -> error "Invalid map sequence"


levelStringToMap :: [String] -> Level
levelStringToMap sl = fromList . concatMap (\(y, s) -> zip [(x,y) | x <- [1..]] (map charToObject s)) $ zip [1..] (reverse sl)
 



printLevel :: Level -> IO ()
printLevel = sequence_ . printAList . levelToSortedAList
        where
        printAList :: [((Integer, Integer), Object)] -> [IO ()]
        printAList ls@(((x0,_),o):((x1,_),_):_)
                | x1 < x0 = print o : (printAList . tail) ls
                | otherwise = (putChar . objectToChar) o : (printAList . tail) ls
        printAList (((_,_),o):xs) = (putChar . objectToChar) o : printAList xs
        printAList [] = []

        levelToSortedAList :: Level -> [((Integer, Integer), Object)]
        levelToSortedAList = sortBy levelMapOutputSort . toList
        
        levelMapOutputSort ((x0,y0),_) ((x1,y1),_)
                | y0 < y1       = GT
                | x0 > x1       = GT
                | otherwise     = LT


updateLevel :: Level -> Level
updateLevel = undefined


main :: IO ()
main = undefined