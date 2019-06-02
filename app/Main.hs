

module Main where

import System.Random
import System.Random.Shuffle (shuffle')



data CardType =  Diamond | Club | Heart | Spade deriving Show
    
-- 
typeLenght = [1..14]

-- 
cardTypeList = [Diamond, Club, Heart, Spade]

data Card = Card {
    value   :: Int,
    ctype    :: CardType
} deriving Show


getRandomNumber :: IO Int
getRandomNumber = randomRIO (1, 56)


generateDeck :: [CardType] -> [Card]
generateDeck [] = []
generateDeck types = generateCardsOfType (head types) ++ generateDeck (tail types)


generateCardsOfType :: CardType -> [Card]
generateCardsOfType cardtype = [ Card x cardtype | x <- typeLenght]


getCardFrom :: [Card] -> Int -> Card
getCardFrom cards pos = cards!!pos


cardList :: [Card]
cardList = generateDeck cardTypeList

-- main = do
    
twentyone = do
    stdgen <- newStdGen
    shuffle' cardList (length cardList) stdgen


main = putStrLn "Hello to 21 game"