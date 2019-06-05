

module Main where

import System.Random



data CardType =  Diamond | Club | Heart | Spade deriving (Show, Eq)
    
-- 
typeLength = [1..14]

-- 
cardTypeList = [Diamond, Club, Heart, Spade]

data Card = Card {
    value   :: Int,
    ctype    :: CardType
} deriving (Show, Eq)

cprint::String-> IO()
cprint str = do 
    putStr str
    putStr "\n"

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)


generateDeck :: [CardType] -> [Card]
generateDeck [] = []
generateDeck types = generateCardsOfType (head types) ++ generateDeck (tail types)


generateCardsOfType :: CardType -> [Card]
generateCardsOfType cardtype = [ if x < 10 then Card x cardtype else Card 10 cardtype | x <- typeLength ]


cardList :: [Card]
cardList = generateDeck cardTypeList

getCardFromMaze :: [Card] -> Int -> Card
getCardFromMaze maze position = maze !! position

removeCard :: [Card] -> Card -> [Card]
removeCard maze card = [x | x <- maze, x /= card]

addCard :: [Card] -> Card -> [Card]
addCard maze card = [card] ++ maze

sumMaze :: [Card] -> Int
sumMaze [] = 0
sumMaze ((Card value _):xs) = value + (sumMaze xs)


finalSum :: Int -> [Card] -> Int
finalSum sum cards 
    | sum + 10 < 21 && length cards > 0 = finalSum (sum+10) tailcards
    | otherwise = sum
    where tailcards = tail cards

getOnes :: [Card] -> [Card]
getOnes cards = filter ( ( == 1) . value ) cards



casino:: [Card] -> [Card] -> [Card] -> IO()
casino cards pcards mcards = do
    cprint "Welcome to the casino :)"

    cprint "Player takes card"
    x <- getRandomNumber 0 (length cards - 1)
    addCard pcards $ getCardFromMaze cards x

    cprint "end game"

main = casino cardList [] []