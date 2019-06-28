--  File     : Proj1.hs
--  Author   : Tong-Ing Wu
--  Login ID : TONGINGW
--  Origin   : Mon April 15 14:00 2019
--  Purpose  : Try to guess the pitches selected by a composer with a minimum
--             of attempts
--
-- |This haskell program implements a game called Musician. The composer begins
--  by selecting a three-pitch musical chord, where each pitch comprise a
--  musical note and an octave. For example, ("A1","B2","C3").
--  The performer repeatedly choose a defined chord as a guess and give
--  it to the composer. The composer then responds the performer by giving
--  a feedback.
--  The goal of this program is to minimize the number of attempts.
--  This program implements three strategies to minimize the number of
--  attempts, including a naive approach, the fanout approach,
--  and the Gini index approach.

module Proj1 (Pitch, toPitch, feedback,
                GameState, initialGuess, nextGuess) where

import Data.Char
import Data.Ord
import Data.List

-- | The program uses Pitch to represent each pitch and the
--   GameState to hold possible guesses based on current information.

data Pitch = Pitch {
    pitchNote :: Char
    , pitchOctave :: Char
    } deriving(Eq, Ord)

data GameState = GameState {
    remainGuess :: [[Pitch]]
    } deriving(Show)

instance Show Pitch where show = showPitch

-- | A function defines how the Pitch is showed.
showPitch :: Pitch -> String
showPitch x = pitchNote x : pitchOctave x : []

-- | A function will generate a Pitch if the provided String is valid.
toPitch :: String -> Maybe Pitch
toPitch str =
    if (length str) == 2 && elem (head str) ['A'..'G'] && elem (last str) ['1'..'3']
    then
        Just $ Pitch { pitchNote = (head str), pitchOctave = (last str) }
    else
        Nothing

-- | A function will give appropriate feedback based on a target and a guess.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (right, note, oct)
    where
        intersectNote = realIntersect (returnNote target) (returnNote guess)
        intersectOct = realIntersect (returnOct target) (returnOct guess)
        right = length (intersect target guess)
        note = abs(right - length intersectNote)
        oct = abs(right - length intersectOct)

-- | Generate an initial guess. The initial guess is always (A1, B2, C3)
--   and the initial GameState is always an list holds all possible guess
initialGuess :: ([Pitch],GameState)
initialGuess = ([ Pitch 'A' '1', Pitch 'B' '2' ,Pitch 'C' '3' ]
                , GameState (genCombinations 3 createSet))

-- | The function will generate next guess based on the previous guess by
--   removing candidates that have no chance to be the correct answer and
--   pick the next guess with provided approaches.
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (preGuess, preGameState) score = (nextGuess, nextGameState)
    where
        -- remove candidates that have no chance to be correct answer.
        nextGameState = GameState [ result
                                  | result <- remainGuess preGameState
                                  , feedback preGuess result == score]
        -- The following provides three approaches to generate next guess,
        -- 1. naive approach, pick first.
        --nextGuess = (remainGuess nextGameState) !!0
        -- 2. Fanout approach.
        --nextGuess = pickBestGuessFanout (remainGuess nextGameState)
        -- 3. GiniIndex approach.
        nextGuess = pickBestGuessGini (remainGuess nextGameState)

-- | This function will return the best guess by calculating scores of
--   all remaining possible guesses with Fanout approach.
pickBestGuessFanout :: [[Pitch]] -> [Pitch]
pickBestGuessFanout state = bestGuess
    where
        expected =  [ (score, guess) | guess <- state
                    , let score = calculateFanout guess state ]
        bestGuess = snd $ last (sort expected)

-- | This function will calculating the score of a guess with Fanout approach.
calculateFanout :: [Pitch] -> [[Pitch]] -> Int
calculateFanout target state = score
    where
        score = (length . group . sort) (map (feedback target) state)

-- | This function will return the best guess by calculating the score of
--   all remaining possible guesses with the Gini index approach.
pickBestGuessGini :: [[Pitch]] -> [Pitch]
pickBestGuessGini state = bestGuess
    where
        expected =  [ (score, guess) | guess <- state
                    , let score = calculateGini guess state ]
        bestGuess = snd $ head (sort expected)

--  This function will calculating the score of a guess with the .
--  Gini index approach.
calculateGini :: [Pitch] -> [[Pitch]] -> Int
calculateGini target state = sum [ numResults * numResults
                                 | clt <- collected
                                 , let numResults = length (clt) ]
    where
        scores = [score | guess <- state, let score = feedback target guess ]
        collected = (group . sort) scores

-- | Return a list of pitch notes from a Pitch.
returnNote :: [Pitch] -> [Char]
returnNote a = (map pitchNote a)

-- | Return a list of pitch octaves from a Pitch.
returnOct :: [Pitch] -> [Char]
returnOct a = (map pitchOctave a)

-- | Return the real intersect of two lists.
realIntersect :: (Eq a) => [a] -> [a] -> [a]
realIntersect x y = x \\ (x \\ y)

-- | Generate all combinations with a given list and an integer.
genCombinations :: Int -> [a] -> [[a]]
genCombinations 0 _  = [ [] ]
genCombinations n xs = [ y:ys | y:xs' <- tails xs
                       , ys <- genCombinations (n-1) xs']

-- | Create a list of possible Pitches.
createSet :: [Pitch]
createSet = map createPitch [ "A1","A2","A3","B1","B2","B3","C1","C2","C3","D1"
                            ,"D2","D3","E1","E2","E3","F1","F2","F3","G1","G2"
                            ,"G3"]

-- | Create a Pitch with a given String.
createPitch :: String -> Pitch
createPitch x = Pitch (head x) (last x)

