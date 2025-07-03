-- NOTE: PLEASE READ THE DOCUMENTATION FOUND IN "../docs/index,.html", the preamble found under the heading "Halatro AI Approach is important"

module CourseworkOne where
import Halatro.Constants
import Halatro.Types
import Data.List
import AuxiliaryFunctions
import ProbabilityFunctions

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

{-|
Purpose:
    
    * Check whether a played hand is a certain hand type

Parameters:
    
    * 1st Parameter (xs) : The list of cards to check from
    
    * 2nd Parameter : Hand type to check against

Output:
    
    * Returns whether a played hand is a certain hand type

Note:
    
    * Only Hands with 5 cards or less should be input to this function
-}
contains :: Hand -> HandType -> Bool

contains [] None                = True

contains _ None                 = False

-- If the hand forms no other valid hand types then return true for HighCard
contains xs HighCard            = not $ any (contains xs) [Pair .. RoyalFlush]

-- Group the Cards By Rank, if there exists a group with 2 or more cards then the hand contains a Pair
contains xs Pair                = any (\x -> length x >= 2) groupedByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending

-- Group the Cards By Rank, if there exists 2 groups with 2 or more cards then the hand contains a TwoPair
contains xs TwoPair             = length pairsByRank >= 2
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending
          pairsByRank           = filter (\x -> length x >= 2) groupedByRank

-- Group the Cards By Rank, if there exists a group with 3 or more cards then the hand contains a ThreeOfAKind
contains xs ThreeOfAKind        = any (\x -> length x >= 3) groupedByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending

-- Group the Cards By Rank, if there exists a group with 4 or more cards then the hand contains a FourOfAKind
contains xs FourOfAKind         = any (\x -> length x >= 4) groupedByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending

-- if the list has a length of five and only contains elements which have successive ranks to each other
contains xs Straight            = length xs == 5 && isAscending
    where sortedByRankAsc       = sortBy compareCardsByRankAsc xs
          sortedByRankAscAceLow = sortBy compareCardsByRankAscAceLow xs
          isAscending           = isHandAscending sortedByRankAsc || isHandAscending sortedByRankAscAceLow

-- Group the Cards By Suit, if there exists a group with 5 or more cards then the hand contains a Flush
contains xs Flush               = any (\x -> length x >= 5) cardsOfSuit
    where cardsOfSuit           = map (`findCardsWithSuit` xs) [Clubs .. Spades]

-- Group the Cards By Rank, if there exists a group with 3 or more cards and a group with 2 cards then the hand contains a Full House
contains xs FullHouse           = any (\x -> length x >= 3) groupedByRank && any (\x -> length x == 2) groupedByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending

-- If the hand contains a Straight and a Flush then it contains a straight flush
contains xs StraightFlush       = contains xs Straight && contains xs Flush

-- If we get a list where we have 1 of each card from Ten to Ace that exists in xs and that list contains a flush then we have a royal flush
contains xs RoyalFlush          = contains royalCards Flush
    where royalCards            = concat [take 1 (findCardsWithRank x xs) | x <- [Ten .. Ace]]

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

{-|
Purpose:
    
    * Identify the highest value hand type in a played hand

Parameters:
    
    * 1st Parameter (xs) : The list of cards to check from

Output:
    
    * Returns the highest value hand type in a played hand
-}
bestHandType :: Hand -> HandType
bestHandType xs = maximum $ filter (contains xs) [None .. RoyalFlush]

--------------------------------------------------------------------------------
-- Part 3: score a played hand

{-|
Purpose:
    
    * Identify cards in hand the that contribute to the best hand type

Parameters:
    
    * 1st Parameter (xs) : The list of cards to check from

Output:
    
    * Returns a list containing the cards in hand the that contribute to the best hand type
-}
whichCardsScore :: Hand -> [Card]
whichCardsScore xs = getScoringCards xs (bestHandType xs)

{-|
Purpose:
    
    * Calculates the score of a hand based on the formula /[(base score + sum of rank scores) * multiplier/]

Parameters:
    
    * 1st Parameter (xs) : The list of cards to check from

Output:
    
    * Returns the score of a hand
-}
scoreHand :: Hand -> Int
scoreHand xs    = (base + bonus) * mult
    where best  = bestHandType xs
          base  = fst (handTypeValues best)
          mult  = snd (handTypeValues best)
          bonus = sum $ map (rankScore . rank) (getScoringCards xs best)

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

{-|
Purpose:
    
    * Identify the highest scoring hand of 5 cards out of n>=5 cards by scoring all combinations of size 5 and playing the highest scoring combination

Parameters:
    
    * 1st Parameter (xs) : The list of cards to check from

Output:
    
    * Returns the highest scoring hand of 5 cards out of n>=5 cards
-}
highestScoringHand :: [Card] -> Hand

highestScoringHand xs         | not $ null allCombinations = maximumBy scoreComparison allCombinations
                              | otherwise                  = xs
    where sortedByRankDesc    = sortBy compareCardsByRankDesc xs
          allCombinations     = combinations 5 sortedByRankDesc
          scoreComparison a b = compare (scoreHand a) (scoreHand b)

--------------------------------------------------------------------------------    
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

{-|
Purpose:
    
    * An AI implementation for Halatro where we always play the 5 highest ranked cards

Parameters:

    * 1st Parameter (moves) : Previous Moves that have been played

    * 2nd Parameter (cards) : The cards in our current hand

Output:
    
    * Returns the move we should play
-}
simpleAI :: [Move] -> [Card] -> Move
simpleAI _ xs              = Move Play handToPlay
    where sortedByRankDesc = sortBy compareCardsByRankDesc xs
          handToPlay       = take 5 sortedByRankDesc

{-|
Purpose:
    
    * An AI implementation for Halatro where we always play the best hand

Parameters:

    * 1st Parameter (moves) : Previous Moves that have been played

    * 2nd Parameter (cards) : The cards in our current hand

Output:
    
    * Returns the move we should play
-}
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ xs = Move Play (highestScoringHand xs)

{-|
Purpose:
    
    * An AI implementation for Halatro where we discard if our probability of improving the current hand is greater than a threshold value
    
    * When we play, we play the cards returned by 'CourseworkOne.highestScoringHand'

    * When we discard, we discard as many cards as possibe given that the rank of the card is less than a Ten

Parameters:
    
    * 1st Parameter (pMin) : Discard probability threshold
    
    * 2nd Parameter (moves) : Previous Moves that have been played

    * 3rd Parameter (cards) : The cards in our current hand

Output:
    
    * Returns the move we should play

Notes:
    
    * See 'ProbabilityFunctions.probabilityOfImprovement' and 'ProbabilityFunctions.probabilityOfHand' for more information on probability calculations

    * Helper function for 'CourseworkOne.myAI'
-}
probabilityBasedAI :: Double -> [Move] -> [Card] -> Move
probabilityBasedAI pMin moves cards    | shouldDiscard = Move Discard cardsToDiscard
                                       | otherwise     = Move Play cardsToPlay
    where playedCards                  = (concat [cardsFromMove x | x <- moves]) ++ cardsToDiscard
          badCards                     = [x |x <- cards, rankScore (rank x) < rankScore Ten]
          cardsToDiscard               = sortBy compareCardsByRankAsc badCards
          cardsToPlay                  = highestScoringHand cards
          canDiscard                   = length [x | x <- moves, moveType x == Discard] < 3
          shouldDiscard                = (probabilityOfImprovement playedCards (bestHandType cardsToPlay) > pMin) && canDiscard

{-|
Purpose:
    
    * My Halatro AI implementation, uses 'probabilityBasedAI'

Parameters:

    * 1st Parameter (moves) : Previous Moves that have been played

    * 2nd Parameter (cards) : The cards in our current hand

Output:
    
    * Returns the move we should play
-}
myAI :: [Move] -> [Card] -> Move
myAI = probabilityBasedAI 0.01

-- NOTE: PLEASE READ THE DOCUMENTATION FOUND IN "/docs/index,.html", the preamble found under the heading "Halatro AI Approach" is important