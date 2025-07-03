
module AuxiliaryFunctions where
import Halatro.Constants
import Halatro.Types
import Data.List

{-|
Purpose: 
    
    * The Ranks in Descending order, Haskell lets me do [Two .. Ace] but not [Ace .. Two] 
    
    * So this is a workaround because I dont want to do reverse [Two .. Ace]

Parameters: N/A

Output:
        
    * Returns a List containing the Ranks in Descending order
-}
ranksDescending :: [Rank]
ranksDescending = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]

{-|
Purpose: 
    
    * Compares two cards for sorting in ascending order, with Ace being considered the highest rank

Parameters:

    * 1st Parameter (x) : The first card we are comparing with the second card

    * 2nd Parameter (y) : The second card we are comparing with the first card

Output:
        
    * Returns an 'Ordering' which will result in a list of cards being sorted in ascending order by rank if used with 'sortBy'

Notes:
        
    * This function is intended to be used by the 'sortBy' function
-}
compareCardsByRankAsc :: Card -> Card -> Ordering
compareCardsByRankAsc x y | rank x > rank y = GT
                          | rank x < rank y = LT
                          | otherwise       = EQ

{-| 
Purpose:

    * Compares two cards for sorting in ascending order, with Ace being considered the lowest rank

Parameters:

    * 1st Parameter (x) : The first card we are comparing with the second card
    
    * 2nd Parameter (y) : The second card we are comparing with the first card 

Output:
    
    * Returns an 'Ordering' which will result in a list of cards being sorted in ascending order by rank if used with 'sortBy'

Notes:
    
    * This function is intended to be used by the 'sortBy' function
-}
compareCardsByRankAscAceLow :: Card -> Card -> Ordering
compareCardsByRankAscAceLow x y | rank x /= Ace && rank y == Ace = GT
                                | rank x == Ace && rank y /= Ace = LT
                                | rank x > rank y                = GT
                                | rank x < rank y                = LT
                                | otherwise                      = EQ


{-| 
Purpose:

    * Compares two cards for sorting in descending order, with Ace being considered the highest rank

Parameters:
    
    * 1st Parameter (x) : The first card we are comparing with the second card
    
    * 2nd Parameter (y) : The second card we are comparing with the first card 

Output:
    
    * Returns an 'Ordering' which will result in a list of cards being sorted in ascending order by rank if used with 'sortBy'

Notes:
    
    * This function is intended to be used by the 'sortBy' function
-}
compareCardsByRankDesc :: Card -> Card -> Ordering
compareCardsByRankDesc x y | rank x < rank y = GT
                           | rank x > rank y = LT
                           | otherwise       = EQ

{-|
Purpose:

    * Finds all the Cards of a certain rank from a list of cards

Parameters:

    * 1st Parameter (target_rank) : The rank that we are searching for

    * 2nd Parameter (hand) : The list of cards to search from

Output:

    * Returns a list of cards containing only cards with the same rank as "target_rank"
-}
findCardsWithRank :: Rank -> Hand -> Hand
findCardsWithRank target_rank = filter (\x -> target_rank == rank x)

{-|
Purpose:
    
    * Finds all the Cards of a certain suit from a list of cards

Parameters:
    
    * 1st Parameter (target_suit) : The suit that we are searching for
    
    * 2nd Parameter (hand) : The list of cards to search from

Output:
    
    * Returns a list of cards containing only cards with the same suit as "target_suit"
-}
findCardsWithSuit :: Suit -> Hand -> Hand
findCardsWithSuit target_suit = filter (\x -> target_suit == suit x)

{-|
Purpose:
    
    * Checks if a list of cards only contains elements which have successive ranks to each other

Parameters:
    
    * 1st Parameter : The list of cards we are checking against

Output:
    
    * Returns true if the list only contains elements which have successive ranks to each other
    
    * Returns false otherwise

Notes:
    
    * The input list should be sorted before being input into this function

    * Helper function for 'contains' when we are checking if the hand type is a 'Straight'
-}
isHandAscending :: Hand -> Bool
isHandAscending (x:y:z:xs) | rank y == Ace && rank x == King = False
                           | rank x == Ace                   = rank y == Two && isHandAscending (y:z:xs)
                           | otherwise                       = rank y == succ (rank x) && isHandAscending (y:z:xs)

isHandAscending [x, y]     | rank x == Ace                   = rank y == Two
                           | otherwise                       = rank y == succ (rank x)

isHandAscending [_]        = False
isHandAscending []         = False

{-|
Purpose:
    
    * Gets a list of cards which are contributing to the score for a given hand type

Parameters:
    
    * 1st Parameter (xs) : The list of cards we are checking against
    
    * 2nd Parameter : The hand type we are checking the scoring cards for

Output:
    
    * Returns a list of cards which are contributing to the score for a given hand type

Notes:
    
    * Only Hands with 5 cards or less should be input to this function

    * Helper function for 'CourseworkOne.whichCardsScore' 
-}
getScoringCards :: Hand -> HandType -> [Card]

getScoringCards [] HighCard     = []

-- Select the card with the highest rank
getScoringCards xs HighCard     = [maximumBy rankComparison xs]
    where rankComparison a b    = compare (rank a) (rank b)

-- Group the Cards by rank and take the first group where the length of that group is greater than or equal to 2
getScoringCards xs Pair         = concat $ take 1 pairsByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending
          pairsByRank           = filter (\x -> length x >= 2) groupedByRank

-- Group the Cards by rank and take the first two groups where the length of that group is greater than or equal to 2
getScoringCards xs TwoPair      = concat $ take 2 pairsByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending
          pairsByRank           = filter (\x -> length x >= 2) groupedByRank

-- Group the Cards by rank and take the first group where the length of that group is greater than or equal to 3
getScoringCards xs ThreeOfAKind = concat $ take 1 threesByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending
          threesByRank          = filter (\x -> length x >= 3) groupedByRank

-- Group the Cards by rank and take the first group where the length of that group is greater than or equal to 4
getScoringCards xs FourOfAKind  = concat $ take 1 foursByRank
    where groupedByRank         = map (`findCardsWithRank` xs) ranksDescending
          foursByRank           = filter (\x -> length x >= 4) groupedByRank

-- For five card hand types just take the five cards because all cards will be contributing to the hand type
getScoringCards xs _ = take 5 xs

{-|
Purpose:
    
    * Generates a list of all possible combinations of size n from a list

Parameters:
    
    * 1st Parameter (n) : size of combinations
    
    * 2nd Parameter (xs) : The list to generate the combinations from

Output:
    
    * Returns a list of all possible combinations of size n from a list

Notes:
    
    * Helper function for 'CourseworkOne.highestScoringHand' 
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [x:ys | ys <- combinations (n-1) xs]  ++ combinations n xs

{-|
Purpose:
    
    * Extracts the list of cards that were played/discarded in the move

Parameters:
    
    * 1st Parameter : The Move to extract the cards from

Output:
    
    * Returns the list of cards that were played/discarded in the move

Notes:
    
    * Helper function for 'CourseworkOne.probabilityBasedAI' 
-}
cardsFromMove :: Move -> [Card]
cardsFromMove (Move _ c) = c

{-|
Purpose:
    
    * Extracts whether we played or discarded in a move

Parameters:
    
    * 1st Parameter : The Move to extract the information from

Output:
    
    * Returns whether we played or discarded in a move

Notes:
    
    * Helper function for 'CourseworkOne.probabilityBasedAI' 
-}
moveType :: Move -> PlayOrDiscard
moveType (Move m _) = m