module ProbabilityFunctions where
import Halatro.Constants
import Halatro.Types
import Data.List

import AuxiliaryFunctions

{-|
Purpose:
    
    * Computes n!

Parameters:
    
    * 1st Parameter (n) : The number top compute the factorial for

Output:
    
    * Returns n!

Notes:
    
    * n must be greater than zero
-}
factorial :: Int -> Double
factorial 0 = 1
factorial n | n > 0     = fromIntegral n * factorial (n - 1)
            | otherwise = 0

{-|
Purpose:
    
    * Computes n Choose r (aka the binomial coeficcient)

Parameters:
    
    * 1st Parameter (n) : The term "n" in the binomial coeficcient
    
    * 2nd Parameter (r) : The term "r" in the binomial coeficcient

Output:
    
    * Returns n Choose r

Notes:
    
    * n must be greater than or equal to r
-}
nCr :: Int -> Int -> Double
nCr n r = factorial n / (factorial r * factorial (n-r))

{-|
Purpose:
    
    * Computes the probability P(X = w) for the Hypergeometric Probability Distribution  X = Hypergeometric(x, y, z);
      where x is the population size, y is the number of desired elements in the population, z is the number of draws,
      w is the target number of successes

Parameters:
    
    * 1st Parameter (x) : The population size
    
    * 2nd Parameter (y) : The number of desired elements in the population
    
    * 3rd Parameter (z) : The number of draws / samples taken from the population
    
    * 4th Parameter (w) : The target number of successes

Output:
    
    * Returns P(X = w) for the Hypergeometric Probability Distribution  X = Hypergeometric(x, y, z)

Notes:
    
    * x must be greater than or equal to y, z and w
    
    * y must be greater than or equal to z and w
    
    * z must be greater than or equal to w
    
    * This function is used to estimate the probabilities of drawing certain hand types. This is because a Hypergerometric Distribution can be used to
      model the probability of picking w desired elements when we sample without replacement z times from a population of size x with 
      y desired elements in the total population.
    
    * See <https://en.wikipedia.org/wiki/Hypergeometric_distribution> for more information
-}
hyperGeometric :: Int -> Int -> Int -> Int -> Double
hyperGeometric x y z w   = (nCr y w * nCr (x-y) (z-w)) / nCr x z


{-|
Purpose:
    
    * Gets the number of cards of a each rank remaining in the population
Parameters:
    
    * 1st Parameter (playedCards) : A list containing the cards which have already been played

Output:
    
    * Returns list containing the number of cards of a each rank remaining in the population

Notes:
    
    * playedCards must contain cards which could feasibly occur. For example playedHands could not contain 5 Aces as we know in a deck of cards
      there are ony 4 cards of each rank

      * Helper function for 'ProbabilityFunctions.probabilityOfHand'
-}
numAvailableCardsByRank :: Hand -> [Int]
numAvailableCardsByRank playedCards = map (\x -> 4 - length (findCardsWithRank x playedCards)) [Two .. Ace]

{-|
Purpose:
    
    * Estimates the probability of getting a certain hand type, taking into account which cards have already been used already

Parameters:
    
    * 1st Parameter (playedCards) : A list containing the cards which have already been played
    
    * 2nd Parameter : The hand type we are estimating the probability for

Output:
    
    * Returns an estimated the probability of getting a certain hand type, taking into account which cards have already been used already

Notes:
    
    * playedCards must contain cards which could feasibly occur. For example playedHands could not contain 5 Aces as we know in a deck of cards
      there are ony 4 cards of each rank

    * Helper function for 'ProbabilityFunctions.probabilityOfImprovement'

    * See "Estimating Probabilities" in the "Halatro AI Approach" section found in <index.html>
-}
probabilityOfHand :: [Card] -> HandType -> Double

probabilityOfHand _ None   = 0;

probabilityOfHand _ HighCard   = 0;

probabilityOfHand playedCards Pair         = sum [hyperGeometric numCards x 5 2 | x <- numAvailableCardsByRank playedCards, x >= 2]
    where numCards                         = 52 - length playedCards

probabilityOfHand playedCards TwoPair     = totalTwoPairHands / totalHands
    where numCards                        = 52 - length playedCards
          rankCombinations                = [(x, y) | [x, y] <- combinations 2 (numAvailableCardsByRank playedCards), x >= 2, y >= 2]
          totalTwoPairHands               = sum [nCr x 2 * nCr y 2 * fromIntegral (numCards - x - y) | (x, y) <- rankCombinations]
          totalHands                      = nCr numCards 5

probabilityOfHand playedCards ThreeOfAKind = sum [hyperGeometric numCards x 5 3 | x <- numAvailableCardsByRank playedCards, x >= 3]
    where numCards                         = 52 - length playedCards

probabilityOfHand playedCards FourOfAKind = sum [hyperGeometric numCards x 5 4 | x <- numAvailableCardsByRank playedCards, x >= 4]
    where numCards                        = 52 - length playedCards

probabilityOfHand playedCards Flush       = sum [hyperGeometric numCards x 5 5 | x <- numAvailableCardsBySuit, x >= 5]
    where numCards                        = 52 - length playedCards
          numAvailableCardsBySuit         = [13 - length (findCardsWithSuit x playedCards) | x <- [Clubs .. Spades]]

probabilityOfHand playedCards FullHouse   = totalTwoPairHands / totalHands
    where numCards                        = 52 - length playedCards
          rankCombinations                = [(x, y) | [x, y] <- combinations 2 (numAvailableCardsByRank playedCards), x >= 2, y >= 3]
          totalTwoPairHands               = (sum [nCr x 2 * nCr y 3 | (x, y) <- rankCombinations]) * 2
          totalHands                      = nCr numCards 5

probabilityOfHand playedCards Straight   = totalStraightHands / totalHands
    where numCards                        = 52 - length playedCards
          combinationsForStraight cards   = product $ map (\x -> 4 - length (findCardsWithRank x playedCards)) cards
          totalStraightHands              = fromIntegral $ sum $ map combinationsForStraight possibleStraights
          totalHands                      = nCr numCards 5
          possibleStraights               = [ [Ace, Two, Three, Four, Five],
                                            [Two, Three, Four, Five, Six],
                                            [Three, Four, Five, Six, Seven],
                                            [Four, Five, Six, Seven, Eight],
                                            [Five, Six, Seven, Eight, Nine],
                                            [Six, Seven, Eight, Nine, Ten],
                                            [Seven, Eight, Nine, Ten, Jack],
                                            [Eight, Nine, Ten, Jack, Queen],
                                            [Nine, Ten, Jack, Queen, King],
                                            [Ten, Jack, Queen, King, Ace] ]

probabilityOfHand playedCards StraightFlush   = probabilityOfHand playedCards Straight * probabilityOfHand playedCards Flush

probabilityOfHand playedCards RoyalFlush   = totalRoyalFlushHands / totalHands
    where numCards                        = 52 - length playedCards
          totalRoyalFlushHands            = fromIntegral $ minimum $ map (\x -> 4 - length (findCardsWithRank x playedCards)) [Ten .. Ace]
          totalHands                      = nCr numCards 5

{-|
Purpose:
    
    * Estimates the probability of getting a better hand than the hand that you currently have

Parameters:
    
    * 1st Parameter (playedCards) : A list containing the cards which have already been played
    
    * 2nd Parameter : The hand type we are estimating the probability for

Output:
    
    * Returns an estimated probability of getting a better hand than the hand that you currently have

Notes:
    
    * Helper function for 'CourseworkOne.probabilityBasedAI'
-}
probabilityOfImprovement :: [Card] -> HandType -> Double
probabilityOfImprovement playedCards currentHandType = sum [probabilityOfHand playedCards x | x <- [nextHandType .. RoyalFlush]]
    where nextHandType | currentHandType == RoyalFlush = RoyalFlush
                       | otherwise                     = succ currentHandType