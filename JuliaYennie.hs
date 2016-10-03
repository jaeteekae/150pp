{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}
module PPOne
where

import Data.Ratio
import Data.List

{-

    Comp 150 PPL Programming Problems
    By: Julia Knight and Yennie Jun
    Date: September 24, 2016 

    To see our answers for the questions we answered, please run
    the following commands once everything has been run:

    Simple Distribution and Probability Questions:
        probabilityQuestionA_d6
        probabilityQuestionA_d12

    Dice and Coins:
        probabilityQuestionF

    Tally-sheet Questions:
        tallyProbabilityN
        tallyProbabilityO

    Gambling Question:
        expected_money

    Note: theoretically, the tally sheet and gambling answers are correct.
        However, we could not get the state space small enough for the
        computations to terminate

-}

--TYPES
type Dist a = [(Rational, a)]

dEq :: Eq a => Dist a -> Dist a -> Bool
dEq [] [] = True
dEq x y = 
    if length x /= length y 
        then False 
    else if head x `elem` y 
        then tail x == filter (\a -> a /= head x) y
    else False

data Coin = Heads | Tails deriving (Show, Eq)
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show, Eq)

--COIN
coins :: Dist Coin
coins = [(1%2, Heads), (1%2, Tails)]
coinlist :: Dist [Coin]
coinlist = [(1%2, [Heads]), (1%2, [Tails])]

--MAKE A DISTRIBUTION
uniform :: [a] -> Dist a
uniform xs = let l = genericLength xs
             in map (\x -> ((1%l)::Rational, x)) xs

--DICE
d1 :: Dist Integer
d1 = uniform [1..1]
d2 :: Dist Integer
d2 = uniform [1..2]
d3 :: Dist Integer
d3 = uniform [1..3]
d4 :: Dist Integer
d4 = uniform [1..4]
d5 :: Dist Integer
d5 = uniform [1..5]
d6 :: Dist Integer
d6 = uniform [1..6]
d8 :: Dist Integer
d8 = uniform [1..8]
d10 :: Dist Integer
d10 = uniform [1..10]
d12 :: Dist Integer
d12 = uniform [1..12]
d20 :: Dist Integer
d20 = uniform [1..20]

--Probability distribution of one d6 roll
probabilityQuestionA_d6 :: Dist Integer
probabilityQuestionA_d6 = d6
probabilityQuestionA_d12 :: Dist Integer
probabilityQuestionA_d12 = d12


-------FUNCTIONS FOR CREATING JOINT DISTRIBUTIONS-------

--Join two distribution lists using mapping function f
--TODO: only one dist as input? DistElement a, or just a? get rid of Distelement, just arrays. no foldables
--dmap :: ((Rational, a)->(Rational, b)) -> Dist a -> Dist b
--dmap f dist = let Dist xs = dist
--              in Dist (map f xs)
--join d1 d2 f = concat (dmap (f d2) d1)


join :: Dist a -> Dist b -> (Dist b -> (Rational, a) -> Dist c) -> Dist c
join d1 d2 f = concat (map (f d2) d1)

--Multiplies probabilities & cons
sq :: Dist [a] -> (Rational, [a]) -> Dist [a]
sq d2s (prob, d) = map (\(a, b) -> (prob*a, b++d)) d2s

--Multiplies probabilities & tuple
sqdiff :: Dist a -> (Rational, b) -> Dist (a,b)
sqdiff d2s (p1, b) = map (\(p2, a) -> (p1*p2, (a,b))) d2s

--D2 distribution depends on D1 value
dep :: (Eq b, Num b) => Dist [a] -> (Rational, b) -> Dist [a]
dep d2s (p1, 1) = map (\(p2, x) ->  (p1*p2, x)) d2s
dep d2s (p, n) = join d2s (dep d2s (p, n-1)) sq

--Adding two values
add :: Num a => Dist a -> (Rational, a) -> Dist a
add d2s (p1, x) = map (\(p2, y) -> (p1*p2, x+y)) d2s

--Join a distribution with itself n times
multiple_join :: Dist [a] -> Integer -> Dist [a]
multiple_join dist 1 = dist
multiple_join dist n = multiple_join (join dist dist sq) (n-1)

--A join specifically for adding rolling two dice together
dice_tup :: (Num a, Num b) => Dist (a, Dist b) -> (Rational, (a, Dist b)) -> Dist (a, a, Dist b)
dice_tup d2s (p1, (die1,dist1)) =
    map (\(p2, (die2,dist2)) -> (p1*p2, (die1,die2,(join dist1 dist2 add)))) d2s


------EXAMPLES OF JOINT DISTRIBUTIONS------

--Joint distribution of two coin flips
cxc :: Dist [Coin]
cxc = join coinlist coinlist sq

--Joint distribution of a d6 roll and coin flips that depend on the outcome
distd6 :: Dist [Coin]
distd6 = join d6 coinlist dep

{-}
-------FILTERING A DISTRIBUTION-------

-- Determine how many heads/tails are flipped in the list  
-- Coin -> Coin list -> int 
determine :: Coin -> [Coin] -> Int 
determine ht coinlist = foldl (\acc x -> if (x==ht) then acc + 1 else acc) 0 coinlist  

-- Int -> Coin -> Dist list -> Dist list 
makeFinalList :: Int -> Coin -> Dist [Coin] -> Dist [Coin]
makeFinalList numFlips flipType dlist = filter (\(_, flipList) -> (determine flipType flipList == numFlips)) dlist

-- Adds all of the probabilities of flipping numFlips Heads/Tails
-- Int -> Coin -> Dist List -> Rational
probSum :: Int -> Coin -> Dist [Coin] -> Rational
probSum numFlips flipType distType = toRational (sum (map (\(prob, _) -> prob) (makeFinalList numFlips flipType distType)))

--The probability of seeing 3 heads when a d6 roll determines the number of coin tosses
-- Rational

probabilityQuestionF :: Rational
probabilityQuestionF = probSum 3 Heads distd6
-}

-------TALLY SHEET-------

--Pot of dice distribution
pot :: Dist (Die, Dist Integer)
pot = [((9%46), (D6,d6)), ((9%46) (D8,d8)), ((14%46) (D12,d12)), ((14%46) (D20,d20))]

--WE ARE DOING WITH REPLACEMENT 
--Probability of drawing any two dice
--probDraw :: [Dist (Die, Die, [Dist Int])]
probDraw :: Dist (Die, Die, [DistElement Integer])
probDraw = let draw2 = join pot pot dice_tup
            in map (\(prob, (d1, d2, dist)) -> (prob, (d1, d2, (mergeRolls sameRoll dist)))) draw2

--Probability of drawing two d12s, filtered to decrease problem size
twoD12s :: Dist (Die, Die, [DistElement Integer])
twoD12s = filter (\(_ ,(a,b,_)) -> ((a==D12) && (b == D12))) probDraw
d12Prob :: Rational
b :: (Die, Die, [DistElement Integer])
(d12Prob, b) = head twoD12s

twoD12s_rollProbs :: Dist Integer
twoD12s_rollProbs = join d12 d12 add

{-}


--Functions to use with mergeRolls
sameRoll :: Eq a => DistElement a -> DistElement a -> Bool
sameRoll (Node _ b) (Node _ d) = b==d
sameTally :: (Eq a, Eq a1, Eq a2) => DistElement (a, a1, a2) -> DistElement (a, a1, a2) -> Bool
sameTally (Node _ (l1, e1, g1)) (Node _ (l2, e2, g2)) = (l1==l2)&&(e1==e2)&&(g1==g2)
sameDice :: Eq a => DistElement (a, a, t) -> DistElement (a, a, t1) -> Bool
sameDice (Node _ (d_a1, d_a2, _)) (Node _ (d_b1, d_b2, _)) = ((d_a1==d_b1)&&(d_a2==d_b2))||((d_a1==d_b2)&&(d_a2==d_b1))

--For merging distribution Nodes with the same value
mergeRolls :: (DistElement t -> DistElement t -> Bool) -> [DistElement t] -> [DistElement t]
mergeRolls f xs = case xs' of
             [] -> []
             x'@(_:[]) -> x'
             x'@(_:_:[]) -> x'
             _'@(x1:x2:xs) -> x1:(mergeRolls f (x2:xs))
    where xs' = aux (head xs) (tail xs) []
          aux acc [] temp = (acc:temp)
          aux acc (y:ys) temp = if (f acc y)
                                then 
                                    let (Node p1 r1) = acc
                                        (Node p2 _) = y
                                    in aux (Node (p1+p2) r1) (ys ++ temp) []
                                else aux acc ys (y:temp)

compactD12s :: Dist Integer
compactD12s = mergeRolls sameRoll twoD12s_rollProbs 

twoD12s_rollProbsl :: Dist [Integer]
twoD12s_rollProbsl = map (\(Node prob a) -> (Node prob [a])) compactD12s

--Distribution of two d12's rolled 30 times
d12s_30rolls :: Dist [Integer]
d12s_30rolls = multiple_join twoD12s_rollProbsl 30

testlist :: Dist [Integer]
testlist = [(Node (1%2) [2 .. 24]), (Node (1%2) [16..18]), (Node (1%2) [19..21]), (Node (1%2) [19..22])]

-- Int List -> Operator -> Int 
compareSixteen :: (Num b, Num a1, Foldable t) => t a -> (a -> a1 -> Bool) -> b
compareSixteen sumList op = foldl(\acc x -> if (op x 16) then acc + 1 else acc) 0 sumList

-- Int -> Dist List -> Operator -> Int 
determineHowMany :: (Eq a, Num a, Num a2, Foldable t) => a -> [DistElement (t a1)] -> (a1 -> a2 -> Bool) -> [DistElement (t a1)]
determineHowMany numOverUnder dlist op = filter (\(Node _ sumList) -> compareSixteen sumList op == numOverUnder) dlist

-- Adds: probability of the two D12s added PLUS probabilty of drawing 2 D12s
-- Int -> Dist List -> Operator -> Rational
tallyProb :: (Num a2, Foldable t) => Integer -> [DistElement (t a1)] -> (a1 -> a2 -> Bool) -> Rational
tallyProb numOverUnder dlist op = toRational(sum (map(\(Node prob _)->prob)(determineHowMany numOverUnder dlist op))) + d12Prob

-- Rational
tallyProbabilityN :: Rational
tallyProbabilityN = tallyProb 3 d12s_30rolls (>)
tallyProbabilityO :: Rational
tallyProbabilityO = tallyProb 16 d12s_30rolls (<)
--NOTE: theoretically, tallyProbabilityN is the correct probability. However, we could not get the calculation to terminate

-- Dist List of probability and tuple (numUnder16, numEqual16, numOver16)
createTallySheet :: (Num a, Num t1, Num t2, Num t3, Ord a, Foldable t) => [DistElement (t a)] -> [DistElement (t1, t2, t3)]
createTallySheet dist = 
    map (\(Node prob sumList) -> 
        let 
            underSixteen = compareSixteen sumList (<)
            equalSixteen = compareSixteen sumList (==)
            overSixteen  = compareSixteen sumList (>)
        in (Node prob (underSixteen, equalSixteen, overSixteen)))
    dist


-------GAMBLING-------

max3ps :: Dist a -> Rational -> Rational -> Rational -> (Rational, Rational, Rational)
max3ps [] p1 p2 p3 = (p1, p2, p3)
max3ps ((Node r _):xs) p1 p2 p3 =
    if r > p1
        then max3ps xs r p1 p2
        else if r > p2
            then max3ps xs p1 r p2
            else if r > p3
                then max3ps xs p1 p2 r
                else max3ps xs p1 p2 p3

-- Making all possible tally
makeTuples :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
makeTuples _ (-1) _ _ = [] 
makeTuples totalTallies a b c = 
    let addThese = 
            if a == b
            then [(a, b, c), (a, c, b), (c, b, a)] 
            else if a == c 
                then [(a, b, c), (a, c, b), (b, a, c)] 
                else if b == c 
                    then [(a, b, c), (b, a, c), (c, b, a)] 
                    else [(a, b, c), (a, c, b), (b, a, c), (b, c, a), (c, a, b), (c, b, a)] 
    in addThese ++ (makeTuples totalTallies (a-1) (b+1) c) 

noRepeats :: Eq t => [t] -> [t]
noRepeats [] = []
noRepeats (x:xs)  = if x `elem` xs
    then noRepeats xs
    else x:(noRepeats xs)

-- The list with all possible tuples (LT, E, GT)
tuple30 :: [(Integer, Integer, Integer)]
tuple30 = noRepeats(makeTuples 30 30 0 0)

--Dist of all pairs of dice rolls with 30 rolls
complete_dist :: [DistElement [DistElement [Integer]]]
complete_dist =
    map (\(Node prob (_,_,dist)) -> 
        let array_dist = map (\(Node prob val) -> (Node prob [val])) dist
            joined_rolls = multiple_join array_dist 30
        in (Node prob joined_rolls))
        probDraw

--Dist of all pairs of dice rolls and their tally sheets
complete_tallies :: [DistElement [DistElement (Integer, Integer, Integer)]]
complete_tallies = 
    map (\(Node r dist) ->
            let tallies = createTallySheet dist
                merged_tallies = mergeRolls sameTally tallies 
            in (Node r merged_tallies))
    complete_dist

--Filters a distribution of tally sheets into their expected payouts, given a tally sheet
filter_tallies :: Integer -> Integer -> Integer -> [DistElement Rational]
filter_tallies lt eq gt = 
    map (\(Node r tallies) ->
        let 
            ftallies =
                filter (\(Node _ (l, e, g)) ->
                (lt==l) && (eq==e) && (gt==g))
                tallies
            p_sum = foldl (\r2 (Node r1 _)->
                r1+r2) 0 ftallies
            norm_tallies =
                map (\(Node r x) ->
                (Node (r/p_sum) x))
                ftallies
            (p1, p2, p3) = max3ps norm_tallies 0 0 0
            payout = (p1*1)+(p2*(1%2))+(p3*(1%4))-((1-p1-p2-p3)*(1%10))
        in (Node r payout))
    complete_tallies

expected_money_per_tsheet :: [Rational]
expected_money_per_tsheet = 
    map (\(a,b,c) -> 
            foldr (\(Node r payout) prev ->
            payout*r + prev) 0 (filter_tallies a b c))
    tuple30
    
expected_money :: Rational
expected_money = foldl (+) 0 expected_money_per_tsheet
-}
