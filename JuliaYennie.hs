module PPOne
where

import Data.Ratio

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
data DistElement a = Node Rational a deriving (Show)
type Dist a = [DistElement a]
data Coin = Heads | Tails deriving (Show, Eq)
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show, Eq)

--COIN
coins :: Dist [Coin]
coins = [Node (1%2) [Heads], Node (1%2) [Tails]]

--MAKE A DIE
dx :: Integer -> Dist Integer
dx x = let ratio = 1 % x in map (Node ratio) [1..x]

--DICE
d1 :: Dist Integer
d1 = dx 1
d2 :: Dist Integer
d2 = dx 2
d3 :: Dist Integer
d3 = dx 3
d4 :: Dist Integer
d4 = dx 4
d5 :: Dist Integer
d5 = dx 5
d6 :: Dist Integer
d6 = dx 6
d8 :: Dist Integer
d8 = dx 8
d10 :: Dist Integer
d10 = dx 10
d12 :: Dist Integer
d12 = dx 12
d20 :: Dist Integer
d20 = dx 20

--Probability distribution of one d6 roll
probabilityQuestionA_d6 :: Dist Integer
probabilityQuestionA_d6 = d6
probabilityQuestionA_d12 :: Dist Integer
probabilityQuestionA_d12 = d12


-------FUNCTIONS FOR CREATING JOINT DISTRIBUTIONS-------

--Join two distribution lists using mapping function f
join :: Dist a -> Dist b -> (Dist b -> DistElement a -> Dist c) -> Dist c
join d1 d2 f = concat (map (f d2) d1)

--Multiplies probabilities & cons
sq :: [DistElement [a]] -> DistElement [a] -> [DistElement [a]]
sq d2s (Node c d) = map (\(Node a b) -> Node (c*a) (b++d)) d2s

--Multiplies probabilities & tuple
sqdiff :: [DistElement t] -> DistElement t1 -> [DistElement (t, t1)]
sqdiff d2s (Node c d) = map (\(Node a b) -> Node (c*a) (b,d)) d2s

--D2 distribution depends on D1 value
dep :: (Eq a, Num a) => [DistElement [a1]] -> DistElement a -> [DistElement [a1]]
dep d2s (Node c 1) = map (\(Node a b) -> Node (c*a) b) d2s
dep d2s (Node c n) = join d2s (dep d2s (Node c (n-1))) sq

--Adding two values
add :: Num a => [DistElement a] -> DistElement a -> [DistElement a]
add d2s (Node c d) = map (\(Node a b) -> Node (c*a) (d+b)) d2s

--Join a distribution with itself n times
multiple_join :: [DistElement [a1]] -> Integer -> [DistElement [a1]]
multiple_join dist 1 = dist
multiple_join dist n = multiple_join (join dist dist sq) (n-1)

--A join specifically for adding rolling two dice together
dice_tup :: Num a => [DistElement (t1, [DistElement a])] -> DistElement (t, [DistElement a]) -> [DistElement (t, t1, [DistElement a])]
dice_tup d2s (Node c (die1,dist1)) =
    map (\(Node a (die2,dist2)) -> Node (c*a) (die1,die2,(join dist1 dist2 add))) d2s


------EXAMPLES OF JOINT DISTRIBUTIONS------

--Joint distribution of two coin flips
cxc :: [DistElement [Coin]]
cxc = join coins coins sq

--Joint distribution of a d6 roll and coin flips that depend on the outcome
distd6 :: [DistElement [Coin]]
distd6 = join d6 coins dep


-------FILTERING A DISTRIBUTION-------

-- Determine how many heads/tails are flipped in the list  
-- Coin -> Coin list -> int 
determine :: (Eq a, Num b, Foldable t) => a -> t a -> b
determine ht coinlist = foldl (\acc x -> if (x==ht) then acc + 1 else acc) 0 coinlist  

-- Int -> Coin -> Dist list -> Dist list 
makeFinalList :: (Eq a, Eq a1, Num a, Foldable t) => a -> a1 -> [DistElement (t a1)] -> [DistElement (t a1)]
makeFinalList numFlips flipType dlist = filter (\(Node _ flipList) -> (determine flipType flipList == numFlips)) dlist

-- Adds all of the probabilities of flipping numFlips Heads/Tails
-- Int -> Coin -> Dist List -> Rational
probSum :: (Eq a1, Foldable t) => Integer -> a1 -> [DistElement (t a1)] -> Rational
probSum numFlips flipType distType = toRational (sum (map (\(Node prob _) -> prob) (makeFinalList numFlips flipType distType)))

--The probability of seeing 3 heads when a d6 roll determines the number of coin tosses
-- Rational

probabilityQuestionF :: Rational
probabilityQuestionF = probSum 3 Heads distd6

-------TALLY SHEET-------

--Pot of dice distribution
pot :: Dist (Die, Dist Integer)
pot = [Node (9%46) (D6,d6), Node (9%46) (D8,d8), Node (14%46) (D12,d12), Node (14%46) (D20,d20)]

--WE ARE DOING WITH REPLACEMENT 
--Probability of drawing any two dice
--probDraw :: [Dist (Die, Die, [Dist Int])]
probDraw :: Dist (Die, Die, [DistElement Integer])
probDraw = let draw2 = join pot pot dice_tup
            in map (\(Node prob (d1, d2, dist)) -> (Node prob (d1, d2, (mergeRolls sameRoll dist)))) draw2

--Probability of drawing two d12s, filtered to decrease problem size
twoD12s :: Dist (Die, Die, [DistElement Integer])
twoD12s = filter (\(Node _ (a,b,_)) -> ((a==D12) && (b == D12))) probDraw
d12Prob :: Rational
b :: (Die, Die, [DistElement Integer])
(Node d12Prob b) = head twoD12s

twoD12s_rollProbs :: Dist Integer
twoD12s_rollProbs = join d12 d12 add

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
