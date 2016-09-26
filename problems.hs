{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

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
data Dist a = Node Rational a deriving (Show)
data Coin = Heads | Tails deriving (Show, Eq)
data Die = D4 | D6 | D8 | D10 | D12 | D20 deriving (Show, Eq)

--COIN
coins = [Node (toRational 1/2) [Heads], Node (toRational 1/2) [Tails]]

--MAKE A DIE
dx x = let ratio = (toRational 1/x) in map (Node ratio) [1..round(x)]

--DICE
d1 = dx 1
d2 = dx 2
d3 = dx 3
d4 = dx 4
d5 = dx 5
d6 = dx 6
d8 = dx 8
d10 = dx 10
d12 = dx 12
d20 = dx 20

--Probability distribution of one d6 roll
probabilityQuestionA_d6 = d6
probabilityQuestionA_d12 = d12


-------FUNCTIONS FOR CREATING JOINT DISTRIBUTIONS-------

--Join two distribution lists using mapping function f
join d1 d2 f = concat (map (f d2) d1)

--Multiplies probabilities & cons
sq d2s (Node c d) = map (\(Node a b) -> Node (c*a) (b++d)) d2s

--Multiplies probabilities & tuple
sqdiff d2s (Node c d) = map (\(Node a b) -> Node (c*a) (b,d)) d2s

--D2 distribution depends on D1 value
dep d2s (Node c 1) = map (\(Node a b) -> Node (c*a) b) d2s
dep d2s (Node c n) = join d2s (dep d2s (Node c (n-1))) sq

--Adding two values
add d2s (Node c d) = map (\(Node a b) -> Node (c*a) (d+b)) d2s

--Join a distribution with itself n times
multiple_join dist 1 = dist
multiple_join dist n = multiple_join (join dist dist sq) (n-1)

--A join specifically for adding rolling two dice together
dice_tup d2s (Node c (die1,dist1)) =
    map (\(Node a (die2,dist2)) -> Node (c*a) (die1,die2,(join dist1 dist2 add))) d2s


------EXAMPLES OF JOINT DISTRIBUTIONS------

--Joint distribution of two coin flips
cxc = join coins coins sq

--Joint distribution of a d6 roll and coin flips that depend on the outcome
distd6 = join d6 coins dep


-------FILTERING A DISTRIBUTION-------

-- Determine how many heads/tails are flipped in the list  
-- Coin -> Coin list -> int 
determine ht coinlist = foldl (\acc x -> if (x==ht) then acc + 1 else acc) 0 coinlist  

-- Int -> Coin -> Dist list -> Dist list 
makeFinalList numFlips flipType dlist = filter (\(Node prob flipList) -> (determine flipType flipList == numFlips)) dlist

-- Adds all of the probabilities of flipping numFlips Heads/Tails
-- Int -> Coin -> Dist List -> Rational
probSum numFlips flipType distType = toRational (sum (map (\(Node prob flipList) -> prob) (makeFinalList numFlips flipType distType)))

--The probability of seeing 3 heads when a d6 roll determines the number of coin tosses
-- Rational
probabilityQuestionF = probSum 3 Heads distd6

-------TALLY SHEET-------

--Pot of dice distribution
pot = [Node(toRational 9/46) (D6,d6), Node(toRational 9/46) (D8,d8), Node(toRational 14/46) (D12,d12), Node(toRational 14/46) (D20,d20)]

--WE ARE DOING WITH REPLACEMENT 
--Probability of drawing any two dice
--probDraw :: [Dist (Die, Die, [Dist Int])]
probDraw = let draw2 = join pot pot dice_tup
            in map (\(Node prob (d1, d2, dist)) -> (Node prob (d1, d2, (mergeRolls sameRoll dist)))) draw2

--Probability of drawing two d12s, filtered to decrease problem size
twoD12s = filter (\(Node prob (a,b,_)) -> ((a==D12) && (b == D12))) probDraw
(Node d12Prob b) = head twoD12s

twoD12s_rollProbs = join d12 d12 add

--Functions to use with mergeRolls
sameRoll (Node a b) (Node c d) = b==d
sameTally (Node a (l1, e1, g1)) (Node c (l2, e2, g2)) = (l1==l2)&&(e1==e2)&&(g1==g2)
sameDice (Node a (d_a1, d_a2, dist1)) (Node c (d_b1, d_b2, dist2)) = ((d_a1==d_b1)&&(d_a2==d_b2))||((d_a1==d_b2)&&(d_a2==d_b1))

--For merging distribution Nodes with the same value
mergeRolls f xs = case xs' of
             [] -> []
             x'@(x:[]) -> x'
             x'@(x1:x2:[]) -> x'
             x'@(x1:x2:xs) -> x1:(mergeRolls f (x2:xs))
    where xs' = aux (head xs) (tail xs) []
          aux acc [] temp = (acc:temp)
          aux acc (y:ys) temp = if (f acc y)
                                then 
                                    let (Node p1 r1) = acc
                                        (Node p2 r2) = y
                                    in aux (Node (p1+p2) r1) (ys ++ temp) []
                                else aux acc ys (y:temp)

compactD12s = mergeRolls sameRoll twoD12s_rollProbs 

twoD12s_rollProbsl = map (\(Node prob a) -> (Node prob [a])) compactD12s

--Distribution of two d12's rolled 30 times
d12s_30rolls = multiple_join twoD12s_rollProbsl 30

testlist = [(Node (toRational 1/2) [2 .. 24]), (Node (toRational 1/2) [16..18]), (Node (toRational 1/2) [19..21]), (Node (toRational 1/2) [19..22])]

-- Int List -> Operator -> Int 
compareSixteen sumList op = foldl(\acc x -> if (op x 16) then acc + 1 else acc) 0 sumList

-- Int -> Dist List -> Operator -> Int 
determineHowMany numOverUnder dlist op = filter (\(Node prob sumList) -> compareSixteen sumList op == numOverUnder) dlist

-- Adds: probability of the two D12s added PLUS probabilty of drawing 2 D12s
-- Int -> Dist List -> Operator -> Rational
tallyProb numOverUnder dlist op = toRational(sum (map(\(Node prob sumlist)->prob)(determineHowMany numOverUnder dlist op))) + d12Prob

-- Rational
tallyProbabilityN = tallyProb 3 d12s_30rolls (>)
tallyProbabilityO = tallyProb 16 d12s_30rolls (<)
--NOTE: theoretically, tallyProbabilityN is the correct probability. However, we could not get the calculation to terminate

-- Dist List of probability and tuple (numUnder16, numEqual16, numOver16)
createTallySheet dist = 
    map (\(Node prob sumList) -> 
        let 
            underSixteen = compareSixteen sumList (<)
            equalSixteen = compareSixteen sumList (==)
            overSixteen  = compareSixteen sumList (>)
        in (Node prob (underSixteen, equalSixteen, overSixteen)))
    dist


-------GAMBLING-------

--Dist of all pairs of dice rolls with 30 rolls
complete_dist =
    map (\(Node prob (d1,d2,dist)) -> 
        let joined_rolls = multiple_join dist 30
        in (Node prob joined_rolls))
        probDraw

--Dist of all pairs of dice rolls and their tally sheets
complete_tallies = 
    map (\(Node r dist) ->
            let tallies = createTallySheet dist
                merged_tallies = mergeRolls sameTally tallies 
            in (Node r merged_tallies))
    complete_dist

max3ps [] p1 p2 p3 = (p1, p2, p3)
max3ps ((Node r _):xs) p1 p2 p3 =
    if r > p1
        then max3ps xs r p1 p2
        else if r > p2
            then max3ps xs p1 r p2
            else if r > p3
                then max3ps xs p1 p2 r
                else max3ps xs p1 p2 p3

--Filters a distribution of tally sheets into their expected payouts, given a tally sheet
filter_tallies lt eq gt = 
    map (\(Node r tallies) ->
        let 
            ftallies =
                filter (\(Node r (l, e, g)) ->
                (lt==l) && (eq==e) && (gt==g))
                tallies
            p_sum = foldl (\r2 (Node r1 _)->
                r1+r2) 0 ftallies
            norm_tallies =
                map (\(Node r x) ->
                (Node (r/p_sum) x))
                ftallies
            (p1, p2, p3) = max3ps norm_tallies 0 0 0
            payout = (p1*1)+(p2*(toRational 1/2))+(p3*(toRational 1/4))-((1-p1-p2-p3)*(toRational 1/10))
        in (Node r payout))
    complete_tallies

-- Making all possible tally
makeTuples totalTallies (-1) b c = [] 
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

noRepeats [] = []
noRepeats (x:xs)  = if x `elem` xs
    then noRepeats xs
    else x:(noRepeats xs)

-- The list with all possible tuples (LT, E, GT)
tuple30 = noRepeats(makeTuples 30 30 0 0)

expected_money = 
    map (\(a,b,c) -> 
            foldr (\(Node r payout) prev ->
            payout*r + prev) 0 (filter_tallies a b c))
    tuple30
    
