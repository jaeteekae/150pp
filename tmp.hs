{-

    Comp 150 PPL Programming Problems
    By: Julia Knight and Yennie Jun
    Date: September 24, 2016 

    To see our answers for the questions we answered, please run
    the following commands once everything has been run:

    Simple Distribution and Probability Questions:
        probabilityQuestionA

    Dice and Coins:
        probabilityQuestionF

    Tally-sheet Questions:
        tallyProbabilityN
        tallyProbabilityO

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

probabilityQuestionA = d6


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

multiple_join dist 1 = dist
multiple_join dist n = multiple_join (join dist dist sq) (n-1)

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

-- Rational
probabilityQuestionF = probSum 3 Heads distd6

-------TALLY SHEET-------

--twoD12s = filter (\(Node prob (a:b:[])) -> ((a==D12) && (b == D12))) probDraw
--(Node d12Prob b) = head twoD12s

--twoD12s_rollProbs = join d12 d12 add

sameRoll (Node a b) (Node c d) = b==d
sameTally (Node a (l1, e1, g1)) (Node c (l2, e2, g2)) = (l1==l2)&&(e1==e2)&&(g1==g2)
sameDice (Node a (d_a1, d_a2, dist1)) (Node c (d_b1, d_b2, dist2)) = ((d_a1==d_b1)&&(d_a2==d_b2))||((d_a1==d_b2)&&(d_a2==d_b1))

--mergeRolls :: Eq a => [a] -> [a]
mergeRolls xs f = case xs' of
             [] -> []
             x'@(x:[]) -> x'
             x'@(x1:x2:[]) -> x'
             x'@(x1:x2:xs) -> x1:(mergeRolls (x2:xs) f)
    where xs' = aux (head xs) (tail xs) []
          --aux :: Dist a => Dist a -> [Dist a] -> [Dist a] -> [Dist a]
          aux acc [] temp = (acc:temp)
          aux acc (y:ys) temp = if (f acc y)
                                then 
                                    let (Node p1 r1) = acc
                                        (Node p2 r2) = y
                                    in aux (Node (p1+p2) r1) (ys ++ temp) []
                                else aux acc ys (y:temp)

pot = [Node(toRational 9/46) (D6,d6), Node(toRational 9/46) (D8,d8), Node(toRational 14/46) (D12,d12), Node(toRational 14/46) (D20,d20)]

-- WE ARE DOING WITH REPLACEMENT 
simple_draw = join pot pot dice_tup