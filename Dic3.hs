module Dic3 where

import Data.Char (ord, toLower)
import Tree

data AbsD a = MkAbsD {comp :: (a->a->Bool), magn :: (a->Int)}

-- | Returns True the two elements have equal magnitude
(=||=) :: AbsD a -> (a -> a -> Bool)
(=||=) = comp

-- | Get the magnitude of this element as an Int
magnitude :: AbsD a  -> (a -> Int)
magnitude = magn

-- | Dictionary for Int
dAbsInt :: AbsD Int
dAbsInt = MkAbsD {comp = \x -> \y -> abs x == abs y, magn = abs }

-- | Dictionary for Int
dAbsChar :: AbsD Char
dAbsChar = MkAbsD {comp = \x -> \y -> toLower x == toLower y, magn = ord . toLower }

-- | Dictionary for Trees

dAbsTree :: AbsD a -> AbsD (Tree a)
dAbsTree = \d -> MkAbsD {comp = \x -> \y -> let recComp = \x -> \y -> case x of
                                                    Leaf a                     -> case y of
                                                                                    Leaf b -> comp d a b
                                                                                    _      -> False
                                                    Node val1 left1 right1     -> case y of
                                                                                    Node val2 left2 right2 ->
                                                                                        recComp left1 left2 &&
                                                                                        recComp right1 right2 &&
                                                                                        comp d val1 val2
                                                                                    _                      -> False
                                            in recComp x y
                , magn = \x -> let recMag = \x -> case x of
                                                    Leaf a              -> magnitude d a
                                                    Node val left right -> (magnitude d val) + recMag left + recMag right
                               in recMag x
                }

-- | Dictionary for Tree Int
dAbsTreeInt :: AbsD (Tree Int)
dAbsTreeInt = dAbsTree dAbsInt

-- | Dictionary for Tree Char
dAbsTreeChar :: AbsD (Tree Char)
dAbsTreeChar = dAbsTree dAbsChar

cmp :: AbsD a -> a -> a -> String
cmp dic x y = if((=||=) dic x y) then "Abs-Equal" else "Abs-Not-Equal"

cmpMagnitude :: AbsD a -> AbsD b -> a -> b -> String
cmpMagnitude dicx dicy x y = if(magnitude dicx x == magnitude dicy y) then "Abs-Equal" else "Abs-Not-Equal"
