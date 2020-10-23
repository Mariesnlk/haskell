{-# OPTIONS_GHC -Wall #-}
module Synelnyk06 where

import Data.List

type GraphS = (Int,[(Int,Int)])
type Graph  = [[Int]]

-- ������ 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary gr = False `notElem` [a `elem` gr!!b | a<- nodes gr, b<-gr!!a]

nodes::Graph-> [Int]
nodes gr=[0..(length gr-1)]

-- ������ 2 ------------------------------------
fromGraph :: Graph -> GraphS
fromGraph gr = (length gr - 1,edges gr)

edges::Graph -> [(Int,Int)]
edges g = [(x,y)| x<-nodes g, y <-g!!x]

-- ������ 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph grS = [map snd y | y<- [filter (\n -> fst n == x) $ snd grS | x <- [0 .. fst grS]]]

-- ������ 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b = if res==[] then [] else res!!0
      where res = getNext gr b [[a]]

getNext :: Graph -> Int -> [[Int]] -> [[Int]]
getNext _ _ [] = []
getNext gr b xs = if length res > 0 then res
                      else getNext gr b $ stepW gr xs
    where   res = [n | n<-xs, n!!(length n-1)==b]

stepW :: Graph -> [[Int]] -> [[Int]]
stepW _ [] = []
stepW gr (w:ws) = [w++[x] | x<-adj, not (elem x w)] ++ stepW gr ws
      where adj = gr!!(w!!(length w - 1))

-- ������ 5 ------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = length gr == (length $ goNodes gr 0)

goNodes :: Graph -> Int -> [Int]
goNodes gr v = sort $ snd $ until cond (oneStep gr) ([v],[])

oneStep :: Graph -> ([Int], [Int]) -> ([Int],[Int])
oneStep gr (ns,os) = let old = ns ++os
                         ns1 = concatMap (gr !!) ns
                         ns2 = filter (`notElem` old) ns1
                         new = nub ns2
 in (new,old)

cond :: ([Int],[Int]) -> Bool
cond (ns,_) = ns == []

-- ������ 6 ------------------------------------
components :: Graph -> [[Int]]
components gr= reverse $ snd $ until condComponent (findComponent gr) ([0..(length gr-1)],[])

condComponent :: ([Int],[[Int]])->Bool
condComponent (vs,_) = null vs

findComponent :: Graph -> ([Int],[[Int]]) -> ([Int],[[Int]])
findComponent gr (xs,res) = (filter (\x-> notElem x comp) xs,comp:res)
    where comp = goNodes gr (head xs)

-- ������ 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v = maximum [length (shortWay gr v x) - 1 | x<-nodes gr]

-- ������ 8 ------------------------------------
findDiameter :: Graph -> Int
findDiameter gr = maximum [eccentricity gr x | x<-nodes gr]

findRadius :: Graph -> Int
findRadius gr = minimum [eccentricity gr x | x<-nodes gr]

-- ������ 9 ------------------------------------
findCenter :: Graph -> [Int]
findCenter gr = [x | x<-nodes gr, (eccentricity gr x) == findRadius gr]

-- ������ 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]]
shortWays gr a b = getNext gr b [[a]]

gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
