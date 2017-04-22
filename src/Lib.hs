module Lib where

import Data.List (maximumBy)
import Model

-- First Person -> The person in the network who we think
--                 knows the answer (highest perceived knowledge)
-- Rel -> Includes score we think we got from that person
-- Second Person -> The person who has the highest Planck knowledge
--                  in the network
ask :: [Rel] -> (Person,Rel,Person)
ask rs = (t1,percRel,t2)   
  where percRel = highestPerceivedKn rs
        plRel = highestPlanckKn rs
        t1 = target percRel
        t2 = target plRel

gotRightAnswer :: (Person,Rel,Person) -> Bool
gotRightAnswer (Person {plKn = p1},_,Person {plKn = p2}) = p1 >= p2

-- Perceived Planck Score
-- Best Planck Score
-- Actual Planck Score
planckScores :: (Person,Rel,Person) -> (Double,Double,Double)
planckScores (Person {plKn = p1}
                ,Rel {perceivedKn = f,target = t}
                ,Person {plKn = p2}) = (f t,p2,p1)

gotRightAnswer' :: (Person, Rel, Person) -> String
gotRightAnswer' (Person {name = n1, plKn = p1, chKn = c1},_,
                 Person {name = n2, plKn = p2}) =
                    "Should get " ++ show p2 ++ " from " ++ n2 ++
                    ". Actual Planck Kn " ++ show p1 ++ " from " ++ n1 ++ 
                    " with Chauffeur Kn " ++ show c1 ++ "."

highestPerceivedKn :: [Rel] -> Rel
highestPerceivedKn = maximumBy ordByPerceivedKn

highestPlanckKn :: [Rel] -> Rel
highestPlanckKn = maximumBy ordByPlanckKn

-- Size of network
-- Planck score we got
-- Planck score we thought we got
-- Planck score we could have got
runExample :: [Rel] -> (Int,Double,Double,Double)
runExample rs = (length rs, v1 , v2, v3)
  where (v1,v2,v3) = planckScores $ ask rs

runExamples :: [Rel] -> [(Int,Double,Double,Double)]
runExamples [] = []
runExamples rss@(_:rs) = runExample rss : runExamples rs

getKnowledges :: (Person -> Double) -> [Rel] -> [Double]
getKnowledges f xs = map (f . target) xs

