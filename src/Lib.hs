module Lib where

import Data.List (maximumBy)
import Model

-- Rel -> Person we think has the best knowledge
-- Second Person -> The person who has the highest Planck knowledge
--                  in the network
ask :: [Rel] -> (Rel,Person)
ask rs = (percRel,planckPers)  
  where percRel = highestPerceivedKn rs
        plRel = highestPlanckKn rs
        planckPers = target plRel

-- Perceived Planck Score
-- Best Planck Score
-- Actual Planck Score
planckScores :: (Rel,Person) -> (Double,Double,Double)
planckScores (Rel {perceivedKn = f,target = p1}
                ,Person {plKn = s2}) = (f p1,s2,plKn p1)
 
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
getKnowledges f = map (f . target) 

