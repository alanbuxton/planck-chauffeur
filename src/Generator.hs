module Generator where

import Model
import Data.Random.Normal (normalsIO')
import System.Random (randomRIO, randomRs,newStdGen)
import Control.Monad (sequence)
import Control.Applicative ((<$>))
import Data.List (zip4)

-- Number of examples
-- Percentage chance that person uses Chauffeur knowledge vs Planck knowledge
createRels :: Int -> Int -> (Double,Double,Double,Double) -> IO [Rel]
createRels x y (pm,ps,cm,cs)= do
  pls <- take x <$> normalsIO' (pm,ps)
  chs <- take x <$> normalsIO' (cm,cs)
  g <- newStdGen
  let plch = take x $ randomRs (0 :: Int, 99 :: Int) g
  return $ map (createRel y) $ zip4 [1..] pls chs plch

createRel :: Int -> (Int,Double,Double,Int) -> Rel
createRel y (id,pl,ch,plch) = Rel p f
  where f = if plch < y then chKn else plKn 
        p = Person (show id) pl ch

