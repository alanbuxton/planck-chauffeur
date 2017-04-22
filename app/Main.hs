{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Generator
import View
import Model
import Web.Scotty
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static
import System.Environment (getArgs)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  (port,maxPeople) <- args
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/run-model" $ do
      -- rescue block sets sensible default if the parameter is missing
      ex' <- param "ex" `rescue` (\_ -> return 200)
      pm <- param "pm" `rescue` (\_ -> return 100.0)
      ps <- param "ps" `rescue` (\_ -> return 15.0)
      cm <- param "cm" `rescue` (\_ -> return 100.0)
      cs <- param "cs" `rescue` (\_ -> return 15.0)
      pp' <- (param "pp" :: ActionM Float) `rescue` (\_ -> return 50)
      let pp = floor pp'
      let ex = if ex' > maxPeople then 200 else ex'
      rels <- liftIO $ createRels ex pp (pm,ps,cm,cs)
      let res = reverse $ runExamples rels
      let plKns = getKnowledges plKn rels
      let chKns = getKnowledges chKn rels
      let minK = floor $ minimum (plKns ++ chKns)
      let maxK = ceiling $ maximum (plKns ++ chKns)
      let pls = createHisto plKns minK maxK 
      let chs = createHisto chKns minK maxK
      let maxC = yAxisMax pls chs
      let (_,s1,s2,s3) = tuplesToArrs res
      html $ renderHtml $ graphPage pm ps cm cs ex pp s1 s2 s3 pls chs maxC maxPeople
    get "/run-model" $ 
      -- if previous section fails (e.g. nr examples can't be parsed to an Int,
      -- then we fall through to this section
      redirect "/"
    get "/" $ 
      redirect "/run-model?ex=200&pm=100&ps=15&cm=100&cs=15&pp=50"

tuplesToArrs :: [(Int,Double,Double,Double)] -> ([Int],[Double],[Double],[Double])
tuplesToArrs res = 
  let ids = map (\(x,_,_,_) -> x) res
      thought = map (\(_,x,_,_) -> x) res

      best = map (\(_,_,x,_) -> x) res
      got = map (\(_,_,_,x) -> x) res
  in (ids,thought,best,got)

yAxisMax :: M.Map Int Int -> M.Map Int Int -> Int
yAxisMax m1 m2 = 
  let v = maximum $ M.elems m1 ++ M.elems m2
      v' = (1.05 :: Float) * fromIntegral v
  in  ceiling v'

args :: IO (Int,Int)
args = do
  (a1:a2:_) <- getArgs
  let port = read a1 
  let a2' = read a2 
  let maxPeople = if a2' < 200 then 200  else a2'
  return (port,maxPeople)

--printRes :: [(Int,Double,Double,Double)] -> T.Text
--printRes rs = T.pack $ intercalate "\n" $ "Count,Thought,Best,Actual" : map csvFormatter rs

--csvFormatter :: (Int,Double,Double,Double) -> String
--csvFormatter (a,b,c,d) = show a ++ "," ++ show b ++ "," ++
--                          show c ++ "," ++ show d 


