{-# LANGUAGE OverloadedStrings #-}

module View where

import Prelude hiding (head,div,id)
import Text.Blaze.Html5 hiding (style) 
import Text.Blaze.Html5.Attributes hiding (form,title,label)
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M

inputForm :: AttributeValue -> AttributeValue -> AttributeValue ->
              AttributeValue -> AttributeValue -> AttributeValue -> Int ->  Html
inputForm pm ps cm cs ex pp maxP = form ! action "/run-model" $ do
  table $ do
    tr $ do
      td ! class_ "right" $ label ! for "pm" $ "Planck Mean: "
      td $ input ! type_ "text" ! name "pm" ! id "pm" ! value pm ! size "3"
      td ! class_ "right" $ label ! for "cm" $ "Chauffeur Mean: "
      td $ input ! type_ "text" ! name "cm" ! id "cm" ! value cm ! size "3"
      td ! class_ "right" $ label ! for "ex" $ "Number of people to ask: "
      td $ input ! type_ "text" ! name "ex" ! id "ex" ! value ex ! size "3"
      td $ toMarkup $ " (max " ++ show maxP ++ ")"
    tr $ do
      td ! class_ "right" $ label ! for "ps" $ "Planck Std Dev: "
      td $ input ! type_ "text" ! name "ps" ! id "ps" ! value ps ! size "3"
      td ! class_ "right" $ label ! for "cs" $ "Chauffeur Std Dev: "
      td $ input ! type_ "text" ! name "cs" ! id "cs" ! value cs ! size "3"
      td ! class_ "right" $ label ! for "pp" $ "Chance of mistaking Chauffeur for Planck Knowledge: "
      td $ input ! type_ "text" ! name "pp" ! id "pp" ! value pp ! size "2"
      td $ "% (whole numbers only)"
    tr $ do
      td ! colspan "7" ! class_ "centered" $ input ! type_ "submit" ! value "Run Model"

graphPage :: Double -> Double -> Double -> Double -> Int -> Int ->
              [Double] ->  [Double] -> [Double] ->  M.Map Int Int -> 
              M.Map Int Int -> Int -> Int ->  Html
graphPage pm ps cm cs ex pp s1 s2 s3 pls chs maxC maxP = docTypeHtml $ do
  head $ do 
    script ! src "/js/jquery-3.2.1.min.js" $ mempty
    script ! src "/js/highcharts/code/highcharts.js" $ mempty
    title "Planck vs Chauffeur Knowledge Model"
    link ! href "/css/styles.css" ! rel "stylesheet" ! type_ "text/css"
  body $ do
    h1 "Effects of Planck vs Chauffeur Knowledge"
    p $ do
      "If this is your first time here, please follow this link to "
      a ! href "https://github.com/alanbuxton/planck-chauffeur/blob/master/README.md" $ "read the background"
      "."
    p "Set parameters for the model below:"
    inputForm (toValue pm) (toValue ps) (toValue cm) (toValue cs) (toValue ex) (toValue pp) maxP
    div ! id "knowledgeChart" $ mempty
    script $ scoreChart s1 s2 s3
    explanatoryText
    h1 "Knowledge Distribution Charts"
    explanatoryText2
    div ! id "histo1" ! class_ "distribChart" $ mempty 
    div ! id "histo2" ! class_ "distribChart" $ mempty
    script $ distribChart "histo1" "Planck Knowledge Distribution" pls maxC
    script $ distribChart "histo2" "Chauffeur Knowledge Distribution" chs  maxC

-- Thought, Best, Actual
scoreChart :: [Double] -> [Double] -> [Double] -> Html
scoreChart s1 s2 s3 = 
  let minVal = minimum (s1 ++ s2 ++ s3)
      minY = show $ floor $ minVal * 0.85
  in toMarkup $ "$(function () { Highcharts.chart('knowledgeChart', {" ++
    "  chart: {" ++
    "    type: 'area'" ++
    "  }," ++
    "  title: {" ++
    "    text: 'Quality of Knowledge by Size of Network'" ++
    "  }," ++
    "  xAxis: {" ++
    "    title: {text: 'Number of People Asked'}" ++
    "  }," ++ 
    "  yAxis: {" ++
    "    min: " ++ minY ++ "," ++
    "    title: {" ++
    "      text: 'Knowledge Score'" ++
    "    }" ++
    "  }," ++
    "  legend: {" ++
    "    layout: 'vertical'," ++
    "    align: 'right'," ++
    "    verticalAlign: 'middle'" ++
    "  }," ++
    "  plotOptions: {" ++
    "    series: {" ++
    "      pointStart: 1" ++
    "    }" ++
    "  }," ++
    "  series: [{" ++
    "    name: 'Think I Know'," ++
    "    data: " ++ show s1 ++
    "  }, {" ++
    "    name: 'Best Available Knowledge'," ++ 
    "    data: " ++ show s2 ++
    "  }, {" ++
    "    name: 'I Really Know'," ++
    "    data: " ++ show s3 ++ 
    "  }]" ++
    "});});"

createHisto :: [Double] -> Int -> Int -> M.Map Int Int
createHisto vs x y = 
  let vs' = groupResults vs
  in fillBlanks vs' x y

groupResults :: [Double] -> M.Map Int Int
groupResults = foldr groupResult M.empty 

groupResult :: Double -> M.Map Int Int -> M.Map Int Int
groupResult k m = M.insert k' (r + 1) m 
  where k' = round k
        r = M.findWithDefault 0 k' m

fillBlanks :: M.Map Int Int -> Int -> Int -> M.Map Int Int
fillBlanks m x y =  foldr fillBlank m [x..y]

fillBlank :: Int -> M.Map Int Int -> M.Map Int Int 
fillBlank k m = M.insert k v m 
  where v = M.findWithDefault 0 k m 

distribChart :: String -> String -> M.Map Int Int -> Int -> Html
distribChart divid n vs maxY = 
  let minX = show $ (M.keys vs) !! 0
  in toMarkup $ "$(function () { " ++
                  " Highcharts.chart('" ++ divid ++ "', {" ++
                  "  chart: {" ++
                  "    type: 'column'" ++
                  "  }," ++
                  "  title: {" ++
                  "    text: '" ++ n ++ "' " ++
                  "  }," ++
                  "  yAxis: {" ++
                  "    max: " ++ show maxY ++ "," ++ 
                  "    title: {text: 'Count'}" ++
                  "  }," ++
                  "  plotOptions: {" ++
                  "    column: {" ++
                  "      pointStart: " ++ minX ++ "," ++
                  "      pointPadding: 0," ++
                  "      groupPadding: 0," ++
                  "    }" ++
                  "  }," ++
                  "  series: [{" ++ 
                  "    showInLegend: false," ++
                  "    name: 'Count'," ++
                  "    data: " ++ show (M.elems vs) ++ 
                  "  }]" ++
                  " });" ++
                  "});"

explanatoryText :: Html
explanatoryText = do
  p $ do
    toMarkup $ "The chart above shows how well (or badly) you do at getting the best possible " ++
      "answer to a particular question, depending on the network of people you ask. A higher " ++
      "Knowledge Score on the Y axis means a better answer. Or, at least, what you " 
    em "think"
    " is a better answer."
  p $ toMarkup $ "Suppose you need the answer to a question. It could be anything, for example " ++
    "'Why is the sky blue?' or 'When were tiles first used on houses?' Now suppose that the only " ++
    "way to get the answer is to ask people that you know to help you." 
  p $ toMarkup $ "You ask the first person you know. She gives you what she thinks is the answer. " ++
    "She might be confident or not, depending on how well she thinks she knows the subject. " ++
    "You then ask the next person you know and see if you get a better answer, until you get all " ++
    "they way through your network. The X axis shows how your knowledge of the subject changes " ++
    "as you ask more and more people."
  p $ do
    "If the only colour you can see is "
    strong "green" 
    " then, great, you got the best answer."
  p $ do
    "The "
    strong "blue"
    toMarkup $ " area shows where your network got it wrong. At some point along the line you mixed up someone's " ++ 
      "Planck Knowledge with his Chauffeur Knowledge. You gave ths person's opinion too much weight. " ++
      "While you may "
    em "think"
    toMarkup $ " that you got the best answer, in fact you may well have got a worse answer than what you already had. " ++
      "Either way you have a problem because what you "
    em "think"
    " you know is much more than what you "
    em "really"
    " know. The "
    strong "black"
    toMarkup $ " area shows the best answer that you could have got from your network, if only you had managed to " ++
      "find the person with the best Planck Knowledge." 
  p "Notice how the blue and black parts always increase. It's just the green - what you really know - that jumps around."

explanatoryText2 :: Html
explanatoryText2 = 
  p $ toMarkup $ "The charts below show the distribution of Planck and Chauffeur Knowledge that this model generated. " ++
    "Of course, you will get a better-looking normal distribution as you increase the number of people in the network."

