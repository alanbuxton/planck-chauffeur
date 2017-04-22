module Model where

data Person = Person { name :: String
                     , plKn :: Double -- PlanckKnowledge
                     , chKn :: Double -- ChauffeurKnowledge
                     } deriving (Show, Eq, Ord)

data Rel = Rel { target :: Person
               , perceivedKn :: (Person -> Double) } 

instance Show Rel where
  show (Rel t p) = "Rel {" ++ show t ++ ", perceived Kn = " ++
                    (show . p) t ++ "}"

instance Ord Rel where
  (Rel t p) `compare` (Rel t' p') = (t,p t) `compare`  (t', p' t')

instance Eq Rel where
  (Rel t p) == (Rel t' p') = (t, p t) == (t', p' t')

ordByPerceivedKn :: Rel -> Rel -> Ordering
ordByPerceivedKn (Rel t p) (Rel t' p') = p t `compare` p' t'

ordByPlanckKn :: Rel -> Rel -> Ordering
ordByPlanckKn (Rel t _) (Rel t' _) = plKn t `compare`  plKn t'

