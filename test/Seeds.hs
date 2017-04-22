module Seeds where

import Model
import Lib

alice, bob, charlie :: Person
alice = Person "Alice" 60 40 
bob = Person "Bob" 50 65 
charlie = Person "Charlie" 40 80 

rs1, rs2, rs3, rs4 :: [Rel]
rs1 = [Rel alice chKn]
rs2 = [Rel alice chKn, Rel bob plKn]
rs3 = [Rel alice plKn, Rel bob chKn]
rs4 = [Rel alice chKn, Rel bob plKn, Rel charlie chKn]
      




