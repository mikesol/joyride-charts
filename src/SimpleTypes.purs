module SimpleTypes
  ( Column
  , Rail(..)
  , Smpl
  , SmplRail
  , farLeft
  , farRight
  , left
  , nearLeft
  , nearRight
  , overOne
  , overThree
  , overTwo
  , right
  , leftRail
  , rightRail
  )
  where

import Prelude

import Data.Number ((%))
import Simple.JSON as JSON

newtype Column = Column Number

derive instance Eq Column
derive instance Ord Column

farLeft ∷ Column
farLeft = Column (-1.5)

nearLeft ∷ Column
nearLeft = Column (-0.5)

nearRight ∷ Column
nearRight = Column 0.5

farRight ∷ Column
farRight = Column 1.5

over ∷ Number → Column → Column
over n (Column x) = Column (((x + 1.5 + n) % 3.0) - 1.5)

overOne ∷ Column → Column
overOne = over 1.0

overTwo ∷ Column → Column
overTwo = over 2.0

overThree ∷ Column → Column
overThree = over 3.0

left :: Column
left = Column (-1.0)

right :: Column
right = Column (1.0)

instance JSON.WriteForeign Column where
  writeImpl (Column c) = JSON.writeImpl c

instance JSON.ReadForeign Column where
  readImpl c = Column <$> JSON.readImpl c

type Smpl =
  { timing :: Number
  , column :: Column
  }


newtype Rail = Rail Number

derive instance Eq Rail
derive instance Ord Rail

leftRail ∷ Rail
leftRail = Rail (-1.0)

rightRail ∷ Rail
rightRail = Rail (1.0)


instance JSON.WriteForeign Rail where
  writeImpl (Rail c) = JSON.writeImpl c

instance JSON.ReadForeign Rail where
  readImpl c = Rail <$> JSON.readImpl c

type SmplRail =
  { timing :: Number
  , column :: Rail
  }
