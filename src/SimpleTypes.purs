module SimpleTypes
  ( Column
  , Smpl
  , farLeft
  , farRight
  , left
  , nearLeft
  , nearRight
  , overOne
  , overThree
  , overTwo
  , right
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