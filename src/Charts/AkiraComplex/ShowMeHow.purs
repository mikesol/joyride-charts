module Charts.AkiraComplex.ShowMeHow where

-- SHOW ME HOW
-- | 1-8    | 9-16 | 17-24   | 25-32    | 33-40    | 41-48     | 49-56      | 56 - 64  | 65     |
-- | ------ | ---- | ------- | -------- | -------- | --------- | ---------- | -------- | ------ |
-- | groove | rise | groove2 | rigroove | ethereal | ethgroove | mutegroove | lastrise | fizzle |

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Writer (runWriter, tell)
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Simple.JSON as JSON

tempo :: Number
tempo = 120.0

newtype Column = Column Int

derive instance Eq Column
derive newtype instance JSON.ReadForeign Column
derive newtype instance JSON.WriteForeign Column
newtype Note = Note { timing :: Number, column :: Column }

derive instance Newtype Note _
derive instance Eq Note
derive newtype instance JSON.ReadForeign Note
derive newtype instance JSON.WriteForeign Note

data NotesOrErrors = Notes (Array Note) | Errors (Array String)

instance JSON.WriteForeign NotesOrErrors where
  writeImpl (Notes x) = JSON.writeImpl x
  writeImpl (Errors x) = JSON.writeImpl x

instance JSON.ReadForeign NotesOrErrors where
  readImpl x = Notes <$> JSON.readImpl x <|> Errors <$> JSON.readImpl x

col1 :: Column
col1 = Column 0

col2 :: Column
col2 = Column 1

col3 :: Column
col3 = Column 2

col4 :: Column
col4 = Column 3

c' :: Column -> Int -> Number -> Note
c' c i n = Note
  { timing: (toNumber (i - 1) * 4.0 + (n - 1.0)) * 60.0 / tempo, column: c }

c1 :: Int -> Number -> Array Note
c1 i n = [ c' col1 i n ]

c2 :: Int -> Number -> Array Note
c2 i n = [ c' col2 i n ]

c3 :: Int -> Number -> Array Note
c3 i n = [ c' col3 i n ]

c4 :: Int -> Number -> Array Note
c4 i n = [ c' col4 i n ]

-- | 1-8    
groove :: Array Note
groove = join
  [ c2 1 2.0
  , c4 1 4.0
  , c2 2 2.0
  , c4 2 4.0
  , c4 2 4.5
  , c2 3 2.0
  , c4 3 4.0
  , c2 4 2.0
  , c3 4 3.5
  , c4 4 4.0
  , c4 4 4.5
  , c4 4 4.75
  --
  , c2 5 1.0
  , c1 5 2.0
  , c3 5 2.0
  , c2 5 3.0
  , c4 5 4.0
  , c2 6 1.0
  , c1 6 2.0
  , c3 6 2.0
  , c2 6 3.0
  , c4 6 4.0
  , c4 6 4.5
  , c2 7 1.0
  , c1 7 2.0
  , c3 7 2.0
  , c2 7 3.0
  , c4 7 4.0
  , c2 8 1.0
  , c1 8 2.0
  , c3 8 2.0
  , c2 8 3.0
  , c3 8 3.5
  , c4 8 4.0
  , c4 8 4.5
  , c4 8 4.75
  ]

-- | 9-16
rise :: Array Note
rise = join
  [ c1 9 1.0
  , c3 9 1.0
  , c1 9 1.5
  , c3 9 1.5
  , c2 9 2.0
  , c2 9 2.5
  , c1 9 3.0
  , c3 9 3.0
  , c1 9 3.5
  , c3 9 3.5
  , c2 9 4.0
  , c2 9 4.5
  , c2 9 4.75
  -- skip
  , c1 10 1.5
  , c3 10 1.5
  , c2 10 2.0
  , c3 10 2.0
  , c2 10 2.5
  , c3 10 2.5
  , c1 10 3.0
  , c3 10 3.0
  , c1 10 3.5
  , c3 10 3.5
  , c2 10 4.0
  , c3 10 4.0
  , c2 10 4.5
  , c2 10 4.75
  -- skip
  , c1 11 1.5
  , c3 11 1.5
  , c2 11 2.0
  , c3 11 2.0
  , c2 11 2.5
  , c3 11 2.5
  , c1 11 3.0
  , c3 11 3.0
  , c1 11 3.5
  , c3 11 3.5
  , c2 11 4.0
  , c4 11 4.25
  , c2 11 4.5
  , c4 11 4.75
  -- skip
  , c1 12 1.5
  , c3 12 1.5
  , c2 12 2.0
  , c3 12 2.0
  , c1 12 2.5
  , c1 12 3.0
  , c3 12 3.0
  , c3 12 3.5
  , c2 12 4.0
  , c4 12 4.25
  , c2 12 4.5
  , c4 12 4.75
  --
  , c1 13 1.0
  , c2 13 1.0
  , c1 13 1.5
  , c2 13 1.5
  , c2 13 2.0
  , c3 13 2.0
  , c3 13 2.5
  , c4 13 2.5
  , c1 13 3.0
  , c3 13 3.0
  , c2 13 3.5
  , c4 13 3.5
  , c1 13 4.0
  , c1 13 4.25
  , c1 13 4.5
  , c1 13 4.75
  -- skip
  , c2 14 1.5
  , c3 14 1.5
  , c3 14 2.0
  , c4 14 2.0
  , c1 14 2.5
  , c2 14 2.5
  , c2 14 3.0
  , c4 14 3.0
  , c1 14 3.5
  , c3 14 3.5
  , c2 14 4.0
  , c3 14 4.25
  , c4 14 4.5
  , c4 14 4.75
  -- skip
  , c2 15 1.5
  , c3 15 1.5
  , c3 15 2.0
  , c4 15 2.0
  , c1 15 2.5
  , c2 15 2.5
  , c3 15 2.5
  , c1 15 3.0
  , c3 15 3.0
  , c3 15 3.5
  , c2 15 4.0
  , c3 15 4.25
  , c4 15 4.5
  , c4 15 4.75
  --
  , c1 16 1.0
  , c2 16 1.0
  , c2 16 1.5
  , c3 16 1.5
  , c3 16 2.0
  , c4 16 2.0
  , c1 16 2.5
  , c4 16 2.5
  , c1 16 3.0
  , c3 16 3.0
  , c2 16 3.5
  , c4 16 3.5
  , c1 16 4.0
  , c2 16 4.0
  , c3 16 4.25
  , c4 16 4.25
  , c1 16 4.5
  , c2 16 4.5
  , c3 16 4.75
  , c4 16 4.75
  ]

-- | 17-24  
groove2 :: Array Note
groove2 = join
  $ cell 17
      <> cell2 18
      <> cell3 19
      <> cell4 20
      <> endg
  where
  cell i =
    [ c1 i 1.0
    , c3 i 1.0
    , c1 i 1.5
    , c3 i 1.5
    , c2 i 2.0
    , c4 i 2.0
    , c1 i 2.5
    , c3 i 2.5
    --
    , c1 i 3.0
    , c3 i 3.0
    , c1 i 3.5
    , c3 i 3.5
    , c2 i 4.0
    , c4 i 4.0
    , c3 i 4.5
    , c4 i 4.5
    , c3 i 4.75
    , c4 i 4.75
    ]
  cell2 i =
    [ c1 i 1.0
    , c3 i 1.0
    , c1 i 1.5
    , c3 i 1.5
    , c2 i 2.0
    , c4 i 2.0
    , c1 i 2.5
    , c3 i 2.5
    --
    , c1 i 3.0
    , c3 i 3.0
    , c2 i 3.5
    , c4 i 3.5
    , c3 i 4.25
    , c4 i 4.25
    , c3 i 4.5
    , c4 i 4.5
    , c3 i 4.75
    , c4 i 4.75
    ]
  cell3 i =
    [ c1 i 1.0
    , c3 i 1.0
    , c1 i 1.5
    , c3 i 1.5
    , c2 i 2.0
    , c4 i 2.0
    , c1 i 2.5
    , c3 i 2.5
    --
    , c1 i 3.0
    , c3 i 3.0
    , c1 i 3.5
    , c3 i 3.5
    , c2 i 4.0
    , c4 i 4.0
    , c3 i 4.5
    , c4 i 4.5
    ]
  cell4 i =
    [ c1 i 1.0
    , c2 i 1.0
    , c1 i 1.25
    , c2 i 1.25
    , c3 i 1.75
    , c4 i 1.75
    , c3 i 2.0
    , c4 i 2.0
    --
    , c1 i 2.5
    , c3 i 2.5
    , c1 i 2.75
    , c3 i 2.75
    , c2 i 3.25
    , c4 i 3.25
    , c3 i 3.5
    , c4 i 3.5
    --
    , c1 i 4.0
    , c2 i 4.0
    , c3 i 4.25
    , c4 i 4.25
    , c3 i 4.5
    , c4 i 4.5
    ]
  endg =
    [ c1 21 1.0
    , c2 21 1.25
    , c3 21 1.5
    , c4 21 1.75
    , c1 21 2.0
    , c2 21 2.25
    , c3 21 2.5
    , c4 21 2.75
    , c1 21 3.0
    , c2 21 3.25
    , c3 21 3.5
    , c4 21 3.75
    , c1 21 4.0
    , c4 21 4.25
    , c4 21 4.5
    , c4 21 4.75
    --
    , c1 22 1.0
    , c2 22 1.25
    , c3 22 1.5
    , c4 22 1.75
    , c1 22 2.0
    , c2 22 2.25
    , c3 22 2.5
    , c4 22 2.75
    , c1 22 3.0
    , c2 22 3.25
    , c3 22 3.5
    , c4 22 3.75
    , c1 22 4.0
    , c3 22 4.25
    , c3 22 4.5
    , c3 22 4.75
    --
    , c1 23 1.0
    , c2 23 1.25
    , c3 23 1.5
    , c4 23 1.75
    , c1 23 2.0
    , c2 23 2.25
    , c3 23 2.5
    , c4 23 2.75
    , c1 23 3.0
    , c2 23 3.25
    , c3 23 3.5
    , c4 23 3.75
    , c1 23 4.0
    , c3 23 4.25
    , c4 23 4.25
    , c3 23 4.5
    , c4 23 4.5
    , c3 23 4.75
    , c4 23 4.75
    --
    , c1 24 1.0
    , c2 24 1.0
    , c3 24 1.25
    , c4 24 1.25
    , c1 24 1.5
    , c2 24 1.5
    , c3 24 1.75
    , c4 24 1.75
    , c1 24 2.0
    , c2 24 2.0
    , c3 24 2.25
    , c4 24 2.25
    , c1 24 2.5
    , c2 24 2.5
    , c3 24 2.75
    , c4 24 2.75
    , c1 24 3.0
    , c2 24 3.0
    , c3 24 3.25
    , c4 24 3.25
    , c1 24 3.5
    , c2 24 3.5
    , c3 24 3.75
    , c4 24 3.75
    , c1 24 4.0
    , c2 24 4.25
    , c3 24 4.5
    , c4 24 4.75
    ]

-- | 25-32 
rigroove :: Array Note
rigroove = join
  [ c1 25 1.0
  , c1 25 1.5
  , c1 25 2.0
  , c2 25 2.0
  , c1 25 2.5
  , c2 25 2.5
  , c1 25 3.0
  , c2 25 3.0
  , c3 25 3.0
  , c1 25 3.5
  , c2 25 3.5
  , c3 25 3.5
  , c1 25 4.0
  , c2 25 4.0
  , c3 25 4.0
  , c4 25 4.0
  , c1 25 4.5
  , c2 25 4.5
  , c3 25 4.5
  , c4 25 4.5
  -- repeat
  , c1 26 1.0
  , c1 26 1.5
  , c1 26 2.0
  , c2 26 2.0
  , c1 26 2.5
  , c2 26 2.5
  , c1 26 3.0
  , c2 26 3.0
  , c3 26 3.0
  , c1 26 3.5
  , c2 26 3.5
  , c3 26 3.5
  , c1 26 4.0
  , c2 26 4.0
  , c3 26 4.0
  , c4 26 4.0
  , c1 26 4.5
  , c2 26 4.5
  , c3 26 4.5
  , c4 26 4.5
  -- variation
  , c1 27 1.0
  , c2 27 1.0
  , c1 27 1.5
  , c2 27 1.5
  , c1 27 2.0
  , c2 27 2.0
  , c3 27 2.0
  , c1 27 2.5
  , c2 27 2.5
  , c3 27 2.5
  , c1 27 3.0
  , c2 27 3.0
  , c3 27 3.0
  , c4 27 3.0
  , c1 27 3.5
  , c2 27 3.5
  , c3 27 3.5
  , c4 27 3.5
  , c1 27 4.0
  , c1 27 4.5
  , c2 27 4.5
  , c3 27 4.5
  , c4 27 4.5
  --
  , c4 28 1.0
  , c1 28 1.5
  , c2 28 1.5
  , c3 28 1.5
  , c4 28 1.5
  , c2 28 2.0
  , c1 28 2.5
  , c2 28 2.5
  , c3 28 2.5
  , c4 28 2.5
  , c4 28 3.0
  , c1 28 3.5
  , c2 28 3.5
  , c3 28 3.5
  , c4 28 3.5
  , c2 28 4.0
  , c1 28 4.5
  , c2 28 4.5
  , c3 28 4.75
  , c4 28 4.75
  --
  , c2 29 1.0
  , c3 29 1.25
  , c2 29 1.5
  , c3 29 1.75
  , c1 29 2.0
  , c2 29 2.0
  , c3 29 2.25
  , c4 29 2.25
  , c1 29 2.5
  , c2 29 2.5
  , c3 29 2.75
  , c4 29 2.75
  , c2 29 3.0
  , c3 29 3.25
  , c2 29 3.5
  , c3 29 3.75
  , c1 29 4.0
  , c2 29 4.0
  , c3 29 4.25
  , c4 29 4.25
  , c1 29 4.5
  , c2 29 4.5
  , c3 29 4.75
  , c4 29 4.75
  --
  , c2 30 1.0
  , c3 30 1.25
  , c2 30 1.5
  , c3 30 1.75
  , c1 30 2.0
  , c2 30 2.0
  , c3 30 2.25
  , c4 30 2.25
  , c1 30 2.5
  , c2 30 2.5
  , c3 30 2.75
  , c4 30 2.75
  , c2 30 3.0
  , c3 30 3.25
  , c2 30 3.5
  , c3 30 3.75
  , c1 30 4.0
  , c2 30 4.0
  , c3 30 4.25
  , c4 30 4.25
  , c3 30 4.5
  , c4 30 4.5
  , c3 30 4.75
  , c4 30 4.75
  --
  , c2 31 1.0
  , c3 31 1.25
  , c4 31 1.25
  , c1 31 1.5
  , c3 31 1.75
  , c4 31 1.75
  , c2 31 2.0
  , c3 31 2.25
  , c4 31 2.25
  , c1 31 2.5
  , c3 31 2.75
  , c4 31 2.75
  , c2 31 3.0
  , c3 31 3.25
  , c4 31 3.25
  , c1 31 3.5
  , c3 31 3.75
  , c4 31 3.75
  , c2 31 4.0
  , c3 31 4.25
  , c4 31 4.25
  , c1 31 4.5
  , c3 31 4.75
  , c4 31 4.75
  --
  , c1 32 1.0
  , c1 32 1.25
  , c1 32 1.5
  , c1 32 1.75
  , c1 32 2.0
  , c1 32 2.25
  , c1 32 2.5
  , c1 32 2.75
  , c1 32 3.0
  , c4 32 3.0
  , c2 32 3.25
  , c3 32 3.25
  , c1 32 3.5
  , c4 32 3.5
  , c2 32 3.75
  , c3 32 3.75
  , c1 32 4.0
  , c4 32 4.0
  , c2 32 4.25
  , c3 32 4.25
  , c1 32 4.5
  , c4 32 4.5
  , c2 32 4.75
  , c3 32 4.75
  ]

-- | 33-40    
ethereal :: Array Note
ethereal = join
  [ c1 33 1.0
  , c4 33 1.0
  , c2 34 1.0
  , c3 35 1.0
  , c4 36 1.0
  , c2 36 3.0
  , c1 37 1.0
  , c4 37 4.0
  , c2 38 1.0
  , c4 38 4.0
  , c3 39 1.0
  , c4 39 4.0
  , c1 40 1.0
  , c2 40 3.0
  , c3 40 3.0
  , c4 40 3.0
  ]

-- | 41-48     
ethgroove :: Array Note
ethgroove = join
  [ c1 41 1.0
  , c4 41 2.25
  , c3 41 2.5
  , c2 41 2.75
  , c4 41 4.0

  , c1 42 1.0
  , c3 42 2.0
  , c4 42 2.25
  , c3 42 2.5
  , c2 42 2.75
  , c4 42 4.0

  , c1 43 1.0
  , c1 43 1.5
  , c3 43 2.0
  , c4 43 2.25
  , c3 43 2.5
  , c2 43 2.75
  , c4 43 4.0

  , c1 44 1.0
  , c1 44 1.5
  , c3 44 2.0
  , c4 44 2.25
  , c3 44 2.5
  , c2 44 2.75
  , c3 44 4.0
  , c4 44 4.0
  , c3 44 4.5
  , c4 44 4.5
  --
  , c1 45 1.0 --
  , c3 45 2.0 --
  , c4 45 2.25
  , c3 45 2.5
  , c2 45 2.75
  , c2 45 3.0 --
  , c4 45 3.25
  , c3 45 3.5
  , c2 45 3.75
  , c4 45 4.0 --
  , c4 45 4.25
  , c3 45 4.5
  , c2 45 4.75
  , c1 46 1.0 --
  , c2 46 1.25
  , c2 46 1.5
  , c2 46 1.75
  , c3 46 2.0 --
  , c4 46 2.25
  , c3 46 2.5
  , c2 46 2.75
  , c2 46 3.0 --
  , c4 46 3.25
  , c3 46 3.5
  , c2 46 3.75
  , c4 46 4.0 --
  , c4 46 4.25
  , c3 46 4.5
  , c2 46 4.75
  --
  , c1 47 1.0 --
  , c2 47 1.25
  , c2 47 1.5
  , c2 47 1.75
  , c3 47 2.0 --
  , c4 47 2.25
  , c3 47 2.5
  , c2 47 2.75
  , c2 47 3.0 --
  , c4 47 3.25
  , c3 47 3.5
  , c2 47 3.75
  , c4 47 4.0 --
  , c4 47 4.25
  , c3 47 4.5
  , c2 47 4.75
  --
  , c1 48 1.0 --
  , c2 48 1.25
  , c2 48 1.5
  , c2 48 1.75
  , c3 48 2.0 --
  , c4 48 2.25
  , c3 48 2.5
  , c2 48 2.75
  -- , c2 48 3.0 --
  , c4 48 3.25
  , c3 48 3.5
  -- , c2 48 3.75
  , c1 48 4.0 ------
  , c2 48 4.0
  , c3 48 4.0
  , c4 48 4.0
  ]

-- | 49-56      
mutegroove :: Array Note
mutegroove = join
  [ c1 49 1.0
  , c1 49 2.0
  -- s
  , c4 49 2.25
  , c3 49 2.5
  , c2 49 2.75
  -- e
  , c1 49 3.0
  , c1 49 4.0
  , c1 50 1.0
  , c1 50 2.0
  -- s
  , c4 50 2.25
  , c3 50 2.5
  , c2 50 2.75
  -- e
  , c1 50 3.0
  , c1 50 4.0
  , c4 50 4.5
  , c1 51 1.0
  , c4 51 2.0
  -- s
  , c1 51 2.25
  , c2 51 2.5
  , c3 51 2.75
  -- e
  , c1 51 3.0
  , c4 51 4.0
  , c1 52 1.0
  , c4 52 2.0
  -- s
  , c1 52 2.25
  , c2 52 2.5
  , c3 52 2.75
  -- e
  , c1 52 3.0
  , c4 52 4.0
  , c4 52 4.25
  , c4 52 4.5
  , c4 52 4.75
  , c1 53 1.0
  , c2 53 1.0
  , c3 53 2.0
  , c4 53 2.0
  -- s
  , c4 53 2.25
  , c3 53 2.5
  , c2 53 2.75
  -- e 
  , c1 53 3.0
  , c2 53 4.0
  , c3 53 4.0
  , c4 53 4.0
  , c1 54 1.0
  , c2 54 1.0
  , c3 54 2.0
  , c4 54 2.0
  -- s
  , c4 54 2.25
  , c3 54 2.5
  , c2 54 2.75
  -- e
  , c1 54 3.0
  , c2 54 4.0
  , c3 54 4.0
  , c4 54 4.0
  , c1 55 1.0
  , c2 55 1.0
  , c3 55 2.0
  , c4 55 2.0
  -- s
  , c1 55 2.25
  , c2 55 2.5
  , c3 55 2.75
  -- e
  , c1 55 3.0
  , c2 55 3.0
  , c3 55 4.0
  , c4 55 4.0
  , c1 56 1.0
  , c2 56 1.0
  , c3 56 2.0
  , c4 56 2.0
  -- s
  , c1 56 2.25
  , c2 56 2.5
  , c3 56 2.75
  -- e
  , c1 56 3.0
  , c2 56 3.0
  , c1 56 4.0
  , c2 56 4.0
  , c3 56 4.25
  , c4 56 4.25
  , c1 56 4.5
  , c2 56 4.5
  , c3 56 4.75
  , c4 56 4.75
  ]

-- | 57 - 64  
lastrise :: Array Note
lastrise = join
  [ c1 57 1.0
  , c2 57 1.5
  , c3 57 2.0
  -- s 
  , c4 57 2.25
  , c3 57 2.5
  , c2 57 2.75
  -- e
  , c1 57 3.0
  , c2 57 3.5
  , c3 57 4.0
  , c4 57 4.5
  , c1 58 1.0
  , c2 58 1.5
  , c3 58 2.0
  -- s 
  , c1 58 2.25
  , c2 58 2.5
  , c3 58 2.75
  -- e
  , c1 58 3.0
  , c2 58 3.5
  , c3 58 4.0
  , c4 58 4.5
  , c1 59 1.0
  , c2 59 1.5
  , c3 59 2.0
  -- s
  , c4 59 2.25
  , c3 59 2.5
  , c2 59 2.75
  -- e
  , c1 59 3.0
  , c2 59 3.5
  , c3 59 4.0
  , c4 59 4.5
  , c1 60 1.0
  , c2 60 1.5
  , c3 60 2.0
  -- s
  , c1 60 2.25
  , c2 60 2.5
  , c3 60 2.75
  -- e
  , c1 60 3.0
  , c2 60 3.5
  , c3 60 4.0
  , c4 60 4.5
  , c1 61 1.0
  , c2 61 1.5
  , c3 61 2.0
  -- s
  , c4 61 2.25
  , c3 61 2.5
  , c2 61 2.75
  -- e
  , c3 61 3.0
  -- s
  , c1 61 3.25
  , c2 61 3.5
  , c3 61 3.75
  -- e
  , c2 61 4.0
  -- s
  , c4 61 4.25
  , c3 61 4.5
  , c2 61 4.75
  -- e
  , c3 62 1.0
  -- s
  , c1 62 1.25
  , c2 62 1.5
  , c3 62 1.75
  -- e
  , c2 62 2.0
  -- s
  , c4 62 2.25
  , c3 62 2.5
  , c2 62 2.75
  -- e
  , c3 62 3.0
  -- s
  , c1 62 3.25
  , c2 62 3.5
  , c3 62 3.75
  -- e
  , c3 62 4.0
  -- s
  , c4 62 4.25
  , c3 62 4.5
  , c2 62 4.75
  -- e
  , c4 63 1.0
  , c3 63 1.0
  -- s
  , c1 63 1.25
  , c2 63 1.5
  , c3 63 1.75
  -- e
  , c1 63 2.0
  , c2 63 2.0
  -- s
  , c4 63 2.25
  , c3 63 2.5
  , c2 63 2.75
  -- e
  , c4 63 3.0
  , c3 63 3.0
  -- s
  , c1 63 3.25
  , c2 63 3.5
  , c3 63 3.75
  -- e
  , c1 63 4.0
  , c2 63 4.0
  -- s
  , c4 63 4.25
  , c3 63 4.5
  , c2 63 4.75
  -- e
  , c4 64 1.0
  , c3 64 1.0
  -- s
  , c1 64 1.25
  , c2 64 1.5
  , c3 64 1.75
  -- e
  , c1 64 2.0
  , c2 64 2.0
  -- s
  , c4 64 2.25
  , c3 64 2.5
  , c2 64 2.75
  -- e
  , c3 64 3.0
  , c4 64 3.0
  -- s
  , c1 64 3.25
  , c2 64 3.5
  , c3 64 3.75
  -- e
  -- s
  , c1 64 4.25
  , c2 64 4.25
  , c3 64 4.5
  , c4 64 4.5
  -- e
  ]

fizzle :: Array Note
fizzle = join [ c1 65 1.0, c2 65 1.0, c3 65 1.0, c4 65 1.0 ]

readable :: Note -> { measure :: Int, beat :: Number, column :: Int }
readable (Note { timing, column: Column (column) }) =
  { measure, beat, column: column + 1 }
  where
  inTempo = timing * tempo / 60.0
  measure = floor (inTempo / 4.0) + 1
  beat = (inTempo - (toNumber (floor (inTempo / 4.0))) * 4.0) + 1.0

validate :: Array Note -> NotesOrErrors
validate arr = case w of
  [] -> Notes a
  _ -> Errors w
  where
  traversed = arr # traverse \a -> do
    n <- get
    put a
    when ((unwrap n).timing > (unwrap a).timing) do
      tell
        [ "Timing error, the note at " <> JSON.writeJSON (readable n)
            <> " comes after "
            <>
              JSON.writeJSON (readable a)
        ]
    when (a == n) do
      tell
        [ "Equal notes error " <> JSON.writeJSON (readable n) <> " overlaps " <>
            JSON.writeJSON (readable a)
        ]
    pure a
  a /\ w = runWriter $ evalStateT traversed (Note { timing: 0.0, column: col1 })

piece :: NotesOrErrors
piece = validate $ groove
  <> rise
  <> groove2
  <> rigroove
  <> ethereal
  <> ethgroove
  <> mutegroove
  <> lastrise
  <> fizzle
