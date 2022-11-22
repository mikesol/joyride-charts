module Charts.AkiraComplex.ShowMeHow where

-- SHOW ME HOW
-- | 1-8    | 9-16 | 17-24   | 25-32    | 33-40    | 41-48     | 49-56      | 56 - 64  | 65     |
-- | ------ | ---- | ------- | -------- | -------- | --------- | ---------- | -------- | ------ |
-- | groove | rise | groove2 | rigroove | ethereal | ethgroove | mutegroove | lastrise | fizzle |

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Writer (runWriter, tell)
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Simple.JSON as JSON

tempo :: Number
tempo = 120.0

newtype Column = Column Int

derive newtype instance JSON.ReadForeign Column
derive newtype instance JSON.WriteForeign Column
newtype Note = Note { timing :: Number, column :: Column }
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
rise = join []

-- | 17-24  
groove2 :: Array Note
groove2 = join []

-- | 25-32 
rigroove :: Array Note
rigroove = join []

-- | 33-40    
ethereal :: Array Note
ethereal = join []

-- | 41-48     
ethgroove :: Array Note
ethgroove = join []

-- | 49-56      
mutegroove :: Array Note
mutegroove = join []

-- | 56 - 64  
lastrise :: Array Note
lastrise = join []

fizzle :: Array Note
fizzle = join []

validate :: Array Note -> NotesOrErrors
validate arr = case w of
    [] -> Notes a
    _ -> Errors w
  where
  traversed = arr # traverse \(Note a) -> do
    n <- get
    put a.timing
    when (n > a.timing) do
      tell ["Timing error" <> JSON.writeJSON a]
    pure (Note a)
  a /\ w = runWriter $ evalStateT traversed 0.0

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
