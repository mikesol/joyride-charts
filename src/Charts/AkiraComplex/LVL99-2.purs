module Charts.AkiraComplex.LVL99.V2 where

-- |
-- | By Akira Complex
-- |
-- | Global form
-- | 1-17  | 18-34       | 34-66 | 66-82  | 82-98        | 98-130 | 130-(146)-152
-- | intro | ice / chain | fast  | intro2 | ice / chain2 | fast2  | outro (like intro)
-- | ## m. 1-18
-- | 
-- | Intro
-- |
-- | Longs on appearances, the occasional red, very open feel
-- |
-- | ## m. 18-26
-- |
-- | First "ice on my writsts and chain on my neck."
-- | Indivudal points floating in.
-- |
-- | ## m. 26-34
-- |
-- | Builds up.
-- | Indivudal points floating in, means adding greens, in the mix in, biased right.
-- | Left will be red right at the end
-- |
-- | ## m. 34-38
-- |
-- | Low density, high energy.
-- | Give people the opportunity to jump.
-- |
-- | ## m. 38-42
-- |
-- | Sloowwww.
-- | But lots of stuff. Thick
-- |
-- |
-- | ## m. 42-46
-- |
-- | Low density, high energy.
-- | Give people the opportunity to jump. A bit more spread than m. 34.
-- |
-- | ## m. 46-50
-- |
-- | Sloowwww.
-- | Lots of stuff, leads people to the right.
-- |
-- | ## m. 50-54
-- |
-- | Low density, high energy.
-- | Trails left, settles far left.
-- |
-- | ## m. 54-58
-- |
-- | Sloowwww.
-- | Flanking tiles trending right immediately.
-- |                              ---
-- |                  ---
-- |         ---
-- | ---
-- |
-- | ## m. 58-62
-- |
-- | Low density, high energy.
-- | All over the map
-- |
-- | ## m. 62-66
-- |
-- | Sloowwww.
-- | Mega-reward for the front. Very broad & slow.
-- |
-- | ## m. 66-74
-- |
-- | Spacey.
-- | Lots of greens.
-- |
-- | ## m. 72-82
-- |
-- | Still spacey.
-- | But intersperse some red.
-- | 
-- | ## m. 82-90
-- |
-- | Second "ice on my writsts and chain on my neck."
-- | 1-per beat, débit constant
-- |
-- | ## m. 90-94
-- |
-- | Second "ice on my writsts and chain on my neck."
-- | Staggered motion, things catching up with other things
-- |
-- | ## m. 94-98
-- |
-- | Two staggers across the field
-- | First slow
-- | Second following fast
-- |
-- | ## m. 98-102
-- |
-- | Slow, allow jumps, but now with a green to distract
-- |
-- |
-- | ## m. 102-106
-- |
-- | Mega thick
-- |
-- | ## m. 106-110
-- |
-- | Isolated reds, very spread, push to front
-- |
-- | ## m. 110-114
-- |
-- | Huge reward for front, nothing for back
-- | Happens twice
-- |
-- | ## m. 114-118
-- |
-- | Isolated reds, very consolodated, push to back
-- |
-- | ## m. 118-122
-- |
-- | Huge reward for back, nothing for front
-- | Happens twice
-- | Add green to shake stuff up?
-- |
-- | ## m. 122-126
-- |
-- | Somewhat dense
-- |
-- | ## m. 126-130
-- |
-- | Mega dense in first 2m
-- | Whittles down to almost nothing (or sudden to nothing) in middle
-- |
-- | ## m. 130-146
-- |
-- | Greens with the occassional red
-- | 
-- | ## m. 146-out
-- | Only reds to 1 very slow green
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

-- | intro
-- | 1-17
intro :: Array Note
intro = join
  [ c1 2 1.0
  , c4 4 1.0
  , c2 6 1.0
  , c3 8 1.0
  , c1 10 1.0
  , c4 12 1.0
  , c2 14 1.0
  , c3 17 1.0
  , c3 17 2.0
  , c3 17 3.0
  , c3 17 4.0
  ]

-- | ice / chain
-- | 18-33     
iceChain :: Array Note
iceChain = join
  [ c1 18 1.0
  , c4 18 3.0
  , c2 19 1.0
  , c3 19 3.0
  , c1 20 1.0
  , c4 20 3.0
  , c2 21 1.0
  , c3 21 3.0
  --
  , c1 22 1.0
  , c4 22 2.0
  , c4 22 3.0
  , c2 22 4.0
  , c2 23 1.0
  , c3 23 2.0
  , c3 23 3.0
  , c1 23 4.0
  , c1 24 1.0
  , c4 24 2.0
  , c4 24 3.0
  , c2 24 4.0
  , c2 25 1.0
  , c3 25 2.0
  , c3 25 3.0
  , c4 25 4.0
  , c4 25 4.5
  ---
  ---
  , c1 26 1.0
  , c4 26 2.0
  , c1 26 3.0
  , c4 26 4.0
  , c1 27 1.0
  , c4 27 2.0
  , c1 27 3.0
  , c4 27 4.0
  --
  , c1 28 1.0
  , c3 28 2.0
  , c4 28 2.0
  , c1 28 3.0
  , c2 28 4.0
  , c4 28 4.0
  , c1 29 1.0
  , c3 29 2.0
  , c4 29 2.0
  , c1 29 3.0
  , c2 29 4.0
  , c4 29 4.0
  , c1 30 1.0
  , c2 30 1.0
  , c3 30 2.0
  , c4 30 2.0
  , c1 30 3.0
  , c3 30 3.0
  , c2 30 4.0
  , c4 30 4.0
  , c1 31 1.0
  , c2 31 1.0
  , c3 31 2.0
  , c4 31 2.0
  , c1 31 3.0
  , c3 31 3.0
  , c1 31 3.5
  , c3 31 3.5
  , c2 31 4.0
  , c4 31 4.0
  , c2 32 1.0
  , c1 32 1.5
  , c4 32 2.0
  , c3 32 2.5
  , c2 32 3.0
  , c1 32 3.5
  , c4 32 4.0
  , c3 32 4.5
  --
  , c2 33 1.0
  , c1 33 1.5
  , c4 33 2.0
  , c3 33 2.0
  , c4 33 2.5
  , c3 33 2.5
  , c2 33 3.0
  , c1 33 3.0
  , c4 33 3.5
  , c3 33 3.5
  , c2 33 4.0
  , c1 33 4.0
  , c4 33 4.5
  , c3 33 4.5
  ]

-- | fast
-- | 34-65
fast ∷ Array Note
fast = join
  [ c1 34 1.0
  , c2 34 2.0
  , c1 34 3.0
  , c2 34 4.0
  , c1 35 1.0
  , c3 35 1.0
  , c2 35 2.0
  , c4 35 2.0
  , c1 35 3.0
  , c3 35 3.0
  , c2 35 4.0
  , c4 35 4.0
  -- 
  , c1 36 1.0
  , c2 36 2.0
  , c1 36 3.0
  , c2 36 4.0
  , c1 37 1.0
  , c3 37 1.0
  , c2 37 2.0
  , c4 37 2.0
  , c1 37 3.0
  , c3 37 3.0
  , c2 37 4.0
  , c4 37 4.0
  , c2 37 4.5
  , c4 37 4.5
  --
  , c1 38 1.0
  , c2 38 1.0
  , c3 38 1.5
  , c4 38 1.5
  , c1 38 2.0
  , c2 38 2.0
  , c1 38 2.5
  , c2 38 2.5
  , c1 38 3.0
  , c2 38 3.0
  , c3 38 3.5
  , c4 38 3.5
  , c1 38 4.0
  , c2 38 4.0
  , c1 38 4.5
  , c2 38 4.5
  --
  , c1 39 1.0
  , c2 39 1.0
  , c3 39 1.5
  , c4 39 1.5
  , c1 39 2.0
  , c2 39 2.0
  , c1 39 2.5
  , c2 39 2.5
  , c1 39 3.0
  , c2 39 3.5
  , c3 39 4.0
  , c4 39 4.5
  --
  , c1 40 1.0
  , c2 40 1.0
  , c3 40 1.5
  , c4 40 1.5
  , c1 40 2.0
  , c2 40 2.0
  , c1 40 2.5
  , c2 40 2.5
  , c1 40 3.0
  , c2 40 3.0
  , c3 40 3.5
  , c4 40 3.5
  , c1 40 4.0
  , c2 40 4.0
  , c1 40 4.5
  , c2 40 4.5
  --
  , c1 41 1.0
  , c2 41 1.0
  , c3 41 1.5
  , c4 41 1.5
  , c1 41 2.0
  , c2 41 2.0
  , c1 41 2.5
  , c2 41 2.5
  , c1 41 3.0
  , c2 41 3.5
  , c3 41 4.0
  , c4 41 4.5
  --
  --
  --
  --
  --
  , c1 42 1.0
  , c3 42 1.5
  , c1 42 2.0
  , c2 42 2.5
  , c1 42 3.0
  , c3 42 3.5
  , c1 42 4.0
  , c2 42 4.5
  , c1 43 1.0
  , c3 43 1.5
  , c1 43 2.0
  , c2 43 2.5
  , c1 43 3.0
  , c3 43 3.5
  , c1 43 4.0
  , c2 43 4.5
  --
  , c1 44 1.0
  , c3 44 1.5
  , c4 44 1.5
  , c1 44 2.0
  , c2 44 2.5
  , c3 44 2.5
  , c1 44 3.0
  , c3 44 3.5
  , c4 44 3.5
  , c1 44 4.0
  , c2 44 4.5
  , c3 44 4.5
  , c2 45 1.0
  , c3 45 1.5
  , c4 45 1.5
  , c1 45 2.0
  , c2 45 2.5
  , c4 45 2.5
  , c2 45 3.0
  , c3 45 3.5
  , c4 45 3.5
  , c1 45 4.0
  , c2 45 4.5
  , c3 45 4.5
  --
  , c1 46 1.0
  , c2 46 1.0
  , c2 46 2.0
  , c2 46 2.5
  , c3 46 3.0
  , c4 46 3.0
  , c3 46 4.0
  , c4 46 4.0
  , c1 46 4.5
  , c2 46 4.5
  , c1 47 1.5
  , c2 47 1.5
  , c1 47 2.5
  , c2 47 2.5
  , c3 47 3.0
  , c4 47 3.0
  , c3 47 4.0
  , c4 47 4.0
  , c3 47 4.5
  , c4 47 4.5
  --
  , c1 48 1.0
  , c2 48 1.0
  , c2 48 2.0
  , c2 48 2.5
  , c3 48 3.0
  , c4 48 3.0
  , c3 48 4.0
  , c4 48 4.0
  , c1 48 4.5
  , c2 48 4.5
  , c1 49 1.5
  , c2 49 1.5
  , c1 49 2.5
  , c2 49 2.5
  , c3 49 3.0
  , c4 49 3.0
  , c4 49 4.0
  , c3 49 4.25
  , c2 49 4.5
  --
  , c1 50 1.0
  , c2 50 1.0
  , c2 50 2.0
  , c3 50 2.0
  , c1 50 3.0
  , c2 50 3.0
  , c2 50 4.0
  , c3 50 4.0
  , c1 51 1.0
  , c4 51 1.0
  , c2 51 2.0
  , c3 51 2.0
  , c1 51 3.0
  , c4 51 3.0
  , c2 51 4.0
  , c3 51 4.0
  , c1 52 1.0
  , c2 52 1.0
  , c2 52 2.0
  , c3 52 2.0
  , c1 52 3.0
  , c2 52 3.0
  , c2 52 4.0
  , c3 52 4.0
  , c1 53 1.0
  , c4 53 1.0
  , c2 53 2.0
  , c3 53 2.0
  , c1 53 3.0
  , c4 53 3.0
  , c2 53 4.0
  , c3 53 4.0
  --
  , c1 54 1.0
  , c3 54 1.5
  , c4 54 2.0
  , c2 54 2.5
  , c3 54 3.0
  , c1 54 3.5
  , c4 54 4.0
  , c2 54 4.5
  , c4 55 1.0
  , c1 55 1.5
  , c3 55 2.0
  , c2 55 2.5
  , c1 55 3.0
  , c2 55 3.5
  , c3 55 4.0
  , c4 55 4.5
  , c1 56 1.0
  , c4 56 1.5
  , c2 56 2.0
  , c3 56 2.5
  , c4 56 3.0
  , c2 56 3.5
  , c3 56 4.0
  , c1 56 4.5
  , c2 57 1.0
  , c1 57 1.5
  , c4 57 2.0
  , c3 57 2.5
  , c1 57 3.0
  , c2 57 3.5
  , c3 57 4.0
  , c4 57 4.5
  --
  , c1 58 1.0
  , c2 58 1.0
  , c3 58 1.0
  , c2 58 2.0
  , c3 58 2.0
  , c4 58 2.0
  , c1 58 3.0
  , c2 58 3.0
  , c3 58 3.0
  , c2 58 4.0
  , c3 58 4.0
  , c4 58 4.0
  , c1 59 1.0
  , c2 59 1.0
  , c3 59 1.0
  , c2 59 2.0
  , c3 59 2.0
  , c4 59 2.0
  , c1 59 3.0
  , c2 59 3.0
  , c3 59 3.0
  , c2 59 4.0
  , c3 59 4.0
  , c4 59 4.0
  , c1 60 1.0
  , c2 60 1.0
  , c4 60 1.0
  , c2 60 2.0
  , c3 60 2.0
  , c4 60 2.0
  , c1 60 3.0
  , c3 60 3.0
  , c4 60 3.0
  , c1 60 4.0
  , c2 60 4.0
  , c3 60 4.0
  , c2 61 1.0
  , c3 61 1.0
  , c4 61 1.0
  , c1 61 2.0
  , c3 61 2.0
  , c4 61 2.0
  , c1 61 3.0
  , c2 61 3.0
  , c3 61 3.0
  , c1 61 4.0
  , c2 61 4.0
  , c4 61 4.0
  --
  , c1 62 1.0
  , c2 62 1.0
  , c4 62 1.5
  , c3 62 1.75
  , c1 62 2.0
  , c2 62 2.0
  , c4 62 2.5
  , c3 62 2.5
  , c1 62 3.0
  , c3 62 3.5
  , c4 62 3.5
  , c1 62 4.0
  , c3 62 4.5
  , c4 62 4.75
  --
  , c1 63 1.0
  , c2 63 1.0
  , c4 63 1.5
  , c3 63 1.75
  , c1 63 2.0
  , c2 63 2.0
  , c4 63 2.5
  , c3 63 2.5
  , c1 63 3.0
  , c3 63 3.5
  , c4 63 3.5
  , c1 63 4.0
  , c2 63 4.0
  , c3 63 4.0
  , c4 63 4.0
  --
  --
  , c1 64 1.0
  , c2 64 1.0
  , c4 64 1.5
  , c3 64 1.75
  , c1 64 2.0
  , c2 64 2.0
  , c4 64 2.5
  , c3 64 2.5
  , c1 64 3.0
  , c3 64 3.5
  , c4 64 3.5
  , c1 64 4.0
  , c3 64 4.5
  , c4 64 4.75
  --
  , c1 65 1.0
  , c2 65 1.0
  , c4 65 1.5
  , c3 65 1.75
  , c1 65 2.0
  , c2 65 2.0
  , c4 65 2.5
  , c3 65 2.5
  , c4 65 3.0
  , c3 65 3.25
  , c2 65 3.5
  , c1 65 3.75
  , c4 65 4.0
  , c3 65 4.25
  , c2 65 4.5
  , c1 65 4.75
  ]

-- | intro2
-- | 66-81
intro2 :: Array Note
intro2 = join
  [ c4 66 1.0
  , c3 66 1.0
  ]

-- iceChain2
-- | 82-97
iceChain2 :: Array Note
iceChain2 = join
  [
  ]

-- | fast2
-- | 98-129
fast2 :: Array Note
fast2 = join
  [
  ]

-- | outro
-- | 130-(146)-152
outro :: Array Note
outro = join
  [
  ]

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
piece = validate $ intro
  <> iceChain
  <> fast
  <> intro2
  <> iceChain2
  <> fast2
  <> outro
