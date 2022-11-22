-- | # LVL.99
-- |
-- | By Akira Complex
-- |
-- | Global form
-- | 1-18  | 18-34       | 34-66 | 66-82  | 82-98        | 98-130 | 130-(146)-152
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
module Charts.AkiraComplex.NOISZ where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (sortBy)
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V, invalid, validation)
import Effect.Exception (Error, error)
import Foreign (ForeignError(..), MultipleErrors)
import Joyride.Types (Column(..), EventV0(..), Event_(..), Position(..), Track(..), Whitelist(..))

data OneTwoThreeFour = One | Two | Three | Four

derive instance Eq OneTwoThreeFour
derive instance Ord OneTwoThreeFour

asInt :: OneTwoThreeFour -> Int
asInt One = 1
asInt Two = 2
asInt Three = 3
asInt Four = 4

tempo :: Int
tempo = 170

mbToTime :: Int -> OneTwoThreeFour -> Number
mbToTime m b = mbsToTime m b 0.0

mbsToTime :: Int -> OneTwoThreeFour -> Number -> Number
mbsToTime m b s = (toNumber ((((m - 1) * 4) + (asInt b - 1))) + s) * 60.0 / (toNumber tempo)

long :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Number -> Array Event_
long startM startB endM endB column length = pure $ EventV0 $ LongEventV0
  { marker1Time: mbToTime startM startB
  , marker2Time: mbToTime endM endB
  , audioURL: Nothing
  , column
  , length
  , name: Nothing
  , version: mempty
  }


leap :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Position -> Array Event_
leap startM startB endM endB column position = pure $ EventV0 $ LeapEventV0
  { marker1Time: mbToTime startM startB
  , marker2Time: mbToTime endM endB
  , audioURL: Nothing
  , column
  , position
  , name: Nothing
  , version: mempty
  }

leap1 :: Int -> OneTwoThreeFour -> Column -> Position -> Array Event_
leap1 i n = leap i n (i + 1) n

shift' :: Int -> OneTwoThreeFour -> Event_ -> Event_
shift' i b = shift $ toNumber ((i * 4) + asInt b)

shift'' :: Int -> Event_ -> Event_
shift'' i = shift $ toNumber (i * 4)

flipColumn :: Column -> Column
flipColumn C1 = C17
flipColumn C2 = C16
flipColumn C3 = C15
flipColumn C4 = C14
flipColumn C5 = C13
flipColumn C6 = C12
flipColumn C7 = C11
flipColumn C8 = C10
flipColumn C9 = C9
flipColumn C10 = C8
flipColumn C11 = C7
flipColumn C12 = C6
flipColumn C13 = C5
flipColumn C14 = C4
flipColumn C15 = C3
flipColumn C16 = C2
flipColumn C17 = C1


mirror :: Event_ -> Event_
mirror = toColumn flipColumn

toColumn :: (Column -> Column) -> Event_ -> Event_
toColumn f = go
  where
  go (EventV0 (BasicEventV0 e)) = EventV0 $ BasicEventV0 $ e
    { column = f e.column
    }
  go (EventV0 (LeapEventV0 e)) = EventV0 $ LeapEventV0 e
    { column = f e.column
    }
  go (EventV0 (LongEventV0 e)) = EventV0 $ LongEventV0 e
    { column = f e.column
    }
shift :: Number -> Event_ -> Event_
shift n' = go
  where
  n = n' * 60.0 / toNumber tempo
  go (EventV0 (BasicEventV0 e)) = EventV0 $ BasicEventV0 $ e
    { marker1Time = e.marker1Time + n
    , marker2Time = e.marker2Time + n
    , marker3Time = e.marker3Time + n
    , marker4Time = e.marker4Time + n
    }
  go (EventV0 (LeapEventV0 e)) = EventV0 $ LeapEventV0 e
    { marker1Time = e.marker1Time + n
    , marker2Time = e.marker2Time + n
    }
  go (EventV0 (LongEventV0 e)) = EventV0 $ LongEventV0 e
    { marker1Time = e.marker1Time + n
    , marker2Time = e.marker2Time + n
    }

dilate :: Number -> Event_ -> Event_
dilate n = go
  where
  go (EventV0 (BasicEventV0 e)) = EventV0 $ BasicEventV0 $ e
    { marker1Time = e.marker1Time
    , marker2Time = (e.marker2Time - e.marker1Time) * n + e.marker1Time
    , marker3Time = (e.marker3Time - e.marker1Time) * n + e.marker1Time
    , marker4Time = (e.marker4Time - e.marker1Time) * n + e.marker1Time
    }
  go (EventV0 (LeapEventV0 e)) = EventV0 $ LeapEventV0 e
    { marker1Time = e.marker1Time
    , marker2Time = (e.marker2Time - e.marker1Time) * n + e.marker1Time
    }
  go (EventV0 (LongEventV0 e)) = EventV0 $ LongEventV0 e
    { marker1Time = e.marker1Time
    , marker2Time = (e.marker2Time - e.marker1Time) * n + e.marker1Time
    }

basic1313 :: Int -> Column -> Array Event_
basic1313 i = basic i One Three One Three

basic2424 :: Int -> Column -> Array Event_
basic2424 i = basic i Two Four Two Four

basic3131 :: Int -> Column -> Array Event_
basic3131 i = basic i Three One Three One

basic4242 :: Int -> Column -> Array Event_
basic4242 i = basic i Four Two Four Two

basic1234 :: Int -> Column -> Array Event_
basic1234 i = basic i One Two Three Four

basic2341 :: Int -> Column -> Array Event_
basic2341 i = basic i Two Three Four One

basic3412 :: Int -> Column -> Array Event_
basic3412 i = basic i Three Four One Two

basic4123 :: Int -> Column -> Array Event_
basic4123 i = basic i Four One Two Three

cascade1122 :: Int -> Column -> Array Event_
cascade1122 i c = basic'' i One 0.0 i One 0.5 i Two 0.0 i Two 0.5 c
  <> basic'' i One 0.5 i Two 0.0 i Two 0.5 i Three 0.0 (c <> C1)
  <> basic'' i Two 0.0 i Two 0.5 i Three 0.0 i Three 0.5 (c <> C2)
  <> basic'' i Two 0.5 i Three 0.0 i Three 0.5 i Four 0.0 (c <> C3)

cascade2233 :: Int -> Column -> Array Event_
cascade2233 i c = map (shift 1.0) $ cascade1122 i c

cascade3344 :: Int -> Column -> Array Event_
cascade3344 i c = map (shift 2.0) $ cascade1122 i c

cascade4411 :: Int -> Column -> Array Event_
cascade4411 i c = map (shift 3.0) $ cascade1122 i c

basic :: Int -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> Column -> Array Event_
basic m1 b1 b2 b3 b4 = basic' m1 b1 m2 b2 m3 b3 m4 b4
  where
  m2 = if b2 <= b1 then m1 + 1 else m1
  m3 = if b3 <= b2 then m2 + 1 else m2
  m4 = if b4 <= b3 then m3 + 1 else m3

basic'' :: Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Column -> Array Event_
basic'' m1 b1 s1 m2 b2 s2 m3 b3 s3 m4 b4 s4 column = pure $ EventV0 $ BasicEventV0
  { marker1Time: mbsToTime m1 b1 s1
  , marker2Time: mbsToTime m2 b2 s2
  , marker3Time: mbsToTime m3 b3 s3
  , marker4Time: mbsToTime m4 b4 s4
  , marker1AudioURL: Nothing
  , marker2AudioURL: Nothing
  , marker3AudioURL: Nothing
  , marker4AudioURL: Nothing
  , column
  , name: Nothing
  , version: mempty
  }

basic' :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Array Event_
basic' m1 b1 m2 b2 m3 b3 m4 b4 = basic'' m1 b1 0.0 m2 b2 0.0 m3 b3 0.0 m4 b4 0.0

intro1 :: Array Event_
intro1 = join
  [ long 2 One 5 One C7 2.0
  , long 6 One 9 One C9 2.0
  , long 10 One 12 One C6 1.5
  , leap 11 One 13 One C8 Position1
  , long 12 One 14 One C10 1.5
  , leap 13 One 15 One C8 Position2
  , long 14 One 17 One C9 2.0
  , long 16 One 19 One C10 1.0
  , leap 16 Three 17 Three C8 Position1
  , long 17 One 19 One C5 1.0
  , leap 17 Three 18 Three C8 Position2
  ]

ice1 :: Array Event_
ice1 = join
  [ basic1313 18 C7
  , basic1313 20 C9
  , basic1313 22 C7
  , basic1313 23 C7
  , basic1313 24 C9
  , basic1313 25 C9
  , basic3131 25 C9
  , basic4242 25 C9
  -----------------
  , basic1313 26 C6
  , basic2424 26 C7
  , basic3131 26 C6
  , basic4242 26 C7
  --
  , basic1313 27 C6
  , basic2424 27 C7
  , basic3131 27 C6
  , basic4242 27 C7
  , basic1313 27 C8
  , basic2424 27 C9
  , basic3131 27 C8
  , basic4242 27 C9
  --
  , basic1234 28 C5
  , basic1234 28 C6
  , basic2341 28 C7
  , basic2341 28 C8
  , basic3412 28 C9
  , basic4123 28 C10
  , basic4123 28 C11
  , basic4123 28 C12
  --
  , basic1234 29 C4
  , basic1234 29 C6
  , basic1234 29 C7
  , basic2341 29 C5
  , basic2341 29 C6
  , basic2341 29 C8
  , basic3412 29 C6
  , basic3412 29 C7
  , basic3412 29 C9
  , basic4123 29 C7
  , basic4123 29 C8
  , basic4123 29 C10
  --
  , cascade1122 30 C4
  , cascade3344 30 C6
  , cascade1122 31 C8
  , cascade3344 31 C10
  , cascade1122 32 C4
  , cascade2233 32 C4
  , cascade3344 32 C6
  , cascade4411 32 C6
  , cascade1122 33 C8
  , cascade2233 33 C8
  , cascade3344 33 C10
  , cascade4411 33 C10
  , long 32 Three 34 One C4 2.0
  , long 33 One 34 One C5 1.0
  , leap 33 Three 34 One C6 Position1
  , leap 33 Four 34 Two C6 Position1
  ]

fight1 :: Array Event_
fight1 = join
  [ fullPart
  , map (mirror <<< shift'' 8) fullPart
  , echoDilated $ map (shift'' 16) fullPart
  , echoDilated $ map (mirror <<< shift'' 24) fullPart
  ]
  where
  echoDilated :: Array Event_ -> Array Event_
  echoDilated = foldl (\b a -> [ a, dilate 2.0 $ toColumn (_ <> C8) a ] <> b) []
  cell1 n = join
    [ leap1 n One C7 Position1
    , leap1 n Two C9 Position2
    , leap1 n Three C6 Position3
    , leap1 n Four C5 Position4
    , leap1 (n + 1) One C7 Position1
    , long (n + 1) Two (n + 3) Two C6 2.0
    , leap1 (n + 1) Three C9 Position3
    , long (n + 1) Four (n + 3) Four C4 2.0
    ]
  fullPart = join
    [ cell1 34
    , cell1 36
    , basic 37 Three Four One Two C7
    , basic 37 Four One Two Three C7
    , basic1313 38 C3
    , basic1313 38 C4
    , basic1313 38 C5
    , basic1313 38 C6
    , basic3131 38 C7
    , basic3131 38 C8
    , basic3131 38 C9
    , basic3131 38 C10
    , basic1313 39 C11
    , basic1313 39 C12
    , basic3131 39 C13
    , leap 39 Four 40 Two C11 Position1
    , basic1313 40 C3
    , basic1313 40 C4
    , basic1313 40 C5
    , basic1313 40 C6
    , basic2424 40 C5
    , basic2424 40 C6
    , basic3131 40 C7
    , basic3131 40 C8
    , basic3131 40 C9
    , basic3131 40 C10
    , basic4242 40 C9
    , basic4242 40 C10
    , basic1313 41 C11
    , basic1313 41 C12
    , basic2424 41 C12
    , basic3131 41 C13
    , basic4242 41 C13
    , leap 41 Four 42 Two C11 Position1
    ]

intro2 :: Array Event_
intro2 = map (shift'' 64) intro1


ice2 :: Array Event_
ice2 = map (shift'' 64) ice1

fight2 :: Array Event_
fight2 = map (shift'' 64) fight1

outro :: Array Event_
outro = map (shift'' 128) intro1

type Events = V MultipleErrors (Array Event_)

noDice :: forall res. Int -> String -> V MultipleErrors res
noDice i s = invalid $ pure (ErrorAtIndex i (ForeignError s))

validateEvents :: Array Event_ -> Events
validateEvents = traverseWithIndex \i -> case _ of
  EventV0 e -> case e of
    BasicEventV0 be ->
      if be.marker1Time >= be.marker2Time then
        noDice i ("Marker 1 time inconsistent: " <> show be.marker1Time <> "should be less than " <> show be.marker2Time)
      else if be.marker2Time >= be.marker3Time then
        noDice i ("Marker 2 time inconsistent: " <> show be.marker2Time <> "should be less than " <> show be.marker3Time)
      else if be.marker3Time >= be.marker4Time then
        noDice i ("Marker 3 time inconsistent: " <> show be.marker2Time <> "should be less than " <> show be.marker4Time)
      else if be.marker1Time < 0.0 then
        noDice i ("Marker 1 time should be positive, got: " <> show be.marker1Time)
      else pure $ EventV0 $ BasicEventV0 be
    LeapEventV0 le ->
      if le.marker1Time >= le.marker2Time then
        noDice i ("Marker 1 time inconsistent: " <> show le.marker1Time <> "should be less than " <> show le.marker2Time)
      else if le.marker1Time < 0.0 then
        noDice i ("Marker 1 time should be positive, got: " <> show le.marker1Time)
      else pure $ EventV0 $ LeapEventV0 le
    LongEventV0 le ->
      if le.marker1Time >= le.marker2Time then
        noDice i ("Marker 1 time inconsistent: " <> show le.marker1Time <> "should be less than " <> show le.marker2Time)
      else if le.marker1Time < 0.0 then
        noDice i ("Marker 1 time should be positive, got: " <> show le.marker1Time)
      else if le.length <= 0.0 then
        noDice i ("Length must be positive, got: " <> show le.length)
      else pure $ EventV0 $ LongEventV0 le

piece :: forall m. MonadThrow Error m => m { track :: Track, events :: Array Event_ }
piece = do
  let events' = intro1 <> ice1 <> fight1 <> intro2 <> ice2 <> fight2 <> outro
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    { track: TrackV0
        { version: mempty
        , url: "https://cdn.filestackcontent.com/kG1ZasfRPSvsRd2QAMux"
        , title: Just "NOISZ"
        , private: true
        , whitelist: Whitelist []
        , owner: "OKA4OPZguFZOv9p58TBbokciIlq2"
        }
    , events: events # sortBy
        ( compare `on` case _ of
            EventV0 e -> case e of
              BasicEventV0 ee -> ee.marker1Time
              LeapEventV0 ee -> ee.marker1Time
              LongEventV0 ee -> ee.marker1Time
        )
    }