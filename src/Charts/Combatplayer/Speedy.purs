-- | # Speedy
-- |
-- | By Combatplayer
-- |
-- | | 1 - 17  | 17 - 33  | 33 - 49       | 49 - 65 | 65 - 81 | 81 - 97 | 97 - 113 |
-- | | Intro   | Speedy   | Speedy + pizz | Speedy  | Intro   | Speedy  | Outro    |

module Charts.Combatplayer.Speedy where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (sortBy, (..))
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
tempo = 168

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

pushOut :: Event_ -> Event_
pushOut = toColumn case _ of
  C1 -> C8
  C2 -> C7
  C3 -> C6
  C4 -> C5
  C5 -> C4
  C6 -> C3
  C7 -> C2
  C8 -> C1
  C10 -> C17
  C11 -> C16
  C12 -> C15
  C13 -> C14
  C14 -> C13
  C15 -> C12
  C16 -> C11
  C17 -> C10
  C9 -> C9

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

intro :: Array Event_
intro = join
  [ long 1 One 3 One C8 2.0
  , long 3 One 5 One C10 2.0
  , long 5 One 7 One C8 2.0
  , long 7 One 9 One C10 2.0
  , leap 7 One 9 One C9 Position3
  , long 9 One 11 One C8 2.0
  , long 10 One 12 One C10 2.0
  , long 11 One 13 One C7 2.0
  , long 12 One 14 One C11 2.0
  , long 13 One 15 One C8 2.0
  , long 14 One 16 One C10 2.0
  , leap 15 One 17 One C9 Position2
  , long 15 One 17 One C7 2.0
  , leap 16 One 18 One C9 Position3
  , leap 16 Three 18 One C6 Position1
  , leap 16 Three 18 One C12 Position4
  , long 16 One 18 One C11 2.0
  ]

speedy1 :: Array Event_
speedy1 = join
  [ basic1234 17 C10 -- singleton
  , basic1234 18 C8 -- singleton 
  , basic1234 19 C9 -- singleton
  , basic1234 20 C8 -- pair
  , basic1234 20 C10 -- pair
  , basic1234 21 C9 -- singleton
  , basic1234 22 C8 -- pair
  , basic1234 22 C10 -- pair
  , basic1313 23 C7 -- pair
  , basic1313 23 C11 -- pair
  , leap 24 One 25 One C8 Position1 -- leap
  , leap 24 One 25 One C10 Position2 -- leap
  , basic1313 24 C9 -- singleton
  -- cascade
  , basic1234 25 C7 -- -
  , basic2341 25 C8 --  -
  , basic3412 25 C9 --   -
  , basic4123 25 C10 --    -
  -- cascade
  , basic1234 26 C7 -- -
  , basic2341 26 C8 --  -
  , basic3412 26 C9 --   -
  , basic4123 26 C10 --    -
  -- reverse cascade
  , basic1234 27 C11 --    -
  , basic2341 27 C10 --   -
  , basic3412 27 C9 --  -
  , basic4123 27 C8 -- -
  -- reverse cascade
  , basic1234 28 C11 --    -
  , basic2341 28 C10 --   -
  , basic3412 28 C9 --  -
  , basic4123 28 C8 -- -
  -- staggered
  , basic1234 29 C7
  , basic2341 29 C8
  , basic1234 29 C9
  , basic2341 29 C10
  , basic3412 29 C9
  , basic4123 29 C10
  -- 
  , basic1234 30 C11
  , basic2341 30 C10
  , basic1234 30 C9
  , basic2341 30 C8
  --
  , basic1313 31 C5
  , basic1313 31 C6
  , basic1313 31 C8
  , basic1313 31 C10
  , basic1313 31 C12
  , basic1313 31 C13
  , basic1234 31 C7
  , basic1234 31 C11
  , basic1234 32 C7
  , basic1234 32 C11
  , leap 32 One 33 One C9 Position1
  ]

speedy2 :: Array Event_
speedy2 = go <> map (pushOut <<< shift'' 8) go
  where
  go = join
    [ basic1234 33 C6
    , basic1234 33 C7
    , basic1234 33 C9
    , basic2341 33 C8
    -- 
    , basic1234 34 C8
    , basic1234 34 C10
    , basic1234 34 C11
    , basic2341 34 C9
    -- 
    , basic1313 35 C7
    , basic1313 35 C8
    , basic1234 35 C9
    , basic1234 35 C10
    , basic3412 35 C9
    , basic3412 35 C10
    , basic1234 36 C9
    , basic1234 36 C10
    , basic3412 36 C10
    , basic3412 36 C11
    --
    , basic1234 37 C8
    , basic1234 37 C10
    , basic2341 37 C7
    , basic2341 37 C11
    , basic3412 37 C6
    , basic3412 37 C12
    , basic4123 37 C5
    , basic4123 37 C13
    , basic1234 38 C4
    , basic1234 38 C14
    , basic2341 38 C3
    , basic2341 38 C15
    , basic3412 38 C2
    , basic3412 38 C16
    , basic4123 38 C1
    , leap 38 One 39 One C9 Position1
    , basic4123 38 C17
    --
    , basic1234 39 C1
    , basic1234 39 C17
    , basic2341 39 C2
    , basic2341 39 C16
    , basic3412 39 C3
    , basic3412 39 C15
    , basic4123 39 C4
    , basic4123 39 C14
    , basic1234 40 C5
    , basic1234 40 C13
    , basic2341 40 C6
    , basic2341 40 C12
    , basic3412 40 C7
    , basic3412 40 C11
    , basic4123 40 C8
    , leap 40 One 41 One C9 Position1
    , basic4123 41 C10
    ]

speedy3 :: Array Event_
speedy3 = map (scramble <<< shift'' 16) speedy2
  where
  scramble = toColumn case _ of
    C1 -> C2
    C2 -> C4
    C3 -> C6
    C4  -> C8
    C5  -> C1
    C6  -> C3
    C7  -> C5
    C8  -> C7
    C9  -> C9
    C10  -> C11
    C11  -> C13
    C12  -> C15
    C13  -> C17
    C14  -> C10
    C15  -> C12
    C16  -> C14
    C17  -> C16

middle :: Array Event_
middle = map (shift'' 64) intro

speedy4 :: Array Event_
speedy4 = (map (mirror <<< shift'' 48) speedy2) <> join
  ( 81 .. 95 <#> \x ->
      leap x One (x + 1) One (if x `mod` 2 == 0 then C2 else C16)
        ( case x `mod` 3 of
            0 -> Position1
            1 -> Position2
            _ -> Position3
        )
  )

outro :: Array Event_
outro = map (shift'' 96) intro

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
  let events' = intro <> speedy1 <> speedy2 <> speedy3 <> middle <> speedy4 <> outro
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    { track: TrackV0
        { version: mempty
        , url: "https://cdn.filestackcontent.com/Chx54OpSmakW4Xs1I0rx"
        , title: Just "Speedy - Draft 7"
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