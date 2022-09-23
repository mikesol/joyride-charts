module Charts.DJMonad.MysteryMansionMadness where

-- | # Mystery Mansion Madness
-- |
-- | By DJ Monad
-- |
-- | | 1 - 5  | 5 - 9        | 9 - 17 | 17 - 19  | 19 - 21 | 21 - 29 | 29 - 37 | 37 - 45 |
-- | | Intro  | Intro + Beat | Rise   | Suspense | Laugh   | Groove  | Groove2 | Outro   |

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (sortBy)
import Data.Array as Array
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
tempo = 140

mbToTime :: Int -> OneTwoThreeFour -> Number
mbToTime m b = mbsToTime m b 0.0

mbsToTime :: Int -> OneTwoThreeFour -> Number -> Number
mbsToTime m b s = (toNumber ((integrate (m - 1) + (asInt b - 1))) + s) * 60.0 / (toNumber tempo)

integrate :: Int -> Int
integrate m = m * 4

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

long1 :: Int -> OneTwoThreeFour -> Column -> Number -> Array Event_
long1 i ottf = long i ottf (i + 1) ottf

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

earliest :: Array Event_ -> Number
earliest = go top
  where
  go x = Array.uncons >>> case _ of
    Nothing -> x
    Just { head: EventV0 (BasicEventV0 e), tail } -> go (min x e.marker1Time) tail
    Just { head: EventV0 (LeapEventV0 e), tail } -> go (min x e.marker1Time) tail
    Just { head: EventV0 (LongEventV0 e), tail } -> go (min x e.marker1Time) tail

squeeze :: Number -> Number -> Event_ -> Event_
squeeze n refn = go
  where
  go (EventV0 (BasicEventV0 e)) = EventV0 $ BasicEventV0 $ e
    { marker1Time = (e.marker1Time - refn) * n + refn
    , marker2Time = (e.marker2Time - refn) * n + refn
    , marker3Time = (e.marker3Time - refn) * n + refn
    , marker4Time = (e.marker4Time - refn) * n + refn
    }
  go (EventV0 (LeapEventV0 e)) = EventV0 $ LeapEventV0 e
    { marker1Time = (e.marker1Time - refn) * n + refn
    , marker2Time = (e.marker2Time - refn) * n + refn
    }
  go (EventV0 (LongEventV0 e)) = EventV0 $ LongEventV0 e
    { marker1Time = (e.marker1Time - refn) * n + refn
    , marker2Time = (e.marker2Time - refn) * n + refn
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

squeezeFromHead :: Number -> Array Event_ -> Array Event_
squeezeFromHead n l = map (squeeze n stt) l
  where
  stt = earliest l

intro :: Array Event_
intro = join
  [ long1 1 One C8 2.0
  , long1 2 One C10 2.0
  , long1 3 One C8 2.0
  , long1 4 One C10 2.0
  , leap1 4 One C9 Position1
  ]

intro'beat :: Array Event_
intro'beat = join
  [ long1 5 One C8 2.0
  , basic3412 5 C9
  , long1 6 One C10 2.0
  , basic3412 6 C9
  , long1 7 One C8 2.0
  , basic3412 7 C9
  , long1 8 One C10 2.0
  , basic3412 8 C7
  , leap1 8 One C9 Position1
  , basic3412 8 C11
  ]

rise :: Array Event_
rise =
  join
    [ basic1234 9 C8
    , basic1234 9 C10
    , leap1 9 One C9 Position1
    , basic2341 9 C8
    , basic2341 9 C10
    , basic3412 9 C8
    , basic3412 9 C10
    , basic4123 9 C8
    , basic4123 9 C10
    , basic1234 10 C8
    , basic1234 10 C10
    , basic2341 10 C8
    , basic2341 10 C10
    , leap1 10 Three C9 Position2
    , basic3412 10 C8
    , basic3412 10 C10
    , leap1 10 Four C9 Position3
    , basic4123 10 C8
    , basic4123 10 C10
    , leap1 11 One C9 Position1
    , basic1234 11 C8
    , basic1234 11 C10
    , basic1234 11 C7
    , basic1234 11 C11
    , basic2341 11 C8
    , basic2341 11 C10
    , basic2341 11 C7
    , basic2341 11 C11
    , basic3412 11 C8
    , basic3412 11 C10
    , basic3412 11 C7
    , basic3412 11 C11
    , basic4123 11 C8
    , basic4123 11 C10
    , basic4123 11 C7
    , basic4123 11 C11
    --
    , basic1234 12 C8
    , basic1234 12 C10
    , basic1234 12 C7
    , basic1234 12 C11
    , basic2341 12 C8
    , basic2341 12 C10
    , basic2341 12 C7
    , basic2341 12 C11
    , leap1 12 Three C9 Position2
    , basic3412 12 C7
    , basic3412 12 C11
    , leap1 12 Four C9 Position3
    , basic4123 12 C8
    , basic4123 12 C10
    , leap1 13 One C9 Position1
    , basic1234 13 C8
    , basic1234 13 C10
    , basic2341 13 C7
    , basic2341 13 C11
    , basic3412 13 C6
    , basic3412 13 C12
    , basic4123 13 C5
    , basic4123 13 C13
    --
    , basic1234 14 C8
    , basic1234 14 C10
    , basic2341 14 C7
    , basic2341 14 C11
    , leap1 14 Three C9 Position2
    , basic3412 14 C6
    , basic3412 14 C12
    , leap1 14 Four C9 Position3
    , basic4123 14 C5
    , basic4123 14 C13
    , leap1 15 One C9 Position1
    ]
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic1234 15 C8
            , basic1234 15 C10
            , basic2341 15 C7
            , basic2341 15 C11
            , basic3412 15 C6
            , basic3412 15 C12
            , basic4123 15 C5
            , basic4123 15 C13
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic3412 15 C8
            , basic3412 15 C10
            , basic4123 15 C7
            , basic4123 15 C11
            , basic1234 15 C6
            , basic1234 15 C12
            , basic2341 15 C5
            , basic2341 15 C13
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic1234 16 C8
            , basic1234 16 C10
            , basic2341 16 C7
            , basic2341 16 C11
            , basic3412 16 C6
            , basic3412 16 C12
            , basic4123 16 C5
            , basic4123 16 C13
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic3412 16 C8
            , basic3412 16 C10
            , basic4123 16 C7
            , basic4123 16 C11
            , basic1234 16 C6
            , basic1234 16 C12
            , basic2341 16 C5
            , basic2341 16 C13
            ]
        )
    <>
      --
      join
        [ leap1 16 Three C9 Position2
        , leap1 16 Four C9 Position3
        ]

suspense :: Array Event_
suspense = join
  [ long1 17 One C8 2.0
  , long1 17 Three C10 2.0
  , long1 18 One C8 2.0
  , long1 18 Two C9 2.0
  , long1 18 Two C7 2.0
  , long1 18 Three C10 2.0
  , long1 18 Four C9 2.0
  , long1 18 Four C11 2.0
  , leap 19 One 19 Three C8 Position1
  , leap 19 Two 19 Four C10 Position2
  , leap 19 Three 20 One C9 Position3
  , leap 19 Four 20 Two C11 Position4
  , leap 20 One 20 Three C7 Position1
  , leap 20 Two 20 Four C10 Position2
  , leap 20 Three 21 One C8 Position3
  , leap 20 Four 21 Two C9 Position4
  --
  , leap 21 One 21 Three C10 Position1
  , leap 21 Two 21 Four C8 Position2
  , leap 21 Three 22 One C9 Position3
  , leap 21 Four 22 Two C7 Position4
  , leap 22 One 22 Three C11 Position1
  , leap 22 Two 22 Four C8 Position2
  , leap 22 Three 23 One C10 Position3
  , leap 22 Four 23 Two C9 Position4

  ]

laugh :: Array Event_
laugh = join
  [
  ]

groove :: Array Event_
groove = go <> map (shift'' 8) go <>
  join
    [ leap1 25 One C7 Position1
    , leap1 25 Three C11 Position2
    , leap1 26 One C4 Position3
    , leap1 26 Three C10 Position4
    , leap1 27 One C7 Position1
    , leap1 27 Three C12 Position2
    , leap1 28 One C11 Position3
    , leap1 29 Three C5 Position4
    ]
  where
  go =
    join
      [ basic1234 21 C8
      , basic1234 21 C10
      , basic2341 21 C8
      , basic2341 21 C10
      , basic3412 21 C9
      , basic4123 21 C8
      , basic4123 21 C10
      --
      , basic1234 22 C7
      , basic1234 22 C8
      , basic1234 22 C10
      , basic1234 22 C11
      , basic2341 22 C8
      , basic2341 22 C10
      , basic2341 22 C7
      , basic2341 22 C11
      , basic3412 22 C9
      , basic4123 22 C8
      , basic4123 22 C10
      , basic4123 22 C7
      , basic4123 22 C11
      --
      , basic1234 23 C8
      , basic2341 23 C8
      , basic2341 23 C9
      , basic3412 23 C8
      , basic3412 23 C9
      , basic3412 23 C10
      , basic4123 23 C8
      , basic4123 23 C9
      , basic4123 23 C10
      , basic4123 23 C11
      ] <> squeezeFromHead 0.5
      ( join
          [ basic1234 24 C5
          , basic2341 24 C6
          , basic3412 24 C7
          , basic4123 24 C8
          , basic1234 25 C9
          , basic2341 25 C10
          , basic3412 25 C11
          , basic4123 25 C12
          ]
      )

groove2 :: Array Event_
groove2 = map (shift'' 8) (map (toColumn (C12 <> _)) groove <> map (toColumn (C4 <> _)) groove)

outro :: Array Event_
outro = join
  [ long1 37 One C9 3.0
  , long1 38 One C8 3.0
  , long1 39 One C10 3.0
  , long1 40 One C9 3.0
  , long1 41 One C8 1.5
  , long1 41 Three C10 1.5
  , long1 42 One C9 1.5
  , long1 42 Three C8 1.5
  , long1 41 One C10 1.5
  , long1 41 Three C9 1.5
  , long1 42 One C8 1.5
  , long1 42 Three C10 1.5
  ]

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
  let events' = intro <> intro'beat <> rise <> suspense <> laugh <> groove <> groove2 <> outro
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    { track: TrackV0
        { version: mempty
        , url: "https://cdn.filestackcontent.com/sgDrFGySIgSLSlARecMQ"
        , title: Just "Mystery Mansion Madness - Verison 2"
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