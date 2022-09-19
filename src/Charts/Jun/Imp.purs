module Charts.Jun.Imp where

-- | # Lonely little imp
-- |
-- | By Jun Koruda
-- |
-- | | 1 - 18  | 18 - 33  | 33 - 39 | 39 - 42 | 42 - 50 | 50 - 58 | 58 - 74 | 74 - 90 |
-- | | Intro   | Waltz    | Silly   | Docile  | Heroic  | March   | Bold    | Outro   |

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
tempo = 174

mbToTime :: Int -> OneTwoThreeFour -> Number
mbToTime m b = mbsToTime m b 0.0

mbsToTime :: Int -> OneTwoThreeFour -> Number -> Number
mbsToTime m b s = (toNumber ((integrate (m - 1) + (asInt b - 1))) + s) * 60.0 / (toNumber tempo)

integrate :: Int -> Int
integrate m
  | m < 18 = m * 4
  | m < 33 = (m - 17) * 3 + integrate 17
  | m < 74 = (m - 32) * 4 + integrate 32
  | otherwise = (m - 73) * 3 + integrate 73

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

intro :: Array Event_
intro = join
  [ basic1234 2 C8
  , basic1234 3 C10
  , basic1234 4 C7
  , basic1234 5 C9
  , basic1234 6 C11
  , leap 6 Three 7 One C9 Position1
  , basic1234 7 C10
  , leap 7 Three 8 One C11 Position1
  , basic1234 8 C8
  , leap 8 Three 9 One C10 Position1
  , basic1234 9 C9
  , leap 9 Three 10 One C8 Position1
  , basic1234 10 C8
  , basic3412 10 C10
  , basic1234 11 C10
  , basic3412 11 C7
  , basic1234 12 C7
  , basic3412 12 C9
  , basic1234 13 C9
  , basic3412 13 C11
  , basic1234 14 C11
  , leap 14 Three 15 One C9 Position1
  , basic3412 14 C10
  , basic1234 15 C10
  , leap 15 Three 16 One C11 Position1
  , basic3412 15 C8
  , basic1234 16 C8
  , leap 16 Three 17 One C10 Position1
  , basic3412 16 C9
  , basic1234 17 C9
  , leap 17 Three 18 One C8 Position1
  ]

waltz :: Array Event_
waltz = join
  [ long 18 One 20 One C7 2.0
  , long 19 One 21 One C11 1.875
  , long 20 One 22 One C8 1.75
  , long 21 One 23 One C10 1.625
  , long 22 One 24 One C9 1.5
  , long 23 One 25 One C8 1.375
  , long 24 One 26 One C10 1.25
  , long 25 One 27 One C7 1.125
  , long 26 One 28 One C11 1.0
  , leap 26 Three 27 Three C9 Position2
  , long 27 One 29 One C8 0.875
  , long 28 One 30 One C10 0.75
  , leap 28 Three 29 Three C7 Position1
  , long 29 One 31 One C9 0.5
  , long 30 One 32 One C8 0.375
  , leap 30 Three 31 Three C9 Position2
  , long 31 One 33 One C10 0.25
  , leap 31 Three 32 Three C11 Position1
  , long 32 One 34 One C7 0.125
  , leap 32 Three 33 Three C8 Position2
  ]

squeezeFromHead :: Number -> Array Event_ -> Array Event_
squeezeFromHead n l = map (squeeze n stt) l
  where
  stt = earliest l

silly :: Array Event_
silly = squeezeFromHead 0.5 $ join
  [ basic1234 34 C7
  , basic2341 34 C8
  , basic3412 34 C9
  , basic4123 34 C10
  , basic1234 35 C7
  , basic2341 35 C8
  , basic3412 35 C9
  , basic4123 35 C10
  , basic1234 36 C5
  , basic2341 36 C7
  , basic3412 36 C9
  , basic4123 36 C11
  , basic1234 37 C6
  , basic2341 37 C8
  , basic3412 37 C10
  , basic4123 37 C12
  , basic1234 38 C7
  , basic1234 38 C8
  , basic1234 38 C9
  , basic1234 38 C10
  , basic1234 38 C11
  , basic1234 39 C7
  , basic1234 39 C8
  , basic1234 39 C9
  , basic1234 39 C10
  , basic1234 39 C11
  , basic1234 40 C5
  , basic1234 40 C7
  , basic1234 40 C9
  , basic1234 40 C11
  , basic1234 40 C13
  , basic1234 41 C5
  , basic1234 41 C7
  , basic1234 41 C9
  , basic1234 41 C11
  , basic1234 41 C13
  , leap 42 One 43 One C9 Position1
  , leap 42 Two 43 Two C8 Position2
  , leap 42 Three 43 Three C10 Position3
  , leap 42 Four 43 Four C11 Position4
  , leap 43 One 44 One C9 Position1
  , leap 43 Two 44 Two C10 Position2
  , leap 43 Three 44 Three C8 Position3
  , leap 43 Four 44 Four C7 Position4
  ]

docile :: Array Event_
docile = join
  [ long 39 One 40 One C8 3.0
  , long 40 One 41 One C10 3.0
  , long 41 Four 42 One C7 1.0
  , long 41 Four 42 One C9 1.0
  , long 41 Four 42 One C11 1.0
  ]

heroic :: Array Event_
heroic = join
  [ basic1234 42 C8
  , basic1234 42 C9
  --
  , basic1234 43 C9
  , basic1234 43 C10
  --
  , basic1234 44 C7
  , basic1234 44 C8
  , basic1234 44 C9
  --
  , basic1234 45 C9
  , basic1234 45 C10
  , basic1234 45 C11
  ----------
  , basic1234 46 C8
  , basic1234 46 C9
  --
  , basic3412 46 C9
  , basic3412 46 C10
  --
  , basic1234 47 C7
  , basic1234 47 C8
  , basic1234 47 C9
  --
  , basic3412 47 C9
  , basic3412 47 C10
  , basic3412 47 C11
  ----
  , basic1234 48 C7
  , leap 48 One 49 One C6 Position1
  , basic1234 48 C9
  , leap 48 Three 49 Three C8 Position3
  --
  , basic3412 48 C9
  , basic3412 48 C11
  --
  , basic1234 49 C5
  , leap 49 One 50 One C10 Position2
  , basic1234 49 C7
  , basic1234 49 C9
  , leap 49 Three 50 Three C8 Position1
  --
  , basic3412 49 C9
  , basic3412 49 C11
  , basic3412 49 C13
  ]

march :: Array Event_
march = join
  [ basic1234 50 C6
  , basic2341 50 C7
  , basic3412 50 C6
  , basic4123 50 C7
  , basic1234 51 C10
  , basic2341 51 C9
  , basic3412 51 C10
  , basic4123 51 C9
  --
  , basic1234 52 C6
  , basic1234 52 C7
  , basic2341 52 C8
  , basic2341 52 C9
  , basic3412 52 C6
  , basic3412 52 C7
  , basic4123 52 C8
  , basic4123 52 C9
  --
  , basic1234 53 C10
  , basic1234 53 C9
  , basic2341 53 C8
  , basic2341 53 C7
  , basic3412 53 C10
  , basic3412 53 C9
  , basic4123 53 C8
  , basic4123 53 C7
  --
  , basic1234 54 C6
  , basic1234 54 C7
  , basic2341 54 C8
  , basic2341 54 C9
  , basic3412 54 C10
  , basic3412 54 C11
  , basic4123 54 C12
  , basic4123 54 C13
  --
  , basic1234 55 C6
  , basic1234 55 C7
  , basic2341 55 C8
  , basic2341 55 C9
  , basic3412 55 C10
  , basic3412 55 C11
  , basic4123 55 C12
  , basic4123 55 C13
  --
  , basic1234 56 C2
  , basic1234 56 C4
  , leap 56 Two 57 Two C3 Position1
  , basic2341 56 C6
  , basic2341 56 C8
  , basic3412 56 C10
  , basic3412 56 C12
  , basic4123 56 C14
  , basic4123 56 C16
  --
  , basic1234 57 C2
  , basic1234 57 C4
  , leap 57 Two 58 Two C3 Position1
  , basic2341 57 C6
  , basic2341 57 C8
  , basic3412 57 C10
  , basic3412 57 C12
  , basic4123 57 C14
  , basic4123 57 C16
  ]

bold :: Array Event_
bold =
  join
    [ basic1234 58 C14
    , basic2341 58 C10
    , basic3412 58 C6
    , basic4123 58 C2
    , basic1234 59 C13
    , basic2341 59 C13
    , basic3412 59 C13
    , basic4123 59 C13
    , basic1234 60 C2
    , basic2341 60 C6
    , basic3412 60 C10
    , basic4123 60 C14
    , basic1234 61 C3
    , basic2341 61 C3
    , basic3412 61 C3
    , basic4123 61 C3
    --   
    , basic1234 62 C9
    , basic2341 62 C8
    , basic2341 62 C10
    , basic3412 62 C7
    , basic3412 62 C11
    , basic4123 62 C6
    , basic4123 62 C12
    , basic1234 63 C5
    , basic1234 63 C13
    , basic2341 63 C4
    , basic2341 63 C14
    , basic3412 63 C3
    , basic3412 63 C15
    , basic4123 63 C2
    , basic4123 63 C16
    --
    , basic1234 64 C9
    , basic2341 64 C8
    , basic2341 64 C10
    , basic3412 64 C7
    , basic3412 64 C11
    , basic4123 64 C6
    , basic4123 64 C12
    , basic1234 65 C5
    , basic1234 65 C13
    , basic2341 65 C4
    , basic2341 65 C14
    , basic3412 65 C3
    , basic3412 65 C15
    , basic4123 65 C2
    , basic4123 65 C16
    -- 
    , basic1234 66 C6
    , basic1234 66 C7
    , basic1234 66 C8
    , basic3412 66 C10
    , basic3412 66 C11
    , basic3412 66 C12
    --
    , basic1234 67 C6
    , basic1234 67 C7
    , basic1234 67 C8
    , basic3412 67 C10
    , basic3412 67 C11
    , basic3412 67 C12
    --
    , basic1313 68 C5
    , basic1313 68 C7
    , basic1313 68 C9
    , basic1313 68 C11
    , basic1313 68 C13
    , basic1313 68 C15
    --
    , leap 69 One 69 Three C6 Position1
    , leap 69 Two 69 Four C10 Position3
    , leap 69 Three 70 One C12 Position2
    , leap 69 Four 70 Two C4 Position4
    --
    , basic1234 70 C8
    , basic1234 70 C10
    , basic2341 70 C8
    , basic2341 70 C10
    , basic3412 70 C9
    , basic4123 70 C8
    , basic4123 70 C10
    --
    , basic1234 71 C6
    , basic1234 71 C8
    , basic1234 71 C10
    , basic2341 71 C6
    , basic2341 71 C8
    , basic2341 71 C10
    , basic3412 71 C9
    , basic4123 71 C6
    , basic4123 71 C8
    , basic4123 71 C10
    ] <>
    ( do
        let
          l = squeezeFromHead 0.5 $
            join
              [ basic1234 72 C3
              , basic2341 72 C4
              , basic3412 72 C5
              , basic4123 72 C6
              , basic1234 73 C7
              , basic2341 73 C8
              , basic3412 73 C9
              , basic4123 73 C10
              ]
        l <> map (toColumn (_ <> C3) <<< shift'' 1) l
    )

outro :: Array Event_
outro = join
  [ long1 74 One C7 2.0
  , long1 75 One C11 1.875
  , long1 76 One C8 1.75
  , long1 77 One C10 1.625
  , long1 78 One C9 1.5
  , long1 79 One C8 1.375
  , long1 80 One C10 1.25
  , long1 81 One C7 1.125
  , long1 82 One C11 1.0
  , leap1 82 Three C9 Position2
  , long1 83 One C8 0.875
  , long1 84 One C10 0.75
  , leap1 84 Three C7 Position1
  , long1 85 One C9 0.5
  , long1 86 One C8 0.375
  , leap1 86 Three C9 Position2
  , long1 87 One C10 0.25
  , leap1 87 Three C11 Position1
  , long1 88 One C7 0.125
  , leap1 88 Three C8 Position2
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
  let events' = intro <> waltz <> silly <> docile <> heroic <> march <> bold <> outro
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    { track: TrackV0
        { version: mempty
        , url: "https://cdn.filestackcontent.com/0YLgl662SSq7ZGLiQLq4"
        , title: Just "lonely little imp - draft 7"
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