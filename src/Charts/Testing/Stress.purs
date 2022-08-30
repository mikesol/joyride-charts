module Charts.Testing.Stress where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (cons, sortBy, (..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V, invalid, validation)
import Effect.Exception (Error, error)
import Foreign (ForeignError(..), MultipleErrors)
import Joyride.Types (Column(..), EventV0(..), Event_(..), Position(..), Track(..))

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

everything :: Array Event_
everything = join (full <$> (1 .. 128))
  where
  full n = (foldl (\b _ -> { i: b.i <> C1, a: basic1313 n b.i <> b.a }) { i: C1, a: [] } (0 .. 15)).a

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
  let events' = everything
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    { track: TrackV0
        { version: mempty
        , url: "https://cdn.filestackcontent.com/kG1ZasfRPSvsRd2QAMux"
        , title: Just "NOISZ"
        , private: true
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