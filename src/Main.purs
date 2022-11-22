module Main where

import Prelude

-- import Charts.AkiraComplex.NOISZ as NOISZ
-- import Charts.Combatplayer.Speedy as Speedy
-- import Charts.Jun.Imp as Imp
-- import Charts.Testing.Stress as Stress
-- import Charts.Testing.Simple as Simple
import Charts.AkiraComplex.ShowMeHow as ShowMeHow
--import Charts.DJMonad.MysteryMansionMadnessRedo as MysteryMansionMadnessRedo
--import Charts.DJMonad.MysteryMansionMadnessSmpl as MysteryMansionMadnessSmpl
--import Charts.DJMonad.MysteryMansionMadnessLonger as MysteryMansionMadnessLonger
-- import Charts.DJMonad.MysteryMansionMadnessLongerRails as MysteryMansionMadnessLongerRails
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON as JSON

main :: Effect Unit
main = do
  log (JSON.writeJSON ShowMeHow.piece)
