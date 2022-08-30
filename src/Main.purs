module Main where

import Prelude

import Charts.AkiraComplex.NOISZ as NOISZ
import Charts.Testing.Stress as Stress
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON as JSON

main :: Effect Unit
main = do
  Stress.piece >>= log <<< JSON.writeJSON 
