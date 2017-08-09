module Main where

import Control.Monad.Eff

import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt, pi)
import Prelude (Unit, (*), (+), discard)

type Width  = Number
type Height = Number
type Radius = Number
type Area   = Number

diagonal :: Width -> Height -> Area
diagonal w h = sqrt (w * w + h * h)

circleArea :: Radius -> Area
circleArea r = pi * r * r


main :: forall e. Eff (console :: CONSOLE| e) Unit
main = do
    logShow (diagonal 3.0 4.0)
    logShow (circleArea 1.0)
    
