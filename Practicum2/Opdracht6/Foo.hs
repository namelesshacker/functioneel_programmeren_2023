{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Foo where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Process

theMinute :: String
theMinute = $(runIO (readProcess "date" ["+%M"] "") >>= stringE)