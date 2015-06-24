module Cgm.Debug.Trace(
  traceF
) where

import Debug.Trace

traceF :: (a -> String) -> a -> a
traceF f a = trace (f a) a