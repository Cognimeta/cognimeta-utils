{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}

module Cgm.Data.MapMultiset(
  MapMultiset(mapMultisetMap)
  ) where

import Prelude()
import Cgm.Prelude
import Cgm.Data.Maybe
import Cgm.Data.Multiset
import Cgm.Data.Typeable

import Data.Map(Map)
import qualified Data.Map as Map

newtype MapMultiset a = MapMultiset {mapMultisetMap :: Map a Integer} deriving Typeable

instance Multiset MapMultiset where
  emptySet = MapMultiset Map.empty
  insert e (MapMultiset a) = MapMultiset $ Map.insertWith' (+) e 1 a
  delete e (MapMultiset a) = fmap (\c -> MapMultiset $ if c == 1 then Map.delete e a else Map.insert e (c - 1) a) $ Map.lookup e a

instance Ord a => Monoid (MapMultiset a) where
  mempty = MapMultiset Map.empty
  mappend (MapMultiset a) (MapMultiset b) = MapMultiset $ Map.unionWith (+) a b
