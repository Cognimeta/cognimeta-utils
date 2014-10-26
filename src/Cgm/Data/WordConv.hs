{-
Copyright 2010-2013 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE MagicHash, TypeFamilies, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Cgm.Data.WordConv (
  WordConv(..)
  ) where

import Prelude()
import Cgm.Prelude
import Data.Word
import Control.Category
import Cgm.Control.InFunctor

class WordConv a where
  wordConv  :: Bijection' Word a
-- Instances defined in WordInstance.hs within architecture specific source folders
