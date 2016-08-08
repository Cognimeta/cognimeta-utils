{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeOperators #-}

module Cgm.Data.Either ( 
  (:|),
  mkRight,
  mkLeft,
  boolEither,
  fromRight,
  throwExceptT,
  mapError
  ) where

import Data.Bool
import Cgm.Control.InFunctor
import Control.Monad.Except
import Control.Exception
import Control.Arrow

infixr 2 :|
type a :| b = Either a b

mkRight :: a :>> Either b a
mkRight = uncheckedStrictlyIncreasing Right
mkLeft :: a :>> Either a b
mkLeft = uncheckedStrictlyIncreasing Left

boolEither :: a -> b -> Bool -> Either a b
boolEither a b = bool (Left a) (Right b)

fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight called on Left"
fromRight (Right x) = x

throwExceptT :: (Show e, MonadIO m) => ExceptT e m a -> m a
throwExceptT = (>>= liftIO . evaluate . either (error . show) id) . runExceptT

mapError :: Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapError e = mapExceptT $ liftM $ left e


--filterEither :: (a -> Either b c) -> DList a -> (DList b, DList c)
--filterEither f = foldr (flip add . f) (empty, empty)
--    where add lists = either (\b -> flip fmap1 lists (cons b))
--                             (\c -> flip fmap2 lists (cons c))

