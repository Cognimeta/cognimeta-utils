{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, MagicHash, UnboxedTuples #-}

module Cgm.System.Endian (
  Endian(..),
  swapHalves,
  ixBytes,
  littleEndianIxBytes,
  bigEndianIxBytes,
  platformEndianness,
  platformWordEndianness,
  ByteSwapped,
  swapBytes,
  unswapBytes,
  Endianness(..),
  reverseEndianness
  ) where

import Data.Bits
import Data.Ix
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Cgm.Data.Tagged
import Cgm.Data.Word
import Cgm.Data.Array
import Cgm.Data.Len
import Cgm.Data.Structured
import Cgm.Control.InFunctor
import Cgm.Control.Combinators

class (Prim w, Bits w, Num w) => Endian w where 
  untypedSwapBytes :: w -> w

instance Endian Word8 where 
  {-# INLINE untypedSwapBytes #-} 
  untypedSwapBytes = id
instance Endian Word16 where 
  {-# INLINE untypedSwapBytes #-} 
  untypedSwapBytes = swapHalves
instance Endian Word32 where 
  {-# INLINE untypedSwapBytes #-} 
  untypedSwapBytes = swapMask 0xff00ff 8 . swapHalves
instance Endian Word64 where 
  {-# INLINE untypedSwapBytes #-} 
  untypedSwapBytes = swapMask 0xff00ff00ff00ff 8 . swapMask 0xffff0000ffff 16 . swapHalves
instance Endian Word where
  {-# INLINE untypedSwapBytes #-} 
  untypedSwapBytes = onWordConvB (flip liftItI untypedSwapBytes) (flip liftItI untypedSwapBytes)
  

{-# INLINE swapHalves #-} 
swapHalves :: forall w. FiniteBits w => w -> w
swapHalves w = w `shiftR` halfBitSize .|. w `shiftL` halfBitSize where
  halfBitSize = finiteBitSize (undefined :: w) `div` 2

-- | Exchange bits in mask with bits at some specified relative position (the last bits must not be in the mask)
{-# INLINE swapMask #-} 
swapMask :: Bits w => w -> Int -> w -> w
swapMask mask shift w = ((w `shiftR` shift) .&. mask) .|. ((w .&. mask) `shiftL` shift)

-- | Number with byte value n in the nth byte, in order of increasing significance (starting at 0)
byteNumbers :: forall w. Endian w => w
byteNumbers = foldr (.|.) 0 $ fmap (\n -> fromIntegral n `shiftL` (8 * fromIntegral n)) $ (at :: At w) increasingBytes

increasingBytes :: forall w. Prim w => Tagged w [Word8]
increasingBytes = Tagged $ range (0, fromIntegral (primSizeOf (undefined :: w)) - 1)

singletonArray :: Prim w => w -> PrimArray Free w
singletonArray w = runSTPrimArray (mkArray 1 >>= \a -> a <$ writeArray a 0 w)

-- Indexes each byte of a word
ixBytes :: forall w. Endian w => w -> [Word8]
ixBytes w = let a = unsafePrimArrayCast (singletonArray w) :: PrimArray Free Word8 in indexArray a . unsafeLen . fromIntegral <$> (at :: At w) increasingBytes

littleEndianIxBytes :: forall w. Prim w => Tagged w [Word8]
littleEndianIxBytes = increasingBytes
bigEndianIxBytes :: forall w. Prim w => Tagged w [Word8]
bigEndianIxBytes = reverse <$> increasingBytes

data Endianness = LittleEndian | BigEndian deriving (Eq, Show)

-- | Undefined when the platform is neither big nor little endian.
{-# SPECIALIZE platformEndianness :: Tagged Word32 Endianness #-}
{-# SPECIALIZE platformEndianness :: Tagged Word64 Endianness #-}
{-# SPECIALIZE platformEndianness :: Tagged Word Endianness #-}
platformEndianness :: forall w. Endian w => Tagged w Endianness
platformEndianness = do
  let plat = ixBytes (byteNumbers :: w)
  l <- littleEndianIxBytes
  b <- bigEndianIxBytes
  if plat == l then return LittleEndian else if plat == b then return BigEndian else undefined

platformWordEndianness :: Endianness
platformWordEndianness = (at :: At Word) platformEndianness
reverseEndianness LittleEndian = BigEndian
reverseEndianness BigEndian = LittleEndian

newtype ByteSwapped w = ByteSwapped w deriving (Show)
swapBytes :: Endian w => w -> ByteSwapped w
swapBytes = ByteSwapped . untypedSwapBytes
unswapBytes :: Endian w => ByteSwapped w -> w
unswapBytes (ByteSwapped w) = untypedSwapBytes w

-- Deriving Prim fails due to unpacked tuple in readByteArray# so...
instance Prim a => Prim (ByteSwapped a) where
  sizeOf# (ByteSwapped a) = sizeOf# a
  alignment# (ByteSwapped a) = alignment# a
  indexByteArray# arr# i# = ByteSwapped (indexByteArray# arr# i#)
  readByteArray# arr# i# s# = case readByteArray# arr# i# s# of
    (# s1#, x# #) -> (# s1#, ByteSwapped x# #)
  writeByteArray# arr# i# (ByteSwapped a) s# = writeByteArray# arr# i# a s#
  setByteArray# arr# i# j# (ByteSwapped a) s# = setByteArray# arr# i# j# a s#
  indexOffAddr# arr# i# = ByteSwapped (indexOffAddr# arr# i#)
  readOffAddr# arr# i# s# = case readOffAddr# arr# i# s# of
    (# s1#, x# #) -> (# s1#, ByteSwapped x# #)
  writeOffAddr# arr# i# (ByteSwapped a) s# = writeOffAddr# arr# i# a s#
  setOffAddr# arr# i# j# (ByteSwapped a) s# = setOffAddr# arr# i# j# a s#

deriveStructured ''Endianness