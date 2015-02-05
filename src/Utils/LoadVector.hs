{-# LANGUAGE ScopedTypeVariables #-}

module Utils.LoadVector where

import System.IO.MMap
import Foreign
import qualified Data.Vector.Storable as V


-- | Memory map a file to a vector.
loadVector :: forall a. (V.Storable a)
              => FilePath -> Maybe (Int64, Int) -> IO (V.Vector a)
loadVector name range = do
  (ptr, offset, size) <- mmapFileForeignPtr name ReadOnly range
  return $ V.unsafeFromForeignPtr
    ptr
    offset
    (size `div` sizeOf (undefined :: a))
