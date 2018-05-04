{-# LANGUAGE FlexibleContexts          #-}

module MincHask.API (withVolume
                    , openVolume
                    , getVolumeSizes                 
                    , getDimensions
                    , getDimensionSizes
                    , getHyperslabList
                    , getHyperslabRepa
                    , getHyperslabAccelerate
                    , readVolume
                    )
where

import Prelude as P
import MincHask.Internal
import MincHask.Types
import Data.Text hiding (length, reverse)
import Control.Monad.Managed
import Control.Exception.Safe
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peekElemOff)
import Data.Vector.Unboxed hiding (product, reverse)
import qualified Data.Array.Repa.Repr.Unboxed as R (Array, U, fromUnboxed)
import qualified Data.Array.Repa.Eval as R (Load)
import qualified Data.Array.Repa.Repr.Delayed as R (D, delay)
import Data.Array.Repa.Shape (Shape, listOfShape, shapeOfList)

import qualified Data.Array.Accelerate as A hiding (reverse, replicate)
import Data.Array.Accelerate.IO


withVolume :: (MonadIO io, MonadMask io) =>
  Text -> Int -> (Volume -> io a) -> io a
withVolume file n = bracket (cOpenVolume (unpack file) n) cCloseVolume  

openVolume :: Text -> Int -> Managed Volume
openVolume f op = 
  managed (withVolume f op)

getDimensions ::
  (MonadThrow io, MonadIO io) =>
  Volume ->
  DimClass ->
  MiDimattr ->
  DimOrder ->
  Int ->
  io DimArray
getDimensions = cGetDimensions

getDimensionSizes :: Int -> DimArray -> IO [Int]
getDimensionSizes n da = 
  cGetDimensionSizes n da
  

getVolumeSizes :: Text -> Int -> IO [Int]
getVolumeSizes file n = 
  with (openVolume file 1) $ \vol ->
  do
    dims <- liftIO $ getDimensions vol MiDimclassAny 0 MiDimorderFile n
    liftIO $ getDimensionSizes n dims
    
getHyperslabList :: Volume -> [Int] -> [Int] ->  IO [Double]
getHyperslabList v st co = 
  bracket (cGetRealHyperslab v st co) free $ peekArray (product co)

getHyperslabRepa ::
  (Shape s) =>
  Volume -> s -> s -> IO (R.Array R.U s Double)
getHyperslabRepa v st co =
  bracket (cGetRealHyperslab v stl col) free $ \buf ->
   do
     vec <- generateM (product col) (peekElemOff buf)
     return (R.fromUnboxed co vec)
     where
       stl = reverse (listOfShape st)
       col = reverse (listOfShape co)


getHyperslabAccelerate ::
  (Shapes r a, Shape r) =>
  Volume ->
  r ->
  r ->
  IO (A.Array a Double)
getHyperslabAccelerate v st co =
  fromRepa <$>
  (computeAccP =<<
  (fmap R.delay $ getHyperslabRepa v st co))


readVolume ::
  (Shapes r sh) =>
  Text ->
  Int ->
  IO (A.Array sh Double)
readVolume file n = do
  sizes <- getVolumeSizes file n
  with (openVolume file 1) $
    \vol ->
      getHyperslabAccelerate
      vol
      (shapeOfList (P.replicate n 0))
      (shapeOfList (P.reverse sizes))
