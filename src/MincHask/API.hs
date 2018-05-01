module MincHask.API (withVolume
                    , openVolume
                    , withDimensions
                    , getDimensions
                    , getDimensionSizes
                    , getHyperslabList
                    , getHyperslab
                    )
where

import MincHask.Internal
import MincHask.Types
import Data.Text hiding (length, reverse)
import Control.Monad.Managed
import Control.Exception.Safe
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peekElemOff)
import Data.Vector.Unboxed hiding (product, reverse)
import Data.Array.Repa.Repr.Unboxed (Array, U, fromUnboxed)
import Data.Array.Repa.Shape (Shape, listOfShape)

withVolume :: (MonadIO io, MonadMask io) =>
  Text -> Int -> (Volume -> io a) -> io a
withVolume file n = bracket (cOpenVolume (unpack file) n) cCloseVolume  

openVolume :: Text -> Int -> Managed Volume
openVolume f op = 
  managed (withVolume f op)

withDimensions ::
  (MonadIO io, MonadMask io) =>
  Volume ->
  DimClass ->
  MiDimattr ->
  DimOrder ->
  Int ->
  (DimArray -> io a) ->
  io a
withDimensions v dc da dor n =
  bracket (cGetDimensions v dc da dor n) (cFreeDimArray n)

getDimensions ::
  Volume ->
  DimClass ->
  MiDimattr ->
  DimOrder ->
  Int ->
  Managed DimArray
getDimensions v dc da dor n =
  managed (withDimensions v dc da dor n)

getDimensionSizes :: Int -> DimArray -> IO [Int]
getDimensionSizes n da = 
  cGetDimensionSizes n da
  

getVolumeSizes :: Text -> Int -> IO [Int]
getVolumeSizes file n =
  let dims = do
        vol <- openVolume file n
        getDimensions vol MiDimclassAny 0 MiDimorderFile n
  in
    with dims (getDimensionSizes n)
    
getHyperslabList :: Volume -> [Int] -> [Int] ->  IO [Double]
getHyperslabList v st co = 
  bracket (cGetRealHyperslab v st co) free $ peekArray (product co)

getHyperslab ::
  (Shape s) =>
  Volume -> s -> s -> IO (Array U s Double)
getHyperslab v st co =
  bracket (cGetRealHyperslab v stl col) free $ \buf ->
   do
     vec <- generateM (product col) (peekElemOff buf)
     return (fromUnboxed co vec)
     where
       stl = reverse (listOfShape st)
       col = reverse (listOfShape co)
