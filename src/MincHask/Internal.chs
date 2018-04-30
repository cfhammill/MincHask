#include <MincHask.h>

module MincHask.Internal (cOpenVolume
                         , cGetDimensions
                         , cGetDimensionSizes
                         , cGetRealHyperslab
                         , cFreeDimension
                         , cFreeDimArray
                         , cCloseVolume
                         )
where 

import Foreign.Storable
import Foreign.C.Types 
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Exception.Safe
import Control.Monad.IO.Class

{#import MincHask.Types #}

{#context lib="minc2" #}

data MincHaskException =
  VolumeOpenError |
  DimensionOpenError |
  DimensionReadError |
  HyperslabReadError |
  MismatchedHyperslabReadDimensions
  deriving Show

instance Exception MincHaskException 

listToPtr :: Storable a => [a] -> IO (Ptr a)
listToPtr as = do
  arr <- mallocArray (length as)
  pokeArray arr as
  return arr

--int miopen_volume(const char *filename, int mode, mihandle_t *volume)
cOpenVolume :: (MonadThrow io, MonadIO io) =>
  (String) ->
  (Int) ->
  io (Volume)
cOpenVolume file op = liftIO $ do
  (res, vol) <-
    withCString file $ \file' -> 
    let {op' = fromIntegral op} in 
      alloca $ \v' -> 
      {#call miopen_volume #} file' op' v' >>= \res ->
      let {res' = fromIntegral res} in
        peek  v'>>= \v -> 
        return (res', v)

  if res == -1
    then throw VolumeOpenError
    else return vol


cGetDimensions ::
  (MonadThrow io, MonadIO io) =>
  (Volume) ->
  (DimClass) ->
  (MiDimattr) ->
  (DimOrder) ->
  (Int) ->
  io (DimArray)
cGetDimensions v dc da dor n = 
  let v' = id v
      dc' = (fromIntegral . fromEnum) dc
      da' = fromIntegral da
      dor' = (fromIntegral . fromEnum) dor
      n' = fromIntegral n in
    liftIO $ do
      darr <- mallocArray n
      res <- {#call miget_volume_dimensions#} v' dc' da' dor' n' darr
      if res /= 0
        then throw DimensionOpenError
        else return darr


-- int miget_dimension_sizes(const midimhandle_t dimensions[], misize_t array_length,
-- misize_t sizes[]);
cGetDimensionSizes ::
  (MonadThrow io, MonadIO io) =>
  Int ->
  DimArray ->
  io [Int]
cGetDimensionSizes n da = liftIO $ do
  sarr <- mallocArray n
  res <- {#call miget_dimension_sizes#} da (fromIntegral n) sarr
  sl <- peekArray 3 sarr
  free sarr
  if res == -1
    then throw DimensionReadError
    else return (fmap fromIntegral sl)
    
-- int miget_real_value_hyperslab(mihandle_t volume,
--                                       mitype_t buffer_data_type,
--                                       const misize_t start[],
--                                       const misize_t count[],
-- void *buffer);
cGetRealHyperslab ::
  (MonadThrow io, MonadIO io) =>
  Volume ->
  [Int] -> -- ^ starts
  [Int] -> -- ^ counts
  io [Double]

cGetRealHyperslab v st co =
  if length st /= length co
  then throw MismatchedHyperslabReadDimensions
  else
    liftIO $ do
      starr <- listToPtr (fmap fromIntegral st)
      coarr <- listToPtr (fmap fromIntegral co)
      ty <- return (fromIntegral (fromEnum MiTypeDouble))
      buf <- mallocArray (product co)
      res <- {#call miget_real_value_hyperslab #}
             v ty starr coarr (castPtr buf)
      slab <- peekArray (product co) buf
      free buf
      free starr
      free coarr
      if res == -1
        then throw HyperslabReadError
        else liftIO (return slab)


cFreeDimension ::
  (MonadIO io) =>
  Dimension -> io Int
cFreeDimension d = liftIO $ do
  res <- {#call mifree_dimension_handle #} d
  return (fromIntegral res)

cFreeDimArray ::
  (MonadIO io) => Int -> DimArray -> io [Int]
cFreeDimArray n da = liftIO $ do
  dims <- peekArray n da
  sequence (fmap cFreeDimension dims)

cCloseVolume :: (MonadIO io) => Volume -> io Int
cCloseVolume v = liftIO $ do
  res <- {#call miclose_volume #} v
  return (fromIntegral res)
  
