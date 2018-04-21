#include <MincHask.h>

module MincHask (module MincHask.Types
                , openVolume
                , getDimensions
                , getDimensionSizes
                , getRealHyperslab
                , freeDimension
                , freeDimArray
                )
where 

import Foreign.Storable
import Foreign.C.Types 
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

{#import MincHask.Types #}

{#context lib="minc2" #}

listToPtr :: Storable a => [a] -> IO (Ptr a)
listToPtr as = do
  arr <- mallocArray (length as)
  pokeArray arr as
  return arr

--int miopen_volume(const char *filename, int mode, mihandle_t *volume);
{#fun unsafe miopen_volume as openVolume
 {`String'
 , `Int'
 ,  alloca- `Volume' peek* } ->
  `Int' #}


getDimensions ::
  (Volume) ->
  (DimClass) ->
  (MiDimattr) ->
  (DimOrder) ->
  (Int) ->
  IO ((Int), (DimArray))
getDimensions v dc da dor n =
  let v' = id v
      dc' = (fromIntegral . fromEnum) dc
      da' = fromIntegral da
      dor' = (fromIntegral . fromEnum) dor
      n' = fromIntegral n in
    do
      darr <- mallocArray n
      res <- {#call miget_volume_dimensions#} v' dc' da' dor' n' darr
      return (fromIntegral res, darr)


-- int miget_dimension_sizes(const midimhandle_t dimensions[], misize_t array_length,
-- misize_t sizes[]);
getDimensionSizes ::
  Int ->
  DimArray ->
  IO (Int, [Int])
getDimensionSizes n da = do
  sarr <- mallocArray n
  res <- {#call miget_dimension_sizes#} da (fromIntegral n) sarr
  sl <- peekArray 3 sarr
  free sarr
  return (fromIntegral res, fmap fromIntegral sl)
    
-- int miget_real_value_hyperslab(mihandle_t volume,
--                                       mitype_t buffer_data_type,
--                                       const misize_t start[],
--                                       const misize_t count[],
-- void *buffer);
getRealHyperslab ::
  Volume ->
  [Int] -> -- ^ starts
  [Int] -> -- ^ counts
  IO (Int, [Double])

getRealHyperslab v st co =
  if length st /= length co then
    return (-1, [])
  else
    do
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
      return (fromIntegral res, slab)      

{#fun unsafe mifree_dimension_handle as freeDimension
 {id `Dimension'} -> `Int'
 #}

freeDimArray :: Int -> DimArray -> IO ([Int])
freeDimArray n da = do
  dims <- peekArray n da
  sequence (fmap freeDimension dims)

{#fun unsafe miclose_volume as closeVolume
 {`Volume'} -> `Int'
 #}
