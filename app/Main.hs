module Main where

import MincHask

main :: IO ()
main = do
  (_,vol) <- openVolume "/home/chammill/Documents/2018-04-16_minc-hask/MincHask/data/mni_icbm152_t1_tal_nlin_sym_09c.mnc" 1
  (_, dimPtr) <- getDimensions vol MiDimclassAny 0 MiDimorderFile 3
  (_, dimSizes) <- getDimensionSizes 3 dimPtr 
  (_, slab) <- getRealHyperslab vol [0,0,0] [1,1,10]
  print slab
