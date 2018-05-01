{-# LANGUAGE OverloadedStrings #-}
module Main where

import MincHask
import Data.Text hiding (reverse)
import Control.Monad.Managed
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Unboxed

file :: Text
file = "/home/chammill/Documents/2018-04-16_minc-hask/MincHask/data/mni_icbm152_t1_tal_nlin_sym_09c.mnc"

main :: IO ()
main = runManaged $ do
  vol <- openVolume file 1
  slab <-
    liftIO $
    getHyperslab vol
    (shapeOfList [0,0,0])
    (shapeOfList (reverse [1,1,10]))
  slabList <-
    liftIO $
    getHyperslabList vol
    [0,0,0] [1,1,10]
    
  liftIO $ print (slab :: Array U DIM3 Double) 
  liftIO $ print slabList

    

