{-# LANGUAGE OverloadedStrings #-}
module Main where

import MincHask
import Data.Text
import Control.Monad.Managed

file :: Text
file = "/home/chammill/Documents/2018-04-16_minc-hask/MincHask/data/mni_icbm152_t1_tal_nlin_sym_09c.mnc"

main :: IO ()
main = runManaged $ do
  vol <- openVolume file 1
  slab <- liftIO $ getHyperslab vol [0,0,0] [1,1,10]
  liftIO $ print slab
  

    

