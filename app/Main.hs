{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Prelude as P
import MincHask
import Data.Text (Text)
import Control.Monad.Managed
import qualified Data.Array.Repa.Shape as S
--import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Unboxed hiding (Array)
import Data.Array.Accelerate hiding (reverse, replicate)
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Array.Accelerate.LLVM.Native as CPU
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.Data.Fold
import Data.Array.Accelerate.Data.Monoid

file :: Text
file = "/home/chammill/Documents/2018-04-16_minc-hask/MincHask/data/mni_icbm152_t1_tal_nlin_sym_09c.mnc"

type Matrix = Array DIM2

runExp :: Elt e => Exp e -> e
runExp e = indexArray (CPU.run (unit e)) Z

colourize :: Acc (Matrix Double) -> Acc (Matrix Word32)
colourize a =
  let a' = map toFloating a in
    let mx = the (fold max 0 (fold (max) 0 a')) in
      map
      (\x -> packRGB $ rgb
        (toFloating x / mx)
        (toFloating x / mx)
        (toFloating x / mx))
      a'                 

main :: P.IO ()
main = do
  dims <- getVolumeSizes file 3
  len <- P.return $ P.product dims
  lend <- P.return $ P.fromIntegral len
  acc <- readVolume file 3 :: P.IO (Array (Z :. Int :. Int :. Int) Double)
  accArr <- P.return $ reshape (constant (Z :. len)) (use acc) 

  P.print $ CPU.run $ fold (\acc x -> acc + (x ** 2)) 0 accArr
  P.print $ CPU.run $ map (/lend) $ fold (+) 0 accArr


  -- acc <- readVolume file 3

  -- slice_acc <-
  --   P.return
  --   (colourize
  --    (slice (use acc)
  --     (constant (Z :. (70 :: Int) :. All :. All))))

  -- liftIO $ writeImageToBMP "slice.bmp" (GPU.run slice_acc)

  -- liftIO $ P.print $ runExp (shape slice_acc)

