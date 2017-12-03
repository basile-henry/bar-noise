{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Fixed
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Maybe
import           Data.Semigroup          (Max (..), Min (..), (<>))
import           Data.Semigroup.Foldable
import           Linear
import           Options.Applicative
import           System.Environment
import           System.Random

type Point = V2 Double

main :: IO ()
main = do
  Config{..} <- execParser opts
  gen <- newStdGen
  let gif = makeGif gen fps
  case writeGifImages outPath LoopingForever gif of
    Left err -> putStrLn $ "Error: " ++ err
    Right io -> io

data Config =
  Config
    { outPath  :: FilePath
    , width    :: Int
    , height   :: Int
    , duration :: Double
    , fps      :: Double
    }

opts :: ParserInfo Config
opts =
  info (config <**> helper)
      ( fullDesc
     <> progDesc "Make an animated GIF"
     <> header "bar-noise - moving bar, hue noise" )
  where
    config = Config
      <$> option auto
          ( long "output"
         <> short 'o'
         <> metavar "FILENAME"
         <> showDefault
         <> value "out.gif")
      <*> option auto
          ( long "width"
         <> metavar "WIDTH"
         <> showDefault
         <> value 256)
      <*> option auto
          ( long "height"
         <> metavar "HEIGHT"
         <> showDefault
         <> value 256)
      <*> option auto
          ( long "duration"
         <> short 'd'
         <> metavar "SEC"
         <> showDefault
         <> value 1.0)
      <*> option auto
          ( long "fps"
         <> short 'f'
         <> showDefault
         <> value 25.0)

makeGif :: StdGen -> Double -> [(Palette, GifDelay, Image Pixel8)]
makeGif gen fps = zip3 palettes (repeat period) images
  where
    (gen1, gen2) = split gen
    palettes = makePalette . mkStdGen <$> randoms gen1
    images = makeImages gen2
    period = ceiling $ 100 / fps

hsb :: Double -> Double -> Double -> PixelRGB8
hsb h s b =
  case i of
    0 -> rgb b t p
    1 -> rgb q b p
    2 -> rgb p b t
    3 -> rgb p q b
    4 -> rgb t p b
    _ -> rgb b p q
  where
    i = floor (h * 6)
    rgb r g b = PixelRGB8 (c r) (c g) (c b)
    c = floor . (* 255)
    f = h * 6 - fromIntegral i
    p = b * (1 - s)
    q = b * (1 - s * f)
    t = b * (1 - s * (1 - f))

makePalette :: StdGen -> Palette
makePalette gen = convertImage . snd $
  generateFoldImage
    (\g x _ ->
      let (h, g' ) = random g
          (t, g'') = random g'
          b = t * logBase 2 (1 + fromIntegral x) / 8
      in (g'', hsb h 1 b)
    )
    gen
    256
    1

makeImages :: StdGen -> [Image Pixel8]
makeImages gen =
  let
    -- m = createMutableImage 256 256 0
    i =
      snd $ generateFoldImage
        (\g x y ->
          if (64 < x) && (x < 192) && (64 < y) && (y < 192)
          then
            let (a, g') = random g
            in (g', a)
          else
            (g, 0)
        )
        gen
        256
        256
  in replicate 25 i

drawLine :: PrimMonad m => StdGen -> Double -> Point -> Point -> MutableImage (PrimState m) Pixel8 -> m ()
drawLine gen thickness p1 p2 img = do
  let w = mutableImageWidth img
      h = mutableImageHeight img
      v = p2 - p1
      n = thickness *^ normalize (perp v)

      (V2 ax ay, V2 bx by, V2 cx cy, V2 dx dy) =
        (p1 - n, p1 + n, p2 - n, p2 + n)
      (Min x1, Max x2, Min y1, Max y2) = fold1
        [ (Min x, Max x, Min y, Max y)
        | p <- p1 :| [p2]
        , d <- n :| [-n]
        , let V2 x y = p + d
        ]


  return ()
