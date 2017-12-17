{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad           (forM_, when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Random
import           Data.Bits               (shiftR)
import           Data.Fixed              (mod')
import           Data.Maybe
import           Data.Semigroup          ((<>))
import           Linear
import           Options.Applicative
import           System.Random

type Point = V2 Double
data Bar = Bar Point Point
data Frame =
  Frame
    { current :: Bar
    , old     :: [Bar]
    }
type Animation = [Frame]

main :: IO ()
main = do
  conf@Config{..} <- execParser opts
  let animation = makeAnimation conf
  gen <- newStdGen
  gif <- makeGif gen conf animation
  case writeGifImages outPath LoopingForever gif of
    Left err -> putStrLn $ "Error: " ++ err
    Right io -> io

makeAnimation ::  Config -> Animation
makeAnimation Config{..} = map (frame . (/ fromIntegral n) . fromIntegral) [0..n-1]
  where
    n = round $ duration * fps
    scale = V2 (fromIntegral width) (fromIntegral height)

    frame :: Double -> Frame
    frame t =
      let
        s = fromIntegral steps
        dt = 1 / s
        old
          = map (bar . flip mod' 1.0)
          . reverse
          . takeWhile (< (1.0 + t))
          . dropWhile (< (1.0 - 7 * dt + t))
          . map (* dt)
          $ [0..2 * s]
      in
        Frame (bar t) old


    bar :: Double -> Bar
    bar t =
      let
        a =
          V2
            (0.5 + 0.4 * cos (4 * pi * t + 2))
            (0.5 + 0.3 * sin (10 * pi * t))
        b =
          V2
            (0.5 + 0.2 * cos (2 * pi * t + 1))
            (0.5 + 0.49 * sin (6 * pi * t))
      in Bar (scale * a) (scale * b)


data Config =
  Config
    { outPath   :: FilePath
    , width     :: Int
    , height    :: Int
    , duration  :: Double
    , fps       :: Double
    , thickness :: Double
    , steps     :: Int
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
         <> value 512)
      <*> option auto
          ( long "height"
         <> metavar "HEIGHT"
         <> showDefault
         <> value 512)
      <*> option auto
          ( long "duration"
         <> short 'd'
         <> metavar "SEC"
         <> showDefault
         <> value 6.0)
      <*> option auto
          ( long "fps"
         <> short 'f'
         <> showDefault
         <> value 30.0)
      <*> option auto
          ( long "thickness"
         <> short 't'
         <> showDefault
         <> value 3.0)
      <*> option auto
          ( long "steps"
         <> short 's'
         <> showDefault
         <> value 100)

makeGif :: StdGen -> Config -> Animation -> IO [(Palette, GifDelay, Image Pixel8)]
makeGif gen Config{..} animation = zip3 palettes (repeat period) <$> images
  where
    (gen1, gen2) = split gen
    palettes = makePalette . mkStdGen <$> randoms gen1
    images = makeImages gen2 width height thickness animation
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
      let
        (h, g' ) = random g
        b = logBase 2 (1 + fromIntegral x) / 8
      in (g', hsb h 1 b)
    )
    gen
    256
    1

makeImages :: StdGen -> Int -> Int -> Double -> Animation -> IO [Image Pixel8]
makeImages gen w h thickness animation =
  flip evalRandT gen $
    forM animation $ \frame -> do
      m <- createMutableImage w h 0
      g <- mkStdGen <$> getRandom
      forM_ (reverse $ zip (current frame : old frame) [0..]) $
        \(Bar a b, i) -> drawLine g thickness i a b m
      freezeImage m

drawLine :: PrimMonad m => StdGen -> Double -> Int -> Point -> Point -> MutableImage (PrimState m) Pixel8 -> m ()
drawLine gen thickness i p1 p2 img = do
  let
    w  = mutableImageWidth img
    h  = mutableImageHeight img
    v  = p2 - p1
    vn = norm v
    n  = normalize (perp v)
    nt = n ^* thickness

    (V2 ax ay, V2 bx by, V2 cx cy, V2 dx dy) =
      (p1 - nt, p1 + nt, p2 - nt, p2 + nt)

    startx = minimum [ax, bx, cx, dx]
    endx   = maximum [ax, bx, cx, dx]
    starty = minimum [ay, by, cy, dy]
    endy   = maximum [ay, by, cy, dy]

  flip evalRandT gen $
    forM_ [ V2 x y | x <- [startx..endx], y <- [starty..endy] ] $ \p@(V2 xf yf) -> do
      let
        -- projection
        vp = p - p1
        t  = (vp `dot` v) / (vn * norm vp)
        dist = vp `dot` n
        x = floor xf
        y = floor yf

      when (0 <= t && t <= 1 && abs dist <= thickness) $
        writePixel img x y . flip shiftR i =<< getRandom


