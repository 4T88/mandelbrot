-- ascii art mandelbrot set generator
-- because math is pretty when you squint at it
module Main where

import Data.Complex

-- check if a point escapes the mandelbrot set
mandelbrot :: Complex Double -> Int -> Int
mandelbrot c maxIter = go c 0
  where
    go z iter
      | iter >= maxIter = maxIter
      | magnitude z > 2 = iter
      | otherwise = go (z*z + c) (iter + 1)

-- convert iteration count to ascii character
iterToChar :: Int -> Int -> Char
iterToChar iter maxIter
  | iter >= maxIter = '@'
  | iter > maxIter * 3 `div` 4 = '#'
  | iter > maxIter `div` 2 = '*'
  | iter > maxIter `div` 4 = '+'
  | iter > maxIter `div` 8 = '.'
  | otherwise = ' '

-- generate one row of the mandelbrot set
mandelbrotRow :: Double -> Double -> Double -> Int -> Int -> String
mandelbrotRow y xMin xMax width maxIter =
  [ iterToChar (mandelbrot (x :+ y) maxIter) maxIter
  | i <- [0..width-1]
  , let x = xMin + (xMax - xMin) * fromIntegral i / fromIntegral width
  ]

-- generate the full mandelbrot set
mandelbrotSet :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> [String]
mandelbrotSet xMin xMax yMin yMax width height maxIter =
  [ mandelbrotRow y xMin xMax width maxIter
  | j <- [0..height-1]
  , let y = yMax - (yMax - yMin) * fromIntegral j / fromIntegral height
  ]

-- some fun color patterns using unicode
colorfulMandelbrot :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> [String]
colorfulMandelbrot xMin xMax yMin yMax width height maxIter =
  [ [ iterToColorChar (mandelbrot (x :+ y) maxIter) maxIter
    | i <- [0..width-1]
    , let x = xMin + (xMax - xMin) * fromIntegral i / fromIntegral width
    ]
  | j <- [0..height-1]
  , let y = yMax - (yMax - yMin) * fromIntegral j / fromIntegral height
  ]
  where
    iterToColorChar iter maxIterLocal
      | iter >= maxIterLocal = '█'
      | iter > maxIterLocal * 7 `div` 8 = '▓'
      | iter > maxIterLocal * 3 `div` 4 = '▒'
      | iter > maxIterLocal `div` 2 = '░'
      | iter > maxIterLocal `div` 4 = '·'
      | otherwise = ' '

-- animate by zooming into an interesting point
zoomAnimation :: Int -> IO ()
zoomAnimation frames = do
  let centerX = -0.7269
      centerY = 0.1889
  sequence_ [ do
    let zoom = 0.02 * (0.8 ** fromIntegral frame)
        xMin = centerX - zoom
        xMax = centerX + zoom  
        yMin = centerY - zoom * 0.6
        yMax = centerY + zoom * 0.6
        art = colorfulMandelbrot xMin xMax yMin yMax 80 40 50
    putStr "\ESC[2J\ESC[H"  -- clear screen
    mapM_ putStrLn art
    putStrLn $ "zoom level: " ++ show frame ++ "/" ++ show frames
    putStrLn "press ctrl+c to stop the trip..."
    -- slow it down so you can actually see it
    sequence_ $ replicate 100000 (return ())
  | frame <- [1..frames] ]

main :: IO ()
main = do
  putStrLn "mandelbrot set ascii art generator"
  putStrLn "=================================="
  putStrLn ""
  putStrLn "static version:"
  
  -- classic mandelbrot view
  let art = mandelbrotSet (-2.5) 1.0 (-1.0) 1.0 80 30 30
  mapM_ putStrLn art
  
  putStrLn ""
  putStrLn "colorful version:"
  let colorArt = colorfulMandelbrot (-2.5) 1.0 (-1.0) 1.0 60 25 40
  mapM_ putStrLn colorArt
  
  putStrLn ""
  putStrLn "want to see it zoom? (y/n)"
  response <- getLine
  if response == "y" || response == "yes"
    then zoomAnimation 50
    else putStrLn "fair enough, math can be overwhelming"
