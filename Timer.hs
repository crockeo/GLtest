module Timer where

import Control.Concurrent.MVar

import Data.Time.Clock

-- The last time deltaTime was called
lastTime :: IO (MVar Double)
lastTime = newMVar 0.0

printlasttime :: IO ()
printlasttime = do
  (lastTime >>= isEmptyMVar) >>= (\a -> if a
    then putStrLn "No last time."
    else (lastTime >>= readMVar) >>= print)

-- Getting the current time
time :: IO Double
time = do
  currTime <- (getCurrentTime >>= (return . utctDayTime))
  return (fromRational $ toRational currTime :: Double)

-- Getting the delta time
deltaTime :: IO Double
deltaTime = do
  ltv <- lastTime
  isEmptyMVar ltv >>= (\a -> if a
    then do
      time >>= putMVar ltv
      return 0.0
    else do
      lt <- readMVar ltv
      ct <- time

      putMVar ltv ct

      return (ct - lt))