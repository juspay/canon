{-# LANGUAGE DeriveAnyClass      #-}
module Load (main)where

import Prelude
import Network.HTTP.Client
-- import Data.Text
import Data.ByteString.Lazy hiding (filter,elem,concat,length)
import Control.Concurrent.Async
import GHC.Generics
import Data.Aeson
import Network.HTTP.Types
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Fixed
import GHC.Float (int2Double)
main :: IO ()
main = do 
  res <- fn
  print res


unFixed :: Fixed a -> Integer
unFixed (MkFixed i) = i

type WithLatency a = (a,POSIXTime)

type ResponseAndLatency = WithLatency (Response ByteString)

data LoadReport = 
  LoadReport
    { totalRequest :: Int
    , successResponse :: Int
    , failureResponse :: Int
    , rps :: Double
    , avgLatency :: Double
    }
    deriving (Show , Generic , ToJSON)

baseURL :: String
baseURL = "http://localhost:8012/"


fn :: IO String
fn = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ baseURL <> ("offers/health")
  request1 <- parseRequest $ baseURL <> ("offers/heal")
  let req1 = requestMultiplier 1000 request []
  let req2 = requestMultiplier 1000 request1 []

  response <- withLatency $ runRequestParallely manager [req1,req2]
  return $ show $ generateReport (fst response) (snd response)


generateReport :: [[ResponseAndLatency]] -> POSIXTime -> LoadReport
generateReport responses totalTime =
  LoadReport{..}
  where
    successResponses = filter ((200==) . statusCode . responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    rps = (int2Double totalRequest)/(int2Double $ fromDiffTimeToSeconds totalTime)
    avgLatency = 
      let total = sum $ (fromDiffTimeToSeconds . snd) <$> successResponses
      in (int2Double total) / (int2Double $ fromDiffTimeToSeconds totalTime)


fromDiffTimeToSeconds :: POSIXTime -> Int
fromDiffTimeToSeconds = fromInteger . unFixed . nominalDiffTimeToSeconds

requestMultiplier :: Int -> Request -> [Request] -> [Request]
requestMultiplier 0 _ accVal = accVal
requestMultiplier mul baseReq accVal = requestMultiplier (mul-1) baseReq (baseReq : accVal)

runRequestSeqentially :: Manager -> [Request] -> IO [ResponseAndLatency]
runRequestSeqentially manager reqs = sequence $ fmap (runRequest manager) reqs

runRequestParallely :: Manager -> [[Request]] -> IO [[ResponseAndLatency]]
runRequestParallely manager reqList = mapConcurrently (runRequestSeqentially manager) reqList

runRequest :: Manager -> Request -> IO ResponseAndLatency
runRequest manager req = do 
  withLatency $ httpLbs req manager

withLatency :: IO a -> IO (a,POSIXTime)
withLatency action = do
  tick <- getPOSIXTime 
  res <- action
  tock <- getPOSIXTime
  return $ (res, tock-tick)