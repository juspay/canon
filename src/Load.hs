{-# LANGUAGE DeriveAnyClass      #-}
module Load (main,fromDiffTimeToSeconds)where

import Prelude
import Network.HTTP.Client
import Data.ByteString.Lazy hiding (filter,elem,concat,length)
import Control.Concurrent.Async
import GHC.Generics
import Data.Aeson
import Network.HTTP.Types
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Fixed
import qualified RequestBuilder as RB
import qualified SessionBuilder as SB
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import GHC.Stack (HasCallStack)
import Data.Traversable (for)
import Network.HTTP.Client.TLS (tlsManagerSettings)

main :: HasCallStack => IO ()
main = do
  res <- BS.readFile "/Users/shubhanshumani/loadtest/load/src/api_config.json"
  let sessionCount = 10
  let sessionTemplate = fromRightErr $ SB.loadSessionTemplate res
  manager <- newManager tlsManagerSettings 
  result <- loadRunner manager sessionCount sessionTemplate
  print result

type WithLatency a = (a,POSIXTime)

type ResponseAndLatency = WithLatency (Response ByteString)

data LoadReport = 
  LoadReport
    { totalRequest :: Int
    , successResponse :: Int
    , failureResponse :: Int
    , rps :: Pico
    , avgLatency :: Pico
    }
    deriving (Show , Generic , ToJSON)

loadRunner :: Manager -> Int -> SB.SessionTemplate -> IO String
loadRunner manager sessionCount sessionTemplate = do
  requestsForSession <- makeSessions sessionCount sessionTemplate
  response <- withLatency $ runRequestParallely manager requestsForSession
  return $ show $ generateReport (fst response) (snd response)

makeSessions :: Int -> SB.SessionTemplate ->  IO [[Request]]
makeSessions cnt sessionTemplate = sequence $ generate <$> [1..cnt]
  where
    generate _ = makeSessionRequests sessionTemplate

makeSessionRequests :: SB.SessionTemplate -> IO [Request]
makeSessionRequests sessionTemplate = do
  newSession <- SB.generateNewSession sessionTemplate
  let sessionApiData = HMap.toList $ SB.generatedApiData newSession
  for sessionApiData $ \(_,apiData) -> do 
      RB.buildRequest (SB.normalisedPlaceholder newSession) apiData

generateReport :: [[ResponseAndLatency]] -> POSIXTime -> LoadReport
generateReport responses totalTime =
  LoadReport{..}
  where
    successResponses = filter ((200==) . statusCode . responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    rps = (toPico totalRequest)/(fromDiffTimeToSeconds totalTime)
    avgLatency = 
      let total = sum $ (fromDiffTimeToSeconds . snd) <$> successResponses
      in total / (toPico successResponse)

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

fromRightErr :: (HasCallStack,Show a) => Either a b -> b
fromRightErr (Right val) = val
fromRightErr (Left err) = error $ show err


toPico :: Int -> Pico
toPico value = MkFixed $ ((toInteger value) * 1000000000000)

fromDiffTimeToSeconds :: POSIXTime -> Pico
fromDiffTimeToSeconds = nominalDiffTimeToSeconds