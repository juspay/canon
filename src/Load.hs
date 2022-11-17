{-# LANGUAGE DeriveAnyClass      #-}
module Load (main) where

import Prelude
import Network.HTTP.Client
import Data.ByteString.Lazy hiding (filter,elem,concat,length,notElem)
import qualified Data.List as Arr
import Control.Concurrent.Async
import GHC.Generics
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Network.HTTP.Types
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Fixed
import qualified RequestBuilder as RB
import qualified SessionBuilder as SB
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import GHC.Stack (HasCallStack)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM
import Control.Monad.Trans.Either
import qualified Control.Exception as Ex

main :: HasCallStack => IO ()
main = do
  res <- BS.readFile "/Users/shubhanshumani/loadtest/load/src/api_config.json"
  let sessionCount = 1
  let timeInSeconds  = 1
  currentTime <- getPOSIXTime
  let sessionTemplate = fromRightErr $ SB.loadSessionTemplate res
  finalResult <-  runLoadTillGivenTime currentTime sessionCount timeInSeconds sessionTemplate mempty
  print $ show finalResult

runLoadTillGivenTime :: POSIXTime -> Int -> Int -> SB.SessionTemplate -> LoadReport -> IO LoadReport
runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate acc = do
  currentTime <- getPOSIXTime
  if (fromDiffTimeToSeconds $ currentTime - intialTime) < toPico totalTimeToRun
    then do
      res <- loadRunner numberOfParallelThreads sessionTemplate
      runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate (acc <> res)
    else do
      pure acc

type WithLatency a = (a,POSIXTime)

type ResponseAndLatency = WithLatency (Response ByteString)

data LoadReport = 
  LoadReport
    { totalRequest :: Int
    , successResponse :: Int
    , failureResponse :: Int
    , totalTimePerBatch :: [POSIXTime]
    , averageLatencyPerBatch :: [Pico]
    }
    deriving (Show , Generic , ToJSON)


instance Semigroup LoadReport where
  (<>) :: LoadReport -> LoadReport -> LoadReport
  (<>) a b = 
    LoadReport
      {
        totalRequest = (totalRequest a) + (totalRequest b)
      , successResponse = (successResponse a) + (successResponse b)
      , failureResponse = (failureResponse a) + (failureResponse b)
      , totalTimePerBatch = (totalTimePerBatch a) <> (totalTimePerBatch b)
      , averageLatencyPerBatch = (averageLatencyPerBatch a) <> (averageLatencyPerBatch b)
      }

instance Monoid LoadReport where
  mempty :: LoadReport
  mempty =
    LoadReport
      { totalRequest = 0
      , successResponse = 0
      , failureResponse = 0
      , totalTimePerBatch = []
      , averageLatencyPerBatch = []
      }

loadRunner :: Int -> SB.SessionTemplate -> IO LoadReport
loadRunner sessionCount sessionTemplate = do
  requestsForSession <- makeSessions sessionCount sessionTemplate
  response <- withLatency $ runRequestParallely requestsForSession
  return $ generateReport (fst response) (snd response)

makeSessions :: Int -> SB.SessionTemplate ->  IO [SB.NormalisedSession]
makeSessions cnt sessionTemplate = sequence $ generate <$> [1..cnt]
  where
    generate _ = SB.generateNewSession sessionTemplate

runRequestParallely :: [SB.NormalisedSession] -> IO [[ResponseAndLatency]]
runRequestParallely normalSessions = do
  mapConcurrently runRequestSeqentially normalSessions

generateReport :: [[ResponseAndLatency]] -> POSIXTime -> LoadReport
generateReport responses totalTime =
  LoadReport{..}
  where
    successResponses = filter ((200==) . statusCode . responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    totalTimePerBatch = [totalTime]
    averageLatencyPerBatch = [(fromDiffTimeToSeconds $ sum $ snd <$> successResponses) / (toPico successResponse)]

runRequestSeqentially :: SB.NormalisedSession -> IO [ResponseAndLatency]
runRequestSeqentially normalSession = do 
  manager <- newManager tlsManagerSettings
  let sessionApiDataList = SB.generatedApiData normalSession
  let placeholderMapperCount = numberOfMappingPresent (SB.normalisedPlaceholder normalSession)
  go manager (SB.normalisedPlaceholder normalSession) placeholderMapperCount sessionApiDataList []
  where
    go :: Manager -> HMap.HashMap Text.Text SB.PlaceHolder -> Int -> [(Text.Text,SB.ApiTemplate)] -> [ResponseAndLatency] -> IO [ResponseAndLatency]
    go _ _ _ [] acc = pure acc
    go manager placeholder _ [apiData] acc = do
      eitherResponseWithLatency <- buildAndRunRequest placeholder apiData manager
      case eitherResponseWithLatency of
        Right responseWithLatency -> pure $ acc ++ [responseWithLatency]
        Left err -> do 
          print $ show err
          pure $ acc
    go manager placeholder mappingCount ((apiLabel,apiData) : xs) acc = do 
      eitherResponseWithLatency <- buildAndRunRequest placeholder (apiLabel,apiData) manager
      case eitherResponseWithLatency of
          Right (response,latency) -> do
            (updatedPlaceHolder,mappingCtr) <- 
              if mappingCount > 0
                then do 
                  updPlaceholder <- decodeResponseToValue placeholder apiLabel (responseBody response)
                  let updatedMapperCount = numberOfMappingPresent updPlaceholder
                  pure (updPlaceholder,updatedMapperCount)
                else pure (placeholder,mappingCount)
            go manager updatedPlaceHolder mappingCtr xs (acc ++ [(response,latency)])
          Left err -> do
            print $ show err
            go manager placeholder mappingCount xs acc

numberOfMappingPresent :: HMap.HashMap Text.Text SB.PlaceHolder -> Int
numberOfMappingPresent placeholderMap =
  HMap.foldl' (\mappingCount currentValue -> 
    case currentValue of
      SB.Mapping _ -> mappingCount+1
      _ -> mappingCount) 0 placeholderMap

buildAndRunRequest :: HMap.HashMap Text.Text SB.PlaceHolder -> (Text.Text,SB.ApiTemplate) -> Manager -> IO (Either SB.ConversionError ResponseAndLatency)
buildAndRunRequest placeholder apiTemplate manager = runEitherT $ do
  req <- newEitherT $ RB.buildRequest placeholder apiTemplate
  responseWithLatency <- newEitherT $ runRequest manager req
  pure responseWithLatency

runRequest :: Manager -> Request -> IO (Either SB.ConversionError ResponseAndLatency)
runRequest manager req = do 
  (eitherResponse :: Either Ex.SomeException ResponseAndLatency) <- Ex.try (withLatency $ httpLbs req manager)
  pure $ either (Left . SB.HttpException) (Right) eitherResponse

withLatency :: IO a -> IO (a,POSIXTime)
withLatency action = do
  tick <- getPOSIXTime 
  res <- action
  tock <- getPOSIXTime
  print $ "tick :" <> show tick
  print $ "tock :" <> show tock
  return $ (res, tock-tick)

fromRightErr :: (HasCallStack,Show a) => Either a b -> b
fromRightErr (Right val) = val
fromRightErr (Left err) = error $ show err


toPico :: Int -> Pico
toPico value = MkFixed $ ((toInteger value) * 1000000000000)

fromDiffTimeToSeconds :: POSIXTime -> Pico
fromDiffTimeToSeconds = nominalDiffTimeToSeconds

-- TODO : Rework this 
decodeResponseToValue :: HMap.HashMap Text.Text SB.PlaceHolder -> Text.Text -> ByteString ->  IO (HMap.HashMap Text.Text SB.PlaceHolder)
decodeResponseToValue placeholder apiLabel response = do
  case eitherDecodeStrict $ toStrict response of
    Right (val :: Value) -> do
      pure $ HMap.map (updateValuesInPlaceholder (apiLabel,val)) placeholder
    Left err -> do 
      print $ (Text.unpack apiLabel) <> " Failed to decode to a JSON" <>  err
      pure placeholder

-- Remove the replacement of # with mapping route in the api template
updateValuesInPlaceholder :: (Text.Text , Value) -> SB.PlaceHolder -> SB.PlaceHolder
updateValuesInPlaceholder _ (SB.Constant a) = SB.Constant a
updateValuesInPlaceholder _ (SB.Command a) = error $ "Found command which was not expected in this portion of execution " <> (Text.unpack a)
updateValuesInPlaceholder (apilabel , response) (SB.Mapping placeholder) = fromMaybe (SB.Mapping placeholder) $ do
  (label , mapingRoute) <- Arr.uncons $ Arr.filter (`notElem` ["api","response"]) $ Text.splitOn "~" placeholder
  if apilabel == label
    then do
      value <- digMap mapingRoute (Just response)
      case value of
        (String s) -> Just $ SB.Constant s
        _ -> Nothing
    else
      Nothing

digMap :: [Text.Text] -> Maybe Value -> Maybe Value
digMap _ Nothing = Nothing 
digMap [] val = val
digMap [x] (Just val) = lookUpFromObject x val 
digMap (x : xs) (Just val) =  digMap xs $ lookUpFromObject x val 


lookUpFromObject :: Text.Text -> Value -> Maybe Value
lookUpFromObject key val =
  case val of 
    (Object v) -> KM.lookup (KM.fromText key) v
    _ -> Nothing
    
-- makeValue :: Maybe Value
-- makeValue =
--   let m = eitherDecodeStrict "{\n    \"txn_uuid\": \"euladAbdm8j6NxsoGQv\",\n    \"txn_id\": \"mxplayer-QC1668095957-1\",\n    \"status\": \"CHARGED\",\n    \"payment\": {\n        \"authentication\": {\n            \"url\": \"https://sandbox.juspay.in/v2/pay/finish/mxplayer/euladAbdm8j6NxsoGQv/QC1668095957\",\n            \"method\": \"GET\"\n        }\n    },\n    \"order_id\": \"QC1668095957\",\n    \"offer_details\": {\n        \"offers\": []\n    }\n}"
--   in case m of
--       Left _ -> Nothing
--       Right v -> Just v