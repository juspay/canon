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

main :: HasCallStack => IO ()
main = do
  res <- BS.readFile "/Users/shubhanshumani/loadtest/load/src/api_config.json"
  let sessionCount = 1
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

makeSessions :: Int -> SB.SessionTemplate ->  IO [SB.NormalisedSession]
makeSessions cnt sessionTemplate = sequence $ generate <$> [1..cnt]
  where
    generate _ = makeNormalisedSession sessionTemplate

makeNormalisedSession :: SB.SessionTemplate -> IO SB.NormalisedSession
makeNormalisedSession sessionTemplate = SB.generateNewSession sessionTemplate

generateReport :: [[ResponseAndLatency]] -> POSIXTime -> LoadReport
generateReport responses totalTime =
  LoadReport{..}
  where
    successResponses = filter ((200==) . statusCode . responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    rps = (toPico totalRequest)
    avgLatency = 
      let total = sum $ (fromDiffTimeToSeconds . snd) <$> successResponses
      in total

-- TODO : Rework this 
runRequestSeqentially :: Manager -> SB.NormalisedSession -> IO [ResponseAndLatency]
runRequestSeqentially manager normalSession = do 
  let sessionApiDataList = SB.generatedApiData normalSession
  go (SB.normalisedPlaceholder normalSession) sessionApiDataList []
  where
    go :: HMap.HashMap Text.Text SB.PlaceHolder -> [(Text.Text,SB.ApiTemplate)] -> [ResponseAndLatency] -> IO [ResponseAndLatency]
    go _ [] acc = pure acc
    go placeholder [apiData] acc = do
      req <- RB.buildRequest placeholder apiData
      responseWithLatency <- runRequest manager req
      pure $ acc ++ [responseWithLatency]
    go placeholder ((apiLabel,apiData) : xs) acc = do 
      req <- RB.buildRequest placeholder (apiLabel,apiData)
      (response,latency) <- runRequest manager req
      updatedPlaceHolder <- decodeResponseToValue placeholder apiLabel (responseBody response)
      go updatedPlaceHolder xs (acc ++ [(response,latency)])

runRequestParallely :: Manager -> [SB.NormalisedSession] -> IO [[ResponseAndLatency]]
runRequestParallely manager normalSessions = do 
  mapConcurrently (runRequestSeqentially manager) normalSessions

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