{-# LANGUAGE DeriveAnyClass      #-}
module Load (main) where

import           Prelude
import           Control.Concurrent.Async
import           Control.Monad.Trans.Either
import qualified Control.Exception as Ex
import           Data.Aeson (ToJSON,Value(String),eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as Arr
import qualified Data.Text as Text
import qualified Data.IORef as Ref
import           Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import           Data.Maybe (fromMaybe)
import           GHC.Generics(Generic)
import           GHC.Stack (HasCallStack)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified RequestBuilder as RB
import qualified SessionBuilder as SB
import           System.IO.Unsafe (unsafePerformIO)
import qualified Utils as Utils

main :: HasCallStack => IO ()
main = do
  res <- BS.readFile "/Users/shubhanshumani/loadtest/load/src/api_config.json"
  let sessionCount = 500
  let timeInSeconds = 10
  let responseTimeout = 15
  Ref.writeIORef loadTestConfig (sessionCount,timeInSeconds,responseTimeout)
  currentTime <- getPOSIXTime
  let sessionTemplate = Utils.fromRightErr $ SB.loadSessionTemplate res
  finalResult <- runLoadTillGivenTime currentTime sessionCount timeInSeconds sessionTemplate mempty
  errCount <- Ref.readIORef apiErrorCounter
  print $ show $ finalResult{apiExecutionErrorCount = Just errCount}

apiErrorCounter :: Ref.IORef Int
apiErrorCounter = unsafePerformIO $ Ref.newIORef 0

loadTestConfig :: Ref.IORef (Int,Int,Int)
loadTestConfig = unsafePerformIO $ Ref.newIORef (0,0,0)

type WithLatency a = (a,POSIXTime)

type ResponseAndLatency = WithLatency (Client.Response BSL.ByteString)

data LoadReport = 
  LoadReport
    { totalRequest :: Int
    , successResponse :: Int
    , failureResponse :: Int
    , totalTimePerBatch :: [POSIXTime]
    , latencies :: [POSIXTime]
    , apiExecutionErrorCount :: Maybe Int
    }
    deriving (Show , Generic , ToJSON)

instance Semigroup LoadReport where
  (<>) :: LoadReport -> LoadReport -> LoadReport
  (<>) a b = 
    LoadReport
      { totalRequest = (totalRequest a) + (totalRequest b)
      , successResponse = (successResponse a) + (successResponse b)
      , failureResponse = (failureResponse a) + (failureResponse b)
      , totalTimePerBatch = (totalTimePerBatch a) <> (totalTimePerBatch b)
      , latencies = (latencies a) <> (latencies b)
      , apiExecutionErrorCount = (+) <$> (apiExecutionErrorCount a) <*> (apiExecutionErrorCount b)
      }

instance Monoid LoadReport where
  mempty :: LoadReport
  mempty =
    LoadReport
      { totalRequest = 0
      , successResponse = 0
      , failureResponse = 0
      , totalTimePerBatch = []
      , latencies = []
      , apiExecutionErrorCount = Just 0
      }

runLoadTillGivenTime :: POSIXTime -> Int -> Int -> SB.SessionTemplate -> LoadReport -> IO LoadReport
runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate acc = do
  currentTime <- getPOSIXTime
  if (Utils.fromDiffTimeToSeconds $ currentTime - intialTime) < Utils.toPico totalTimeToRun
    then do
      res <- loadRunner numberOfParallelThreads sessionTemplate
      runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate (acc <> res)
    else do
      pure acc

loadRunner :: Int -> SB.SessionTemplate -> IO LoadReport
loadRunner sessionCount sessionTemplate = do
  requestsForSession <- makeSessions sessionCount sessionTemplate
  response <- withLatency $! runRequestParallely requestsForSession
  return $ generateReport (fst response) (snd response)

makeSessions :: Int -> SB.SessionTemplate ->  IO [SB.NormalisedSession]
makeSessions cnt sessionTemplate = sequence $ generate <$> [1..cnt]
  where
    generate _ = SB.generateNewSession sessionTemplate

runRequestParallely :: [SB.NormalisedSession] -> IO [[ResponseAndLatency]]
runRequestParallely normalSessions = do
  mapConcurrently runRequestSeqentially normalSessions

runRequestSeqentially :: SB.NormalisedSession -> IO [ResponseAndLatency]
runRequestSeqentially normalSession = do
  (_,_,responseTimeout) <- Ref.readIORef loadTestConfig
  manager <- Client.newManager $ tlsManagerSettings {Client.managerResponseTimeout = Client.responseTimeoutMicro $ Utils.toMicroFromSec responseTimeout}
  let sessionApiDataList = SB.generatedApiData normalSession
  let placeholderMapperCount = SB.numberOfMappingPresent (SB.normalisedPlaceholder normalSession)
  executeApiTemplate manager (SB.normalisedPlaceholder normalSession) placeholderMapperCount sessionApiDataList []

executeApiTemplate :: Client.Manager -> HMap.HashMap Text.Text SB.PlaceHolder -> Int -> [(Text.Text,SB.ApiTemplate)] -> [ResponseAndLatency] -> IO [ResponseAndLatency]
executeApiTemplate _ _ _ [] acc = pure acc
executeApiTemplate manager placeholder _ [apiData] acc = do
  eitherResponseWithLatency <- buildAndRunRequest placeholder apiData manager
  case eitherResponseWithLatency of
    Right responseWithLatency -> pure $ acc ++ [responseWithLatency]
    Left err -> do 
      print $ show err
      pure $ acc
executeApiTemplate manager placeholder mappingCount ((apiLabel,apiData) : xs) acc = do 
  eitherResponseWithLatency <- buildAndRunRequest placeholder (apiLabel,apiData) manager
  case eitherResponseWithLatency of
      Right (response,latency) -> do
        (updatedPlaceHolder,mappingCtr) <- 
          if mappingCount > 0
            then do 
              updPlaceholder <- decodeResponseToValue placeholder apiLabel (Client.responseBody response)
              let updatedMapperCount = SB.numberOfMappingPresent updPlaceholder
              pure (updPlaceholder,updatedMapperCount)
            else pure (placeholder,mappingCount)
        executeApiTemplate manager updatedPlaceHolder mappingCtr xs (acc ++ [(response,latency)])
      Left err -> do
        print $ show err
        executeApiTemplate manager placeholder mappingCount xs acc

buildAndRunRequest :: HMap.HashMap Text.Text SB.PlaceHolder -> (Text.Text,SB.ApiTemplate) -> Client.Manager -> IO (Either SB.ConversionError ResponseAndLatency)
buildAndRunRequest placeholder apiTemplate manager = runEitherT $ do
  req <- newEitherT $! RB.buildRequest placeholder apiTemplate
  responseWithLatency <- newEitherT $ runRequest manager req
  pure responseWithLatency

runRequest :: Client.Manager -> Client.Request -> IO (Either SB.ConversionError ResponseAndLatency)
runRequest manager req =
    either actOnApiError (pure . Right) =<< (Ex.try $! (withLatency $! Client.httpLbs req manager))
  where
    actOnApiError err = do 
      _ <- Ref.atomicModifyIORef' apiErrorCounter (\x -> (x+1,()))
      pure . Left $ SB.HttpException err

-- TODO : Rework this 
decodeResponseToValue :: HMap.HashMap Text.Text SB.PlaceHolder -> Text.Text -> BSL.ByteString ->  IO (HMap.HashMap Text.Text SB.PlaceHolder)
decodeResponseToValue placeholder apiLabel response = do
  case eitherDecodeStrict $ BSL.toStrict response of
    Right (val :: Value) -> do
      pure $ HMap.map (updateValuesInPlaceholder (apiLabel,val)) placeholder
    Left err -> do 
      print $ (Text.unpack apiLabel) <> " Failed to decode to a JSON" <>  err
      pure placeholder

updateValuesInPlaceholder :: (Text.Text , Value) -> SB.PlaceHolder -> SB.PlaceHolder
updateValuesInPlaceholder _ (SB.Constant a) = SB.Constant a
updateValuesInPlaceholder _ (SB.Command a) = error $ "Found command which was not expected in this portion of execution " <> (Text.unpack a)
updateValuesInPlaceholder (apilabel , response) (SB.Mapping placeholder) = fromMaybe (SB.Mapping placeholder) $ do
  (label , mapingRoute) <- Arr.uncons $ Arr.filter (`notElem` ["api","response"]) $ Text.splitOn "~" placeholder
  if apilabel == label
    then do
      value <- Utils.digMap mapingRoute (Just response)
      case value of
        (String s) -> Just $ SB.Constant s
        _ -> Nothing
    else
      Nothing

generateReport :: [[ResponseAndLatency]] -> POSIXTime -> LoadReport
generateReport responses totalTime =
  LoadReport{..}
  where
    successResponses = filter ((200==) . Client.statusCode . Client.responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    totalTimePerBatch = [totalTime]
    latencies = 
      if successResponse /= 0 
        then (snd <$> successResponses)
        else [0]
    apiExecutionErrorCount = Just 0

withLatency :: IO a -> IO (a,POSIXTime)
withLatency action = do
  tick <- getPOSIXTime 
  res <- action
  tock <- getPOSIXTime
  return $ (res, tock-tick)