{-# LANGUAGE DeriveAnyClass      #-}
module Canon (main) where

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


mkConfig :: Utils.Config
mkConfig =
  Utils.Config
    { numberOfThreads = 100
    , timeToRun = 30
    , pathOfTemplate = "/Users/shubhanshumani/loadtest/canon/src/api_config.json"
    , responseTimeoutInSeconds = 15
    , verbose = False
    }

main :: HasCallStack => IO ()
main = do
  res <- BS.readFile $ Utils.pathOfTemplate config
  -- currentTime <- getPOSIXTime
  let sessionTemplate = Utils.fromRightErr $ SB.loadSessionTemplate res
  finalResult <- loadRunner sessionCount sessionTemplate
  errCount <- Ref.readIORef apiErrorCounter
  requestBuildErrCount <- Ref.readIORef RB.buildRequestErrorCounter
  print $ show $ finalResult{apiExecutionErrorCount = Just errCount, requestBuildErrorCount = Just requestBuildErrCount}
  where
    config = mkConfig
    sessionCount = Utils.numberOfThreads config
    -- timeInSeconds = Utils.timeToRun config

apiErrorCounter :: Ref.IORef Int
apiErrorCounter = unsafePerformIO $ Ref.newIORef 0

loadTestConfig :: Ref.IORef Utils.Config
loadTestConfig = unsafePerformIO $ Ref.newIORef mkConfig

startTimeRef :: Ref.IORef POSIXTime
startTimeRef = unsafePerformIO $ do 
  curTime <- getPOSIXTime
  Ref.newIORef curTime

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
    , requestBuildErrorCount :: Maybe Int
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
      , requestBuildErrorCount = (+) <$> (requestBuildErrorCount a) <*> (requestBuildErrorCount b)
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
      , requestBuildErrorCount = Just 0
      }

-- runLoadTillGivenTime :: POSIXTime -> Int -> Int -> SB.SessionTemplate -> LoadReport -> IO LoadReport
-- runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate acc = do
--   currentTime <- getPOSIXTime
--   if (Utils.fromDiffTimeToSeconds $ currentTime - intialTime) < Utils.toPico totalTimeToRun
--     then do
--       res <- loadRunner numberOfParallelThreads sessionTemplate
--       runLoadTillGivenTime intialTime numberOfParallelThreads totalTimeToRun sessionTemplate (acc <> res)
--     else do
--       pure acc

loadRunner :: Int -> SB.SessionTemplate -> IO LoadReport
loadRunner sessionCount sessionTemplate = do
  response <- withLatency $! runRequestParallely sessionCount sessionTemplate
  return $ generateReport (fst response) (snd response)

runRequestParallely :: Int -> SB.SessionTemplate -> IO [[ResponseAndLatency]]
runRequestParallely sessionCount sessionTemplate = do
  forConcurrently [1..sessionCount] (\_ -> do 
    responseTimeout <- Utils.responseTimeoutInSeconds <$> Ref.readIORef loadTestConfig
    manager <- Client.newManager $ tlsManagerSettings {Client.managerResponseTimeout = Client.responseTimeoutMicro $ Utils.toMicroFromSec responseTimeout}
    runSessionForever sessionTemplate manager [])


runSessionForever :: SB.SessionTemplate -> Client.Manager -> [ResponseAndLatency] -> IO [ResponseAndLatency]
runSessionForever sessionTemplate manager acc = do
  startTime <- Ref.readIORef startTimeRef
  currentTime <- getPOSIXTime
  timeToRun <- Utils.timeToRun <$> Ref.readIORef loadTestConfig
  if ((Utils.fromDiffTimeToSeconds $ currentTime - startTime) >= (Utils.toPico timeToRun))
    then pure acc
    else do
      res <- runRequestSeqentially manager sessionTemplate
      runSessionForever sessionTemplate manager (acc ++ res)

runRequestSeqentially :: Client.Manager -> SB.SessionTemplate -> IO [ResponseAndLatency]
runRequestSeqentially manager sessionTemplate = do
  normalSession <- SB.generateNewSession sessionTemplate
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
      printLog $ show err
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
        printLog $ show err
        executeApiTemplate manager placeholder mappingCount xs acc

buildAndRunRequest :: HMap.HashMap Text.Text SB.PlaceHolder -> (Text.Text,SB.ApiTemplate) -> Client.Manager -> IO (Either SB.ConversionError ResponseAndLatency)
buildAndRunRequest placeholder apiTemplate manager = runEitherT $ do
  req <- newEitherT $! RB.buildRequest placeholder apiTemplate
  responseWithLatency <- newEitherT $ runRequest manager req
  pure responseWithLatency

runRequest :: Client.Manager -> Client.Request -> IO (Either SB.ConversionError ResponseAndLatency)
runRequest manager req = do
    res <- either actOnApiError (pure . Right) =<< (Ex.try $! (withLatency $! Client.httpLbs req manager))
    printLog $ show res
    pure res
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
      printLog $ (Text.unpack apiLabel) <> " Failed to decode to a JSON" <>  err
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
    successResponses = filter ((\code -> code >= 200 && code <= 399) . Client.statusCode . Client.responseStatus . fst) $ concat responses
    successResponse = length successResponses
    failureResponse = totalRequest - successResponse
    totalRequest = length $ concat responses
    totalTimePerBatch = [totalTime]
    latencies =
      if successResponse /= 0
        then (snd <$> successResponses)
        else [0]
    apiExecutionErrorCount = Just 0
    requestBuildErrorCount = Just 0

withLatency :: IO a -> IO (a,POSIXTime)
withLatency action = do
  tick <- getPOSIXTime
  res <- action
  tock <- getPOSIXTime
  return $ (res, tock-tick)

printLog :: (Show a) => a -> IO ()
printLog mssg = do
  isVerbose <- Utils.verbose <$> Ref.readIORef loadTestConfig
  if isVerbose
    then print mssg
    else pure ()
