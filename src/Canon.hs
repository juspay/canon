{-# LANGUAGE DeriveAnyClass      #-}
module Canon (main) where

import           Prelude
import           Control.Concurrent.Async
import           Control.Monad.Trans.Either
import qualified Control.Monad as CM
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
import qualified Utils as Utils
import           Control.Concurrent (forkIO,myThreadId,threadDelay)
import qualified System.Posix.Signals                      as OS

mkConfig :: Utils.Config
mkConfig =
  Utils.Config
    { numberOfThreads = 20
    , timeToRun = 30
    , pathOfTemplate = "/Users/shubhanshumani/loadtest/canon/src/api_config.json"
    , responseTimeoutInSeconds = 15
    , verbose = False
    }

type CanonRefs = (Ref.IORef Utils.Config,Ref.IORef Int,Ref.IORef Bool)

main :: HasCallStack => IO ()
main = do
  canonRefs@(_,apiErrorCounter,_) <- initCanonRef
  res <- BS.readFile $ Utils.pathOfTemplate config
  currentTime <- getPOSIXTime
  let sessionTemplate = Utils.fromRightErr $ SB.loadSessionTemplate res
  runController canonRefs currentTime
  finalResult <- loadRunner canonRefs sessionCount sessionTemplate
  errCount <- Ref.readIORef apiErrorCounter
  requestBuildErrCount <- Ref.readIORef RB.buildRequestErrorCounter
  print $ show $ finalResult{apiExecutionErrorCount = Just errCount, requestBuildErrorCount = Just requestBuildErrCount}
  where
    config = mkConfig
    sessionCount = Utils.numberOfThreads config
    runController refs currentTime = CM.void . forkIO $ controller refs currentTime
    initCanonRef = do
      configRef <- Ref.newIORef mkConfig
      apiErrorRef <- Ref.newIORef 0
      loadStopRef <- Ref.newIORef False
      pure (configRef,apiErrorRef,loadStopRef)

data Completed = Completed
  deriving (Show)

instance Ex.Exception Completed

controller :: CanonRefs -> POSIXTime -> IO ()
controller (loadTestConfig,_,loadStopRef) initialTime =
  CM.void $ Ex.catch timer (\(_e ::Completed) -> (putStrLn "Generating Report...") *> myThreadId)
  where 
    timer = CM.forever $ do
      timeToRun <- Utils.timeToRun <$> Ref.readIORef loadTestConfig
      currentTime <- getPOSIXTime
      _ <- OS.installHandler OS.sigINT (OS.CatchOnce ((Ref.writeIORef loadStopRef True) *> Ex.throwIO Completed)) Nothing
      let isTimeOver = (Utils.fromDiffTimeToSeconds $ currentTime - initialTime) >= (Utils.toPico timeToRun)
      CM.when isTimeOver $ (Ref.writeIORef loadStopRef True) *> Ex.throwIO Completed
      threadDelay $ Utils.toMicroFromSec 1

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

loadRunner :: CanonRefs -> Int -> SB.SessionTemplate -> IO LoadReport
loadRunner refs sessionCount sessionTemplate = do
  response <- withLatency $! runRequestParallely refs sessionCount sessionTemplate
  return $ generateReport (fst response) (snd response)

runRequestParallely :: CanonRefs -> Int -> SB.SessionTemplate -> IO [[ResponseAndLatency]]
runRequestParallely refs@(loadTestConfig,_,_) sessionCount sessionTemplate = do
  forConcurrently [1..sessionCount] (\_ -> do 
    responseTimeout <- Utils.responseTimeoutInSeconds <$> Ref.readIORef loadTestConfig
    manager <- Client.newManager $ tlsManagerSettings {Client.managerResponseTimeout = Client.responseTimeoutMicro $ Utils.toMicroFromSec responseTimeout}
    runSessionForever refs sessionTemplate manager [])


runSessionForever :: CanonRefs
  -> SB.SessionTemplate 
  -> Client.Manager 
  -> [ResponseAndLatency] 
  -> IO [ResponseAndLatency]
runSessionForever refs@(_,_,loadStopRef) sessionTemplate manager acc = do
  shouldStop <- Ref.readIORef loadStopRef
  if shouldStop
    then pure acc
    else do
      res <- runRequestSeqentially refs manager sessionTemplate
      runSessionForever refs sessionTemplate manager (acc ++ res)

runRequestSeqentially :: CanonRefs -> Client.Manager -> SB.SessionTemplate -> IO [ResponseAndLatency]
runRequestSeqentially refs manager sessionTemplate = do
  normalSession <- SB.generateNewSession sessionTemplate
  let sessionApiDataList = SB.generatedApiData normalSession
  let placeholderMapperCount = SB.numberOfMappingPresent (SB.normalisedPlaceholder normalSession)
  executeApiTemplate refs manager (SB.normalisedPlaceholder normalSession) placeholderMapperCount sessionApiDataList []

executeApiTemplate :: CanonRefs 
  -> Client.Manager 
  -> HMap.HashMap Text.Text SB.PlaceHolder 
  -> Int 
  -> [(Text.Text,SB.ApiTemplate)] 
  -> [ResponseAndLatency] 
  -> IO [ResponseAndLatency]
executeApiTemplate _ _ _ _ [] acc = pure acc
executeApiTemplate refs manager placeholder _ [apiData] acc = do
  eitherResponseWithLatency <- buildAndRunRequest refs placeholder apiData manager
  case eitherResponseWithLatency of
    Right responseWithLatency -> pure $ acc ++ [responseWithLatency]
    Left err -> do
      printLog refs $ show err
      pure $ acc
executeApiTemplate refs manager placeholder mappingCount ((apiLabel,apiData) : xs) acc = do
  eitherResponseWithLatency <- buildAndRunRequest refs placeholder (apiLabel,apiData) manager
  case eitherResponseWithLatency of
      Right (response,latency) -> do
        (updatedPlaceHolder,mappingCtr) <-
          if mappingCount > 0
            then do
              updPlaceholder <- decodeResponseToValue refs placeholder apiLabel (Client.responseBody response)
              let updatedMapperCount = SB.numberOfMappingPresent updPlaceholder
              pure (updPlaceholder,updatedMapperCount)
            else pure (placeholder,mappingCount)
        executeApiTemplate refs manager updatedPlaceHolder mappingCtr xs (acc ++ [(response,latency)])
      Left err -> do
        printLog refs $ show err
        executeApiTemplate refs manager placeholder mappingCount xs acc

buildAndRunRequest :: CanonRefs 
  -> HMap.HashMap Text.Text SB.PlaceHolder 
  -> (Text.Text,SB.ApiTemplate) 
  -> Client.Manager 
  -> IO (Either SB.ConversionError ResponseAndLatency)
buildAndRunRequest refs placeholder apiTemplate manager = runEitherT $ do
  req <- newEitherT $! RB.buildRequest placeholder apiTemplate
  responseWithLatency <- newEitherT $ runRequest refs manager req
  pure responseWithLatency

runRequest :: CanonRefs 
  -> Client.Manager 
  -> Client.Request 
  -> IO (Either SB.ConversionError ResponseAndLatency)
runRequest refs@(_,apiErrorCounter,_) manager req = do
    res <- either actOnApiError (pure . Right) =<< (Ex.try $! (withLatency $! Client.httpLbs req manager))
    printLog refs $ show res
    pure res
  where
    actOnApiError err = do
      _ <- Ref.atomicModifyIORef' apiErrorCounter (\x -> (x+1,()))
      pure . Left $ SB.HttpException err

-- TODO : Rework this
decodeResponseToValue :: CanonRefs 
  -> HMap.HashMap Text.Text SB.PlaceHolder 
  -> Text.Text 
  -> BSL.ByteString 
  -> IO (HMap.HashMap Text.Text SB.PlaceHolder)
decodeResponseToValue refs placeholder apiLabel response = do
  case eitherDecodeStrict $ BSL.toStrict response of
    Right (val :: Value) -> do
      pure $ HMap.map (updateValuesInPlaceholder (apiLabel,val)) placeholder
    Left err -> do
      printLog refs $ (Text.unpack apiLabel) <> " Failed to decode to a JSON" <>  err
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

printLog :: (Show a) => CanonRefs -> a -> IO ()
printLog (loadTestConfig,_,_) mssg = do
  isVerbose <- Utils.verbose <$> Ref.readIORef loadTestConfig
  if isVerbose
    then print mssg
    else pure ()
