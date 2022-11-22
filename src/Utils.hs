module Utils 
  ( digMap
  , fromRightErr
  , toPico
  , fromDiffTimeToSeconds
  , commandLineHandler
  , toMicroFromSec
  , Config(..)
  )
  where

import           Prelude
import           Data.Text (Text)
import           Text.Read (readMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM
import           GHC.Stack(HasCallStack)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import qualified Data.Fixed as Fixed
import           System.Environment(getArgs)
import           System.Exit(exitSuccess)

-- Given a list of keys recursively traverse the json 
-- and find the value
digMap :: [Text] -> Maybe A.Value -> Maybe A.Value
digMap _ Nothing = Nothing 
digMap [] val = val
digMap [x] (Just val) = lookUpFromObject x val 
digMap (x : xs) (Just val) =  digMap xs $ lookUpFromObject x val 

lookUpFromObject :: Text -> A.Value -> Maybe A.Value
lookUpFromObject key val =
  case val of 
    (A.Object v) -> KM.lookup (KM.fromText key) v
    _ -> Nothing

fromRightErr :: (HasCallStack,Show a) => Either a b -> b
fromRightErr (Right val) = val
fromRightErr (Left err) = error $ show err

fromJustErr :: HasCallStack => String -> Maybe b -> b
fromJustErr _mssg (Just a) = a
fromJustErr mssg Nothing = error mssg 

toPico :: Int -> Fixed.Pico
toPico value = Fixed.MkFixed $ ((toInteger value) * 1000000000000)

toMicroFromSec :: Int -> Int
toMicroFromSec = (*) (1000 * 1000)

fromDiffTimeToSeconds :: POSIXTime -> Fixed.Pico
fromDiffTimeToSeconds = nominalDiffTimeToSeconds

data Config = Config
  { numberOfThreads :: Int
  , timeToRun :: Int
  , pathOfTemplate :: String
  , responseTimeoutInSeconds :: Int
  , verbose :: Bool 
  }
  deriving (Show,Read)

commandLineHandler :: HasCallStack => IO Config
commandLineHandler = do
  getArgs >>= handler

handler :: HasCallStack => [String] -> IO Config
handler [] = putStrLn "No config provided use -h for help" >> exitSuccess
handler ["-h"] = help >> exitSuccess
handler ["-p",threads ,"-t", duration ,"-c",templatePath] = do
  let numberOfThreads = fromJustErr "Not a valid value" $ readMaybe threads
  let timeToRun = fromJustErr "Not a valid value" $ readMaybe duration
  let pathOfTemplate = templatePath
  let responseTimeoutInSeconds = 15
  let verbose = False
  pure Config{..}
handler _ = help >> exitSuccess

help :: IO ()
help = putStrLn "Pass only these flags and all are manadatory : \n -p <int value> For number of parallel session to be executed \n -t <int value> For number of seconds to run the load test \n -c For path of the template file"