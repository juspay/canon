module Utils 
  ( digMap
  , fromRightErr
  , toPico
  , fromDiffTimeToSeconds
  )
  where

import           Prelude
import           Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM
import           GHC.Stack(HasCallStack)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import qualified Data.Fixed as Fixed


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

toPico :: Int -> Fixed.Pico
toPico value = Fixed.MkFixed $ ((toInteger value) * 1000000000000)

fromDiffTimeToSeconds :: POSIXTime -> Fixed.Pico
fromDiffTimeToSeconds = nominalDiffTimeToSeconds