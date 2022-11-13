module Main  where

import Prelude
import qualified Load as L
import qualified SessionBuilder as RB
import qualified Data.ByteString as BS
import qualified RequestBuilder as R
import qualified Data.HashMap.Strict as HMap
import qualified Network.HTTP.Client as Client
main :: IO ()
main = do 
  res <- BS.readFile "/Users/shubhanshumani/loadtest/load/src/api_config.json"
  case RB.loadSessionTemplate res of
    Left err -> print err
    Right sess -> do 
        ans <- RB.generateNewSession sess
        let placeholder = RB.normalisedPlaceholder ans
        let apiList = HMap.toList $ RB.generatedApiData ans
        resp <- sequence $ R.buildRequest placeholder . snd <$> apiList
        print $ show $ fn . Client.requestBody <$> resp
  where
    --fn :: Client.RequestBody -> Maybe ByteString
    fn (Client.RequestBodyLBS s) = Just s
    fn _ = Nothing
  -- L.main
