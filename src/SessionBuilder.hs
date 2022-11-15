{-# LANGUAGE DeriveAnyClass      #-}
module SessionBuilder 
  ( testParse
  , randomInt
  , generateNewSession
  , loadSessionTemplate
  , normaliseApiData
  , NormalisedSession(..)
  , PlaceHolder(..)
  , ReqMethod(..)
  , ContentType(..)
  , ApiTemplate(..)
  , SessionTemplate(..)
  )
  where

import Prelude
import Data.Text
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HMap
import GHC.Generics(Generic)
import Data.Aeson (FromJSON (parseJSON), withText, ToJSON (toJSON), eitherDecodeStrict, Value(String)) 
import Optics.Prism (Prism', prism')
import           Data.Attoparsec.Text(char,takeText, parseOnly,try)
import           Optics.AffineFold (preview)
import           Data.Aeson.Types (Parser)
import           Optics.Review (review)
import Data.ByteString
import System.Random (randomIO)
import Data.Word (Word8)

data SessionTemplate = 
    SessionTemplate
      { placeholder :: HMap.HashMap Text PlaceHolder
      , api :: HMap.HashMap Text ApiTemplate
      }
    deriving (Generic, FromJSON,ToJSON, Show)

data NormalisedSession = 
    NormalisedSession
      { normalisedPlaceholder :: HMap.HashMap Text PlaceHolder
      , generatedApiData :: HMap.HashMap Text ApiTemplate
      }
    deriving (Generic, FromJSON,ToJSON, Show)

data ApiTemplate = 
  ApiTemplate
    { endpoint :: Text
    , method :: ReqMethod
    , contentType :: Maybe ContentType
    , headers :: HMap.HashMap Text Text
    , request :: HMap.HashMap Text Text
    }
    deriving (Generic, FromJSON,ToJSON, Show)

data ReqMethod = 
    GET 
  | POST
  deriving (Generic, FromJSON,ToJSON, Show)

data ContentType =
    JSON 
  | URLFORMENCODED
  deriving (Generic, FromJSON,ToJSON, Show)

data PlaceHolder = 
    Command Text
  | Mapping Text
  | Constant Text
  deriving (Show)

instance ToJSON PlaceHolder where
  toJSON = String . review placeHolderText

instance FromJSON PlaceHolder where
  parseJSON = withText "PlaceHolder" go
    where
      go :: Text -> Parser PlaceHolder
      go t = case preview placeHolderText t of
        Nothing  -> fail "Invalid value"
        Just pHolder -> pure pHolder

placeHolderText :: Prism' Text PlaceHolder
placeHolderText = prism' out into
  where 
    out a = case a of 
      Command b -> b
      Mapping b -> b
      Constant b -> b
    into = either (const Nothing) pure . parseOnly p
    p = try parseCommand <|> parseMapping <|> parseConstant
    -- Pase a command using `$` identifier
    parseCommand = do 
      _ <- char ('$')
      Command <$> takeText
    -- Parse a mapping placeholder and append `~` in the begining for easy identification
    parseMapping = do
      _ <- char ('@')
      Mapping <$> takeText
    -- Parse a constant value
    parseConstant = do
      Constant <$> takeText


testParse :: Text -> Either String PlaceHolder
testParse = eitherDecodeStrict . encodeUtf8 

loadSessionTemplate :: ByteString -> Either String SessionTemplate
loadSessionTemplate = eitherDecodeStrict


generateNewSession :: SessionTemplate -> IO NormalisedSession
generateNewSession template = do
  normalisedPlaceholder <- getNormalisedPlaceholder
  let nomalisedApi = getNormalisedApi normalisedPlaceholder
  pure $ NormalisedSession
      { normalisedPlaceholder = normalisedPlaceholder
      , generatedApiData = nomalisedApi
      }
  where 
    placeHolderMap = placeholder template
    apiData = api template
    getNormalisedPlaceholder = mapM normaliseCommand placeHolderMap
    getNormalisedApi normalisedPlaceholders = HMap.map (normaliseApiData False normalisedPlaceholders) apiData

normaliseApiData :: Bool -> HMap.HashMap Text PlaceHolder ->  ApiTemplate -> ApiTemplate
normaliseApiData failOnMappingPlaceholder placeholders apiTemplate = apiTemplate {headers = normalisedHeader , request = normalisedRequest}
  where
    normalisedHeader = HMap.map fillConstants (headers apiTemplate)
    normalisedRequest = HMap.map fillConstants (request apiTemplate)

    fillConstants val = fromMaybe val $ do 
      placeholderLabel <- getPlaceholder val
      let placeHolderValue = HMap.lookup placeholderLabel placeholders
      case placeHolderValue of
        Just (Constant value) -> pure value
        Just (Command value)  -> error $ "Unexpected happed : " <> (Text.unpack value) <>  " Command was not normalised"
        Just (Mapping value)  -> if failOnMappingPlaceholder 
                                    then error $ "Found unresolved mapping placeholder " <> (Text.unpack value) 
                                    else Nothing
        Nothing -> error $ (Text.unpack placeholderLabel) <> " : not present"

    getPlaceholder :: Text -> Maybe Text
    getPlaceholder "" = Nothing
    getPlaceholder a = if (Text.head a == '#') then Just $ Text.tail a else Nothing



normaliseCommand :: PlaceHolder -> IO PlaceHolder
normaliseCommand (Command a) = Constant <$> runCommand a 
normaliseCommand a = pure a 

runCommand :: Text -> IO Text
runCommand = \case
  "randomInt" -> Text.pack . show <$> randomInt
  -- TODO: add logic for random string
  "randomId" -> pure "shubhanshu"
  a -> error $ (Text.unpack a) <> " is not a valid command"


-- Word8 is used to keep the output small
randomInt :: IO Word8
randomInt = makeNatual <$> randomIO
  where
    makeNatual a | a < 0 = (a * (-1)) `mod` 10000
                 | a == 0 = 1 
                 | otherwise = a `mod` 10000   