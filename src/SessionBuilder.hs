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
  , ConversionError(..)
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
import GHC.Stack(HasCallStack)
import qualified Data.UUID as UUID
import qualified Control.Exception as Ex

data SessionTemplate = 
    SessionTemplate
      { placeholder :: HMap.HashMap Text PlaceHolder
      , api :: HMap.HashMap Text ApiTemplate
      , apiOrder :: [Text]
      }
    deriving (Generic, FromJSON,ToJSON, Show)

data NormalisedSession = 
    NormalisedSession
      { normalisedPlaceholder :: HMap.HashMap Text PlaceHolder
      , generatedApiData :: [(Text,ApiTemplate)]
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


generateNewSession :: HasCallStack => SessionTemplate -> IO NormalisedSession
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
    apiOrdering = apiOrder template
    getNormalisedPlaceholder = mapM normaliseCommand placeHolderMap
    getNormalisedApi normalisedPlaceholders = (runNormaliseApiData normalisedPlaceholders) <$> orderedApiData

    orderedApiData = (\apiLabel -> (apiLabel,fromJust $ HMap.lookup apiLabel apiData)) <$> apiOrdering

data ConversionError = 
    NotAPlaceholder
  | CommandNotNormalised Text
  | MapperFound Text
  | PlaceholderNotFound Text
  | HttpException Ex.SomeException
  deriving (Show)

runNormaliseApiData :: HasCallStack => HMap.HashMap Text PlaceHolder ->  (Text,ApiTemplate) -> (Text,ApiTemplate)
runNormaliseApiData placeholders (apiLabel,apiTemplate) = 
  let normalisedData = normaliseApiData placeholders (apiLabel,apiTemplate)
  in resolveEither normalisedData
  where
    resolveEither :: Either ConversionError (Text,ApiTemplate) -> (Text,ApiTemplate)
    resolveEither eitherVal = 
      case eitherVal of
        Right val -> val
        Left NotAPlaceholder -> (apiLabel,apiTemplate)
        Left (CommandNotNormalised err) -> error $ Text.unpack err
        Left (MapperFound _) -> (apiLabel,apiTemplate)
        Left (PlaceholderNotFound err) -> error $ Text.unpack err
        Left (HttpException err) -> error $ show err

normaliseApiData :: HMap.HashMap Text PlaceHolder ->  (Text,ApiTemplate) -> Either ConversionError (Text,ApiTemplate)
normaliseApiData placeholders (apiLabel,apiTemplate) = do 
  normalisedHeader <- mapM fillConstants (headers apiTemplate)
  normalisedRequest <- mapM fillConstants (request apiTemplate)
  normalisedUrl <- fillConstants (endpoint apiTemplate)
  pure $ (apiLabel, apiTemplate {headers = normalisedHeader , request = normalisedRequest, endpoint = normalisedUrl})
  where
    fillConstants :: Text -> Either ConversionError Text
    fillConstants val = resolveNotAPlaceholder val $ do --fromMaybe val $ do 
      placeholderLabel <- getPlaceholder val
      let placeHolderValue = HMap.lookup placeholderLabel placeholders
      case placeHolderValue of
        Just (Constant value) -> pure value
        Just (Command value)  -> Left $ CommandNotNormalised $ "Unexpected happed : " <> value <>  " Command was not normalised"
        Just (Mapping value)  -> Left $ MapperFound $ "Found unresolved mapping placeholder " <> value
                                  -- if failOnMappingPlaceholder 
                                  --   then error $ "Found unresolved mapping placeholder " <> (Text.unpack value) 
                                  --   else Nothing
        Nothing -> Left $ PlaceholderNotFound $ placeholderLabel <> " : not present"

    getPlaceholder :: Text -> Either ConversionError Text
    getPlaceholder "" = Left $ NotAPlaceholder
    getPlaceholder a = if (Text.head a == '#') then Right $ Text.tail a else Left $ NotAPlaceholder

    resolveNotAPlaceholder :: Text ->  Either ConversionError Text -> Either ConversionError Text
    resolveNotAPlaceholder defValue eitherVal = 
      case eitherVal of
        (Left NotAPlaceholder) -> Right defValue
        a -> a



normaliseCommand :: PlaceHolder -> IO PlaceHolder
normaliseCommand (Command a) = Constant <$> runCommand a 
normaliseCommand a = pure a 

runCommand :: Text -> IO Text
runCommand = \case
  "randomInt" -> Text.pack . show <$> randomInt
  "randomId" -> do
    uuid <- UUID.toText <$> randomUUID
    pure $ Text.take 6 $ Text.filter (/='-') uuid

  a -> error $ (Text.unpack a) <> " is not a valid command"


-- Word8 is used to keep the output small
randomInt :: IO Word8
randomInt = makeNatual <$> randomIO
  where
    makeNatual a | a < 0 = (a * (-1)) `mod` 10000
                 | a == 0 = 1 
                 | otherwise = a `mod` 10000   

randomUUID :: IO UUID.UUID
randomUUID = randomIO