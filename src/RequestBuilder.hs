module RequestBuilder 
  (buildRequest
  )
  where

import Prelude
import qualified SessionBuilder as SB
import qualified Network.HTTP.Client as Client
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as ClientTypes
import Data.Text.Encoding (encodeUtf8)
import qualified Data.CaseInsensitive as CI
import Data.Aeson(encode)
import Data.Maybe (fromMaybe)
import GHC.Stack(HasCallStack)


-- TODO: use placeholder
buildRequest :: HasCallStack => HMap.HashMap Text SB.PlaceHolder -> (Text,SB.ApiTemplate) -> IO Client.Request
buildRequest placeholders (apiLabel,apiTemplate)  = do
  baseRequest <- Client.parseRequest $ Text.unpack $  SB.endpoint normalApiTemplate
  let newRequest = baseRequest { Client.method = fromReqMethod reqMethod , Client.requestHeaders = fromHeaders reqHeader}
  pure $ addBodyToRequest reqContentType reqBody newRequest
  where
    reqMethod = SB.method normalApiTemplate
    reqHeader = SB.headers normalApiTemplate
    reqContentType = SB.contentType normalApiTemplate
    reqBody = SB.request normalApiTemplate
    normalApiTemplate = snd $ SB.normaliseApiData True placeholders (apiLabel,apiTemplate)


fromReqMethod :: SB.ReqMethod -> ClientTypes.Method
fromReqMethod SB.POST = ClientTypes.methodPost
fromReqMethod SB.GET  = ClientTypes.methodGet

fromHeaders :: HMap.HashMap Text Text -> ClientTypes.RequestHeaders
fromHeaders headerMap = mkHeaders <$> (HMap.toList headerMap)
  where
    mkHeaders (headerLabel, headerValue) = (CI.mk (encodeUtf8 headerLabel), (encodeUtf8 headerValue))

addBodyToRequest :: Maybe SB.ContentType -> HMap.HashMap Text Text -> Client.Request -> Client.Request
addBodyToRequest mbContentType bodyMap request = fromMaybe request $ do 
  contentType <- mbContentType
  pure $ case contentType of 
    SB.URLFORMENCODED -> Client.urlEncodedBody mkFormEncoded request
    SB.JSON -> 
      request 
        { Client.requestHeaders = (jsonContentType : filter (\(x, _) -> x /= ClientTypes.hContentType) (Client.requestHeaders request))
        , Client.requestBody = Client.RequestBodyLBS $ encode bodyMap
        }
  where 
    mkFormEncoded = (\(a,b) -> (encodeUtf8 a, encodeUtf8 b)) <$> HMap.toList bodyMap
    jsonContentType = (ClientTypes.hContentType , "application/json")