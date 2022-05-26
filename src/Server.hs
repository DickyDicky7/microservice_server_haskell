module Server
    ( start
    ) where

import qualified Control.Concurrent.Async      as Async
import qualified Data.Aeson                    as JSON
import qualified Data.Typeable                 as Typeable
import qualified Network.Wai.Handler.Warp      as HTTP
import qualified Network.Wai.Logger            as HTTP
import qualified Network.Wai.Middleware.Servant.Errors
                                               as Middleware
import           Servant
import           Universum
import qualified Universum.Unsafe              as Unsafe

data Response result = Response
    { status  :: Int
    , message :: Text
    , result  :: Maybe result
    }
    deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

data CustomError a
    deriving Accept via JSON

instance Middleware.HasErrorBody (CustomError JSON) '["status" , "message" , "result"] where
    encodeError Middleware.StatusCode {..} Middleware.ErrorMsg {..} =
        JSON.encode @(Response ()) (Response unStatusCode unErrorMsg Nothing)

data OhShit = OhShit
    deriving (Eq, Show)

onError :: SomeException -> Handler a
onError error = throwError err500 { errBody = encodeUtf8 (displayException error) }

instance Exception OhShit where
    displayException _ = "Oh shitz!"

type API_1 = "api1" :> Post '[JSON] (Response ())

server_1 :: Server API_1
server_1 =
    do
        -- throwM OhShit
        throwError err418 { errBody = "This is err 300", errHeaders = [("hihi", "haha")] }
        pure (Response 200 "API_1 was requested" Nothing)
    `catch` onError


app_1 :: Application
app_1 = serve (Proxy @API_1) server_1

type API_2 = "api2" :> Get '[JSON] (Response ())

server_2 :: Server API_2
server_2 = pure (Response 200 "API_2 was requested" Nothing)

app_2 :: Application
app_2 = serve (Proxy @API_2) server_2

microServices :: Vector (HTTP.Port, Application)
microServices = [(3000, app_1), (8000, app_2)]

startMicroService :: (HTTP.Port, Application) -> IO ()
startMicroService (port, app) = HTTP.withStdoutLogger \logger -> do
    let settings = HTTP.defaultSettings & HTTP.setPort port & HTTP.setLogger logger
    HTTP.runSettings settings (Middleware.errorMw @(CustomError JSON) @'["status" , "message" , "result"] app)

start :: IO ()
start = microServices `Async.forConcurrently_` startMicroService
