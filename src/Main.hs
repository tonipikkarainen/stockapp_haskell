{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}


module Main where
import Web.Spock.Lucid (lucid)
import Lucid
import Web.Spock
import Web.Spock.Config
import Data.Text (Text, unpack,pack)
import Data.IORef
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Data.Aeson
import GHC.Generics
import Data.Maybe

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

tags :: [Text]
tags=["roe", "marketcap","bookvaluepershare","ebitdagrowth","ebitgrowth","epsgrowth","fcffgrowth","netincomegrowth",
     "close_price"]  

-- url_comp+"/"+company+"/historical_data/"+data_tag+"?frequency=yearly&start_date=2007-01-01&end_date="+str(today.year)+"-01-01&api_key="+api_key)
-- https://api-v2.intrinio.com/companies/CVX/historical_data/roe/?frequency=yearly&start_date=2007-01-01&end_date=2020-01-01&api_key=OjRkYTY2MzQ0Njg2MjAwM2IwNDdjZGJlZDdlMjc4OWI5
api_key :: Text
api_key = "OjRkYTY2MzQ0Njg2MjAwM2IwNDdjZGJlZDdlMjc4OWI5"

api_ref :: Text
api_ref = "api_key="

url_comp :: Text
url_comp = "https://api-v2.intrinio.com/companies"


-- Exception handling for bad urlS!!!!
getCOMP :: IO B.ByteString
getCOMP = simpleHttp $ unpack $ url_comp <> "?"<> api_ref <> api_key 

getHistorical :: Text -> Text ->  IO B.ByteString
getHistorical comp tag =  simpleHttp $ unpack $ url_comp <> "/"<>comp <> "/historical_data/" <> tag <> 
                      "?frequency=yearly&start_date=2007-01-01&end_date=2020-01-01&api_key="<> api_key

data Company =  Company {
                       ticker :: Maybe Text,
                       name ::  Maybe Text
                       } deriving (Show, Generic)

data Companies = Companies {companies :: [Company]} 
            deriving (Show,Generic)

instance FromJSON Company
instance ToJSON Company

instance FromJSON Companies
instance ToJSON Companies

data DateValue = DateValue {
            date :: Maybe Text, 
            value :: Maybe Double
            } deriving (Show,Generic)

data Historical = Historical {
                      historical_data :: [DateValue],
                      company :: Company
                      } deriving (Show,Generic)

instance FromJSON DateValue
instance ToJSON DateValue

instance FromJSON Historical
instance ToJSON Historical


--get_ticker :: Text -> 


--data Note = Note {author :: Text, contents :: Text}

--newtype ServerState = ServerState {notes :: IORef [Note]}

-- The definition of a Spock application lives in the SpockM conn sess st a monad
-- You can think of it as a Writer monad
-- To connect an URL to an action, we use routes
type Server a = SpockM () () () a 

-- type of get and post etc..
--type RouteSpec xs ps ctx conn sess st = 
-- Path xs ps -> 
--     HVectElim xs (SpockActionCtx ctx conn sess st ()) -> 
--        SpockCtxM ctx conn sess st ()

--html_add :: Html () -> Html ()
--html_add = 

page :: Html ()
page =
  html_
    (do head_
          (do title_ "Introduction page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background:red}")
        body_
          (do div_ [id_ "header",style_ "color:white"] (a_ [href_ "/"] "Stock analyzer")
              p_ (span_ (strong_ "This is a company analysing tool."))
              hr_ []
              ))

app :: Server ()
app = do
  get root $ do
    -- Getting companies from api
    dat <- liftIO ((eitherDecode <$> getCOMP) :: IO (Either String Companies))
    case dat of 
      Left e -> lucid $ do
                  h1_ "Ei löydy"
      Right d -> lucid $  do 
                page
                h1_ "Companies:"
                form_ [method_ "post"] $ do
                    label_ $ do
                      "Choose company: "
                      select_ [name_ "company"] $ forM_ (companies d) $ \item -> li_ $   
                              do
                               case name item of
                                 Just x -> option_ [value_ (fromJust (ticker item))] (toHtml (x))
                                 Nothing -> option_ [value_ ""] ( toHtml ("tyhjä" :: Text))
                    input_ [type_ "submit", value_ "Choose"]
  post root $ do
    a <- param' "company"
    dat <- liftIO ((eitherDecode <$> getHistorical a "roe") :: IO (Either String Historical))
    case dat of 
      Left e -> lucid $ do
                  h1_ "Ei löydy dataa"
      Right d -> lucid $  do 
                page
                h1_ (toHtml (a :: Text))
                ul_ $ forM_ (historical_data d)  $ \item -> li_ $ do
                    case (value item) of 
                      Just x -> do 
                                toHtml (fromJust (date item))
                                ": "
                                toHtml (pack $ show $ x)
                      Nothing -> toHtml ("No value" :: Text) -- Plotting values!?!!?
                    
main :: IO ()
main = do
  -- serverstate menee IO:n sisälle:
  --st <- ServerState <$> newIORef [Note "t" "1", Note "b" "2"]
  -- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
  cfg <- defaultSpockCfg () PCNoDatabase ()
  -- spock :: forall conn sess st. SpockCfg conn sess st -> SpockM conn sess st () -> IO Middleware
  -- runSpock :: Port -> IO Middleware -> IO ()
  runSpock 8080 (spock cfg app)




{- {"historical_data":[{"date":"2018-12-31","value":0.097455},{"date":"2017-12-31","value":0.06262},
{"date":"2016-12-31","value":-0.002868},{"date":"2015-12-31","value":0.03038},
{"date":"2014-12-31","value":0.125955},{"date":"2013-12-31","value":0.149844},
{"date":"2012-12-31","value":0.202574},{"date":"2011-12-31","value":0.236921},
{"date":"2010-12-31","value":0.19293},{"date":"2009-12-31","value":0.117577},
{"date":"2008-12-31","value":null},{"date":"2007-12-31","value":null}],"company":
{"id":"com_DzonXe","ticker":"CVX","name":"Chevron Corp","lei":null,"cik":"0000093410"},"next_page":null} -}