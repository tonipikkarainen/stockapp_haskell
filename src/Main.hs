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
import Data.Aeson -- hiding --('.=') 
import GHC.Generics
import Data.Maybe

import Data.Time
import Control.Exception.Base

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Web.Spock.Action
import Network.Wai.Middleware.Static

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

--TODO: 
-- * plot (ok), state (ok), exception (ok), wiser json download

-- Exception handling for bad urlS!!!!
getCOMP :: IO B.ByteString
getCOMP = simpleHttp $ unpack $ url_comp <> "?"<> api_ref <> api_key 

getHistorical :: Text -> Text ->  IO B.ByteString
getHistorical comp tag =  simpleHttp $ unpack $ url_comp <> "/"<>comp <> "/historical_data/" <> tag <> 
                      "?frequency=quarterly&start_date=2007-01-01&end_date=2020-01-01&api_key="<> api_key

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

type Server a = SpockM () () Companies a 

page :: Html ()
page =
  html_
    (do head_
          (do title_ "Introduction page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background-color:#cfd4d1}")
        body_
          (do div_ [id_ "header",style_ "color:white"] (a_ [href_ "/"] "Stock analyzer")
              p_ (span_ (strong_ "This is a company analysing tool."))
              hr_ []
              ))

app :: Server ()
app = do
  middleware $ staticPolicy $ addBase "static"
  get root $ do
    st <- companies <$> getState 
    lucid $  do 
          page
          h1_ "Companies:"
          form_ [method_ "post"] $ do
              label_ $ do
                "Choose company and datapoints to plot: "
                select_ [name_ "company"] $ forM_ st $ \item -> li_ $   
                        do
                         case name item of
                           Just x -> option_ [value_ (fromJust (ticker item))] (toHtml (x))
                           Nothing -> option_ [value_ ""] ( toHtml ("tyhjä" :: Text))
                select_ [name_ "tag"] $ forM_ tags $ \item -> li_ $   
                        do 
                           option_ [value_ item] ( toHtml item)
              input_ [type_ "submit", value_ "Choose"]
  
  post root $ do
    comp <- param' "company"
    tag <- param' "tag"
    --let tag = (pack "close_price")
    dat <- liftIO $ catch(((eitherDecode <$> getHistorical comp tag) :: IO (Either String Historical))) 
          $ \e  -> seq (e :: SomeException) (return (Left "väärä polku")) 
    case dat of 
      Left e -> lucid $ do
                  page
                  h1_ "Ei löydy dataa"
      Right d -> do 
        liftIO $ plot_test comp tag
        lucid $  do 
                page            
                h1_ (toHtml (comp :: Text))
                img_ [src_ "plot.png", height_ "400", width_ "400"]
                ul_ $ forM_ (historical_data d)  $ \item -> li_ $ do
                    case (value item) of 
                      Just x -> do 
                                toHtml (fromJust (date item))
                                ": "
                                toHtml (pack $ show $ x)
                      Nothing -> toHtml ("No value" :: Text)
                    
main :: IO ()
main = do
  catch (do
    st <- ((eitherDecode <$> getCOMP) :: IO (Either String Companies))
    case st of 
      Left e ->  do
        cfg <- defaultSpockCfg () PCNoDatabase (Companies [])
        runSpock 8080 (spock cfg app)
      Right st -> do 
        cfg <- defaultSpockCfg () PCNoDatabase st
        runSpock 8080 (spock cfg app)) $ \e -> seq (e :: SomeException) (putStrLn "Väärä url.")
  
  -- serverstate menee IO:n sisälle:
  --st <- ServerState <$> newIORef [Note "t" "1", Note "b" "2"]
  -- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
  --cfg <- defaultSpockCfg () PCNoDatabase st
  -- spock :: forall conn sess st. SpockCfg conn sess st -> SpockM conn sess st () -> IO Middleware
  -- runSpock :: Port -> IO Middleware -> IO ()
  --runSpock 8080 (spock cfg app)


-- This works for all historical data.
hist_test :: Historical -> [(UTCTime, Double)]
hist_test xs =  f (historical_data xs) where
  f [] = []
  f (y:ys) = case date y of 
    Just d -> case value y of 
      Just v -> (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (unpack d) :: UTCTime,v): (f ys)
      Nothing -> f ys
    Nothing -> f ys

-- 2018-12-31
plot_test :: Text -> Text -> IO ()
plot_test comp tag= do
  dat <- ((eitherDecode <$> getHistorical comp tag) :: IO (Either String Historical))
  toFile def "static/plot.png" $ do
    layout_title Graphics.Rendering.Chart.Easy..= (unpack comp) ++ " " ++ (unpack tag)
    case dat of 
      Left e -> do 
        setColors [opaque blue, opaque red]
        plot (line "am" [])
      Right st -> do 
        setColors [opaque blue, opaque red]
        plot (line "" [(hist_test st)])
  
