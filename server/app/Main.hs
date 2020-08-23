{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Lib
import Types
import Scraper
import XCSS
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Data.List as L
import Data.Foldable as F
import Control.Applicative
import Data.Maybe
import Web.Scotty as S
import Control.Concurrent
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import System.IO
import GHC.Generics

data Config = Config {
        updateRate:: Double,
        port::Int
    } 
    deriving (Show, Eq, Generic, A.FromJSON, A.ToJSON)

defaultConfig = Config {
    updateRate=60,
    port=5000
}

main :: IO ()
main = do
    mconfig <- A.eitherDecodeFileStrict "config.json"
    config <- case mconfig of
        Right c -> return c
        Left e -> hPutStrLn stderr ("Cannot read config file 'config.json'. Reverting to default: " ++ show e) >> return defaultConfig
    vp <- scrapeAll
    vpVar <- newMVar vp
    forkIO $ updateVPlan config vpVar
    scotty (port config) $ do
        S.get "/ping" $ S.text "\"true\""
        S.get "/pro" $ file "../public/index.html"
        S.get "/main.css" $ xcss "../public/main.css"
        S.get "/select.css" $ xcss "../public/select.css"
        S.get "/teacher.css" $ xcss "../public/teacher.css"
        S.get "/" $ file "../public/index.html"
        S.get "/robots.txt" $ file "../robots.txt"
        S.get "/index.js" $ file "../public/index.js"
        S.get "/teacher" $ file "../public/teacher.html"
        S.get "/teacher.js" $ file "../public/teacher.js"
        S.get "/personal" $ file "../public/personal.html"
        S.get "/personal.js" $ file "../public/personal.js"
        S.get "/personal.css" $ xcss "../public/personal.css"
        S.get "/sw.js" $ file "../public/sw.js"
        S.get "/manifest.webmanifest" $ file "../manifest.webmanifest"
        S.get "/logoMain.png" $ file "../logoMain.png"
        S.get "/usersTotal" $ text "NYI"
        S.get "/json/kuerzel" $ do
            f <- liftIO $ BLC.readFile "../kuerzel.json"
            let ks :: Maybe [(String, String)]
                ks = M.toList <$> (A.decode f)
            case ks of
                Nothing -> raiseStatus (toEnum 500) "Invalid 'kuerzel.json'"
                Just ks -> json $ (map (\(k, v) -> Kuerzel k v)) ks
        S.get "/json" $ setHeader "Access-Control-Allow-Origin" "*" >> liftIO (readMVar vpVar) >>= json
        S.put "/feedback" $ text "NYI"
        notFound $ file "../public/404.html"

updateVPlan :: Config -> MVar VPlan -> IO ()
updateVPlan config vpVar = do
            v <- scrapeAll `catch` scrapeHandler
            takeMVar vpVar
            putMVar vpVar v
            threadDelay $ floor $ 1e6 * updateRate config
            updateVPlan config vpVar
            where
                scrapeHandler :: SomeException -> IO VPlan
                scrapeHandler e = hPutStrLn stderr ("Error scraping vplan: " ++ show e) >> readMVar vpVar


