{-#LANGUAGE OverloadedStrings#-}
module Main where

import Lib
import Types
import Scraper
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


port :: Int
port = 5000

updateRate :: Second
updateRate = 30

type Second = Double

main :: IO ()
main = do
    vp <- scrapeAll
    vpVar <- newMVar vp
    forkIO $ updateVPlan vpVar
    scotty port $ do
        S.get "/ping" $ S.text "\"true\""
        S.get "/pro" $ file "../public/index.html"
        S.get "/main.css" $ file "../public/main.css"
        S.get "/select.css" $ file "../public/select.css"
        S.get "/teacher.css" $ file "../public/teacher.css"
        S.get "/" $ file "../public/select.html"
        S.get "/robots.txt" $ file "../robots.txt"
        S.get "/index.js" $ file "../public/index.js"
        S.get "/teacher" $ file "../public/teacher.html"
        S.get "/teacher.js" $ file "../public/teacher.js"
        S.get "/personal" $ file "../public/personal.html"
        S.get "/personal.js" $ file "../public/personal.js"
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
        S.get "/json" $ liftIO (readMVar vpVar) >>= json
        S.put "/feedback" $ text "NYI"
        S.get "/*" $ file "../404.html"

updateVPlan :: MVar VPlan -> IO ()
updateVPlan vpVar = do
        let loop = do
                v <- scrapeAll
                takeMVar vpVar
                putMVar vpVar v
                threadDelay $ floor $ 1e6 * updateRate
                loop
        loop


scrapeAll :: IO VPlan
scrapeAll = mconcat <$> scrapeAll' 1
    where
        scrapeAll' :: Int -> IO [VPlan]
        scrapeAll' i = do
            mvp <- scrapePage i
            case mvp of 
                Nothing -> return []
                Just (x, lastPage) -> case lastPage of
                    True -> return [x]
                    False -> do
                        mvps <- scrapeAll' (i + 1)
                        return $ x:mvps

