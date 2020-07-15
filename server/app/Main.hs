{-#LANGUAGE OverloadedStrings#-}
module Main where

import Lib
import Types
import Scraper
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.HTML.Scalpel as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Data.List as L
import Data.Foldable as F
import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
    vplan <- scrapeAll
    BLC.putStrLn $ A.encode vplan

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

