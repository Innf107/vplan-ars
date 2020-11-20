{-#LANGUAGE OverloadedStrings#-}
module Scraper where

import Lib
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 as BLC
import Types
import Text.HTML.Scalpel as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Data.List as L
import Data.Foldable as F
import Control.Applicative


managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
        HTTP.managerModifyRequest = return . HTTP.applyBasicAuth "vplan" "ars2013"
        }



scrapePage :: Int -> IO (Maybe (VPlan, Bool))
scrapePage i = do
    manager <- HTTP.newManager managerSettings
    scrapeURLWithConfig (Config {decoder=S.defaultDecoder,manager=Just manager})
        ("https://vplan.ars-hochtaunus.de/subst_" ++ (padLeft '0' 3 (show i)) ++ ".htm") vplanWithLastPage
    where
        vplanWithLastPage :: Scraper String (VPlan, Bool)
        vplanWithLastPage = (,) <$> vplan <*> ((=="12; URL=subst_001.htm") <$> attr "content" ("meta" @: ["http-equiv" @= "refresh"]))

        vplan :: Scraper String VPlan
        vplan = (VPlan . pure) <$> day

        day :: Scraper String Day
        day = Day <$> date <*> motd <*> klassen

        date :: Scraper String String
        date = fmap cleanDate $ chroot ("center" @: []) $ text ("div" @: [S.hasClass "mon_title"])

        cleanDate :: String -> String
        cleanDate [] = []
        cleanDate (c:cs)
            | c `elem` ("AB" :: String) = [c]
            | otherwise = c:(cleanDate cs)

--      TODO: Use affected?
        motd :: Scraper String MOTD
        motd = MOTD <$> return [] <*> fmap tail (chroot ("table" @: [hasClass "info"]) 
                                                    (texts ("tr" @: [hasClass "info"])))

        klassen :: Scraper String [Klasse]
        klassen =  do
            klasseData <- chroots ("tr" @: [S.hasClass "list"]) (klasseName <|> klasseHour)
            return $ parseKlasseData klasseData

        parseKlasseData :: [KlasseData] -> [Klasse]
        parseKlasseData [] = []
        parseKlasseData ((KlasseName n):ks) = parseKlasseData' n [] ks
        parseKlasseData (_:ks) = parseKlasseData ks

        parseKlasseData' :: String -> [Hour] -> [KlasseData] -> [Klasse]
        parseKlasseData' cname chours [] = [Klasse cname chours]
        parseKlasseData' cname chours ((KlasseName n):ks) = (Klasse cname chours): parseKlasseData' n [] ks
        parseKlasseData' cname chours ((KlasseHour h):ks) = parseKlasseData' cname (h:chours) ks

        klasseName :: Scraper String KlasseData
        klasseName = fmap KlasseName $ text ("td" @: [S.hasClass "list", S.hasClass "inline_header"])

        klasseHour :: Scraper String KlasseData
        klasseHour = KlasseHour <$> hour

        hour = (\(s:v:f:r:vt:_) -> Hour s v f r vt) <$> innerHTMLs ("td" @: [hasClass "list"])

data KlasseData = KlasseName String
                | KlasseHour Hour
                deriving (Show, Eq)

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
