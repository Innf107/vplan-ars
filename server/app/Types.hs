{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveAnyClass#-}
{-#LANGUAGE OverloadedStrings#-}
module Types where

import Lib
import Data.Char
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Aeson.Types (Parser)
import Data.List as L

--TODO: Keep Strikethrough

newtype VPlan = VPlan {vplan::[Day]} deriving (Show, Eq, Generic)

instance ToJSON VPlan where
    toJSON (VPlan days) =
        object ["vplan" .= object (map (\(Day d m k) -> (T.pack d) .= Day d m k) days)]

instance FromJSON VPlan where
    parseJSON = withObject "VPlan" $ \v -> (VPlan . map snd . (M.toList))
        <$> ((v .: "vplan") :: Parser (M.Map String Day))


data Day = Day {day::String, motd::MOTD, klassen::[Klasse]}
    deriving (Show, Eq, Generic, FromJSON)
    
instance ToJSON Day where
    toJSON (Day day motd klassen) =
        object ["day" .= day, "motd" .= motd, 
        "klassen" .= object (map (\(Klasse n hs) -> T.pack n .= (Klasse n hs)) klassen)] 

data Klasse = Klasse {
        name :: String,
        hours :: [Hour]
    } deriving (Show, Eq, Generic, FromJSON)

instance ToJSON Klasse where
    toJSON (Klasse n hs) = object ["name" .= n, "hours" .= (sort hs)]

data Hour = Hour {
        stunde :: String,
        vertreter :: String,
        fach :: String,
        raum :: String,
        vtext :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Ord Hour where
--TODO: 6-7
    (Hour {stunde=s1}) `compare` (Hour {stunde=s2}) = (padLeft '0' 2 (takeWhile isDigit s1))`compare`
                                                      (padLeft '0' 2 (takeWhile isDigit s2))


data MOTD = MOTD {
        affected :: [String],
        content :: [String]
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
    
    
instance Semigroup VPlan where
    (VPlan ds1) <> (VPlan ds2) = VPlan (mergeDays $ ds1 ++ ds2)
        where
            mergeDays :: [Day] -> [Day]
            mergeDays ds = map mconcat $ L.groupBy (\(Day d _ _) (Day d2 _ _)-> d == d2) ds 
    
instance Monoid VPlan where
    mempty = VPlan []
    
    
instance Semigroup Day where 
    (Day d1 m1 ks1) <> (Day _ m2 ks2) = Day d1 (m1 <> m2) (mergeKlassen ks1 ks2)
        where
            mergeKlassen = (++)
            
            
            
instance Monoid Day where
    mempty = Day "" mempty []
            
instance Semigroup MOTD where
    (MOTD a1 c1) <> (MOTD a2 c2) = MOTD (a1 <> a2) (c1 <> c2)
    
instance Monoid MOTD where
    mempty = MOTD [] []
    