{-#LANGUAGE OverloadedStrings#-}
module XCSS where

import Web.Scotty as S
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Text.Regex.TDFA as RT

xcss :: FilePath -> ActionM ()
xcss p = do
    userAgent <- (fromMaybe "") <$> S.header ("user-agent")
    let mobile :: Bool
        mobile = (TL.unpack userAgent) =~ ("(Android)|(iPhone)" :: String)
    f <- liftIO $ BLU.toString <$> BLC.readFile p
    setHeader "content-type" "text/css"
    raw $ BLU.fromString (xcssStr mobile f)

xcssStr :: Bool -> String -> String
xcssStr mobile s = case ((s =~ regex) :: (String, String, String)) of
    (s, "", "") -> s
    (s, m, e) -> xcssStr mobile $ s ++ process mobile m ++ e
    where
        regex = ("[^:+]+\\+[^;\n]+"::String)
        process :: Bool -> String -> String
        process False s  = fst $ break (=='+') s
        process True s = drop 1 $ snd $ break (=='+') s

