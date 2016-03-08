module Afftrack.API
    ( module Afftrack.API.AdminManager
    , module Afftrack.API.Affiliate
    , module Afftrack.API.Merchant
    , module Afftrack.API.Offers
    , module Afftrack.API.Report
    , module Afftrack.API.Settings
    ) where

import Afftrack.API.AdminManager
import Afftrack.API.Affiliate
import Afftrack.API.Merchant
import Afftrack.API.Offers
import Afftrack.API.Report
import Afftrack.API.Settings

import Data.Text

-----------------------------------------------------------------------------

-- | Core parameters that builds up
--   auth url to acces methods
-- 
--   endpoint = "http://tracking.affiliate.net/apiv4/"
--   key      = "9bb137a344cdec76c5830a0ef6d2e38e"
--   api      =  endpoint ++ "?key=" ++ key ++ "&format=json"
data Auth =
  Auth { endpoint :: Text
       , key      :: Text
       , format   :: Text  
       } deriving (Show)
