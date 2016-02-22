{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Offers
       ( Offer(..)
       , addBrowserLanguageBlocked  
       , getConvertsOn  
       , getBrowserLanguages
       , getCreativeCounts
       , getCreatives  
       , getOffer
       , getOfferBlacklist
       , getOfferState  
       , getOfferStatus
       , getOfferBrowserLanguageAllowed
       , getOfferBrowserLanguageBlocked
       , getOfferCategories
       , getOfferCategory
       , getOfferCount
       , getOfferCountry
       , getOfferCustomAffiliateCap
       , getOfferCustomAffiliatePayout
       , getOfferDeviceType
       , getOfferPrivate
       , getOfferSchedule
       , getOfferTargeting
       , getOfferTrafficType
       , getOfferTypes
       , getPixelTypes
       , getTestLink
       , getTrackingTypes
       , getTrafficTypes
       , removeOfferBlacklist
       , removeOfferBrowserLanguageAllowed
       , removeOfferBrowserLanguageBlocked
       , removeOfferCountry
       , removeOfferCustomAffiliateCAP
       , removeOfferCustomAffiliatePayout
       , removeOfferDeviceType
       , removeOfferOptimization
       , removeOfferPrivate
       , removeOfferSchedule
       , removeOfferScheduleRate
       , removeOfferState
       , removeOfferTrafficType
       , updateOffer
       , updateOfferSchedule
       , updateTrackingLink  
       )where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS

import Afftrack.API.Common

--------------------------------------------------------------------------------

data Resp =
  Resp { datas   :: [Offer]
       , success :: Bool
       , page    :: Int
       , total   :: Int
       , url     :: String
       , pages   :: Int
       , limit   :: Int  
       } deriving (Generic, Show)

instance FromJSON Resp where
  parseJSON (Object v) =
    Resp <$> v .: "data"       <*>
             v .: "success"    <*>
             v .: "page"       <*>
             v .: "total"      <*>
             v .: "request_url"<*>
             v .: "total_pages"<*>
             v .: "limit"
    -- A non-Object value is of the wrong type, so fail.
  parseJSON _        = empty  

data TrafficType = INCENT | NONINCENT

data Offer =
  Offer { name       :: String
        , link       :: String
        , linkStatus :: Int
        , payout     :: Float
        , merchantID :: Int  
        } deriving (Generic, Show)

instance FromJSON Offer where
  parseJSON (Object v) =
    Offer <$> v .: "program_name"         <*>
              v .: "program_preview_link" <*>
              v .: "program_link_status"  <*>
              v .: "program_adv_paying"   <*>
              v .: "program_mid"
    -- A non-Object value is of the wrong type, so fail.
  parseJSON _        = empty

addBrowserLanguageBlocked = 
  Call "offer_offer"
       "addBrowserLanguageBlocked"
       "GET"
       [ ("language", "")   -- Required
       , ("offer_id", "")   -- Required
       ]  

getBrowserLanguages =
  Call "offer_offer"
       "getBrowserLanguages"
       "GET"
       []

getConvertsOn =
  Call "offer_offer"
       "getConvertsOn"
       "GET"
       []
       
getCreativeCounts =
  Call "offer_offer"
       "getCreativeCounts"
       "GET"
       [ ("offer_id", "")] -- Required  

getCreatives =
  Call "offer_offer"
       "getCreatives"
       "GET"
       [ ("offer_id", "")] -- Required  
  
getDeviceTypes =
  Call "offer_offer"
       "getDeviceTypes"
       "GET"
       []  

getOffer =
  Call "offer_offer"
       "getOffer"
       "GET"
       [ ("category", "")
       , ("converts_on", "")
       , ("device_type", "")
       , ("limit", "")
       , ("merchant_id", "") -- empty
       , ("name", "")
       , ("offer_id", "")
       , ("offer_type", "")
       , ("orderby", "")
       , ("page", "")
       , ("sort", "")
       , ("status", "107")
       , ("tracking_type", "")
       , ("traffic_type", "")
       ]

-- | Returns all blacklisted affiliates for the offer ID provided.
--   
getOfferBlacklist =
  Call "offer_offer"
       "getOfferBlacklist"
       "GET"
       [ ("offer_id", "")] -- Required

getOfferBrowserLanguageAllowed =
    Call "offer_offer"
         "getOfferBrowserLanguageAllowed"
         "GET"
         [ ("offer_id", "")] -- Required
  
getOfferBrowserLanguageBlocked =
    Call "offer_offer"
         "getOfferBrowserLanguageBlocked"
         "GET"
         [ ("offer_id", "")] -- Required

-- | All offer categories returned.
--  
getOfferCategories =
  Call "offer_offer"
       "getOfferCategories"
       "GET"
       []

-- | Returns all categories listed for the offer ID provided.
-- 
getOfferCategory = 
  Call "offer_offer"
       "getOfferCategory"
       "GET"
        [ ("offer_id", "")] -- Required

-- | Return Int
--  
getOfferCount = 
  Call "offer_offer"
       "getOfferCount"
       "GET"
       [ ("category", "")
       , ("converts_on", "")
       , ("device", "")
       , ("merchant_id", "")
       , ("name", "")
       , ("offer_type", "")
       , ("status", "")
       , ("tracking_type", "")
       , ("traffic_type", "")  
       ]

getOfferCountry = 
  Call "offer_offer"
       "getOfferCountry"
       "GET"
        [ ("offer_id", "")] -- Required

getOfferCustomAffiliateCap = 
  Call "offer_offer"
       "getOfferCustomAffiliateCap"
       "GET"
        [ ("offer_id", "")] -- Required

getOfferCustomAffiliatePayout =
  Call "offer_offer"
       "getOfferCustomAffiliatePayout"
       "GET"
       [ ("offer_id", "")]  

getOfferDeviceType =
  Call "offer_offer"
       "getOfferDeviceType"
       "GET"
       [ ("offer_id", "")]  

getOfferOptimization =
  Call "offer_offer"
       "getOfferOptimization"
       "GET"
       [ ("offer_id", "")]

getOfferPrivate =
  Call "offer_offer"
       "getOfferPrivate"
       "GET"
       [ ("offer_id", "")]
  
getOfferSchedule = 
  Call "offer_offer"
       "getOfferSchedule"
       "GET"
       [ ("offer_id", "")]
    
-- | Returns array of all states targeted for the offer_id provided.
-- 
getOfferState =
  Call "offer_offer"
       "getOfferState"
       "GET"
       [ ("offer_id", "")] -- Required

getOfferStatus =
  Call "offer_offer"
       "getOfferStatus"
       "GET"
       []

getOfferTargeting =
  Call "offer_offer"
       "getOfferTargeting"
       "GET"
       []

getOfferTrafficType =
  Call "offer_offer"
       "getOfferTrafficType"
       "GET"
       [ ("offer_id", "")] -- Required
  
getOfferTypes =
  Call "offer_offer"
       "getOfferTypes"
       "GET"
       []
  
getPixelTypes =
  Call "offer_offer"
       "getPixelTypes"
       "GET"
       []
  
getTestLink =
  Call "offer_offer"
       "getTestLink"
       "GET"
       [("admin_id", "")] -- Required
  
getTrackingTypes =
  Call "offer_offer"
       "getTrackingTypes"
       "GET"
       []
       
getTrafficTypes =   
  Call "offer_offer"
       "getTrafficTypes"
       "GET"
       []

removeOfferBlacklist =
  Call "offer_offer"
       "removeOfferBlacklist"
       "POST"
       [("affiliate_id", "")]

removeOfferBrowserLanguageAllowed =
  Call "offer_offer"
       "removeOfferBrowserLanguageAllowed"
       "POST"
       [("offer_id", "")]  -- Required
  
removeOfferBrowserLanguageBlocked =
  Call "offer_offer"
       "removeOfferBrowserLanguageBlocked"
       "POST"
       []
  
removeOfferCountry =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [("offer_id", "")] -- Required

removeOfferCustomAffiliateCap =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [("offer_id", "")] -- Required
  
removeOfferCustomAffiliatePayout =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [("offer_id", "")] -- Required

removeOfferDeviceType = 
  Call "offer_offer"
       "removeOfferDeviceType"
       "POST"
       [("device_id", "")] -- Required
       
removeOfferOptimization =
    Call "offer_offer"
       "removeOfferOptimization"
       "POST"
       [("offer_id", "") -- Required
       ,("affiliate_id", "") 
       ]
       
removeOfferPrivate =
  Call "offer_offer"
       "removeOfferPrivate"
       "POST"
       [("offer_id", "") -- Required
       ,("affiliate_id", "") 
       ]

removeOfferSchedule =
  Call "offer_offer"
       "removeOfferSchedule"
       "POST"
       [("offer_id", "") -- Required
       ,("schedule_id", "") 
       ]
  
removeOfferScheduleRate =
  Call "offer_offer"
       "removeOfferScheduleRate"
       "POST"
       [("offer_id", "") -- Required
       ,("schedule_id", "") 
       ]

removeOfferState =
  Call "offer_offer"
       "removeOfferState"
       "POST"
       [("offer_id", "") -- Required
       ,("state", "") 
       ]
  
removeOfferTrafficType =
  Call "offer_offer"
       "removeOfferTrafficType"
       "POST"
       [("offer_id", "") -- Required
       ,("traffic_type_id", "") 
       ]

updateOffer =
  Call "offer_offer"
       "updateOffer"
       "POST"
       [("offer_id", "") -- Required
       ,("basic_proxy_filter", "") 
       ,("block_ip", "") 
       ,("break_frame", "") 
       ,("captcha", "") 
       ,("cap_redirect", "") 
       ,("category", "") 
       ,("click_frequency", "") 
       ,("click_frequency_subnet", "") 
       ,("click_frequency_unit", "") 
       ,("converts_at", "") 
       ,("cookie_life", "") 
       ,("daily_aff_cap", "") 
       ,("daily_cap", "") 
       ,("geo_redirect", "") 
       ,("hide_lead_rate", "") 
       ,("hourly_cap", "") 
       ,("intense_proxy_filter", "") 
       ,("lead_rate", "") 
       ,("maxmind", "") 
       ,("merchant_id", "") 
       ,("merchant_paying", "") 
       ,("monthly_aff_cap", "") 
       ,("monthly_cap", "") 
       ,("name", "") 
       ,("name_private", "") 
       ,("pixel_type", "") 
       ,("preview", "") 
       ,("private", "") 
       ,("redirect", "") 
       ,("reject_info", "") 
       ,("requirements", "") 
       ,("select", "") 
       ,("select_by", "") 
       ,("select_who", "") 
       ,("status", "") 
       ,("subnet", "") 
       ,("time_zone", "") 
       ,("total_aff_cap", "") 
       ,("total_cap", "") 
       ,("tracking_type", "") 
       ,("type", "") 
       ,("weekly_aff_cap", "") 
       ,("weekly_cap", "") 
       ]
  
updateOfferSchedule =
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ ("datetime_start", "") -- Required
       , ("offer_id", "")       -- Required
       , ("datetime_end", "")
       , ("new_rate", "")
       , ("status", "")  
       ]
       
updateTrackingLink = 
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ ("offer_id", "")     -- Required
       , ("tracking_url", "") -- Required  
       ]
