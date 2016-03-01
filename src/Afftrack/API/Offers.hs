{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Offers
       ( Offer(..)
       , addBrowserLanguageBlocked
       , addOfferBlacklist
       , addOfferBrowserLanguageAllowed
       , addOfferCategory
       , addOfferCountry
       , addOfferCustomAffiliateCap
       , addOfferCustomAffiliatePayout
       , addOfferDeviceType
       , addOfferOptimization
       , addOfferPrivate
       , addOfferState
       , addOfferTrafficType
       , createBannerCreative
       , createOffer
       , createOfferSchedule
       , createOfferScheduleDaily
       , createOfferScheduleRate  
       , createTextCreative  
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
       , removeOfferCustomAffiliateCap
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
       "POST"
       [ Param "language" True ""   -- Required
       , Param "offer_id" True ""   -- Required
       ]

addOfferBlacklist =
  Call "offer_offer"
       "addOfferBlacklist"
       "POST"
       [ Param "affiliate_id" True  ""   -- Required
       , Param "offer_id"     True  ""   -- Required
       , Param "reason"       False ""  
       ]
  
addOfferBrowserLanguageAllowed =
  Call "offer_offer"
       "addOfferBrowserLanguageAllowed"
       "POST"
       [ Param "language" True ""   -- Required
       , Param "offer_id" True ""   -- Required
       ]
  
addOfferCategory =
  Call "offer_offer"
       "addOfferCategory"
       "POST"
       [ Param "category_id" True ""   -- Required
       , Param "offer_id"    True ""      -- Required
       ]
  
addOfferCountry = 
  Call "offer_offer"
       "addOfferCountry"
       "POST"
       [ Param "country"  True ""    -- Required
       , Param "offer_id" True ""   -- Required
       , Param "enforce"  True ""  
       ]

addOfferCustomAffiliateCap =
  Call "offer_offer"
       "addOfferCustomAffiliateCap"
       "POST"
       [ Param "affiliate_id" True ""    -- Required
       , Param "offer_id"     True ""        -- Required
       , Param "type"         True ""            -- Required  
       , Param "day"          True ""  
       , Param "month"        True ""  
       , Param "total"        True ""  
       , Param "week"         True ""  
       ]
       
addOfferCustomAffiliatePayout =
  Call "offer_offer"
       "addOfferCustomAffiliatePayout"
       "POST"
       [ Param "affiliate_id"     True ""        -- Required
       , Param "affiliate_payout" True ""    -- Required
       , Param "offer_id"         True ""            -- Required  
       , Param "merchant_payout"  True ""    
       ]  

addOfferDeviceType =
  Call "offer_offer"
       "addOfferDeviceType"
       "POST"
       [ Param "device_id" True ""        -- Required
       , Param "offer_id"  True ""         -- Required  
       ]  
  
addOfferOptimization =
  Call "offer_offer"
       "addOfferOptimization"
       "POST"
       [ Param "affiliate_id" True ""     -- Required
       , Param "offer_id"     True ""         -- Required
       , Param "percent"      True ""          -- Required  
       ]  
  
addOfferPrivate =
  Call "offer_offer"
       "addOfferPrivate"
       "POST"
       [ Param "affiliate_id" True ""     -- Required
       , Param "offer_id"     True ""         -- Required
       ]    

addOfferState =
  Call "offer_offer"
       "addOfferState"
       "POST"
       [ Param "offer_id" True ""     -- Required
       , Param "state"    True ""        -- Required
       ]    

addOfferTrafficType =
  Call "offer_offer"
       "addOfferTrafficType"
       "POST"
       [ Param "banner"   True ""       -- Required
       , Param "offer_id" True ""     -- Required
       , Param "url"      True ""  
       ]    

createBannerCreative = 
  Call "offer_offer"
       "createBannerCreative"
       "POST"
       [ Param "offer_id"        True ""            -- Required
       , Param "traffic_type_id" True ""     -- Required
       ]

createOffer = 
  Call "offer_offer"
       "createOffer"
       "POST"
       [ Param "admin_id"        True ""      -- Required
       , Param "category"        True ""     -- Required
       , Param "converts_at"     True ""     -- Required
       , Param "lead_rate"       True ""     -- Required
       , Param "merchant_id"     True ""     -- Required
       , Param "merchant_paying" True ""     -- Required
       , Param "name"            True ""
       , Param "name_private"    True ""
       , Param "note"            True ""
       , Param "preview"         True ""
       , Param "requirements"    True ""  
       ]

createOfferSchedule =
  Call "offer_offer"
       "createOfferSchedule"
       "POST"
       [ Param "offer_id"       True ""        -- Required
       , Param "schedule_start" True ""  -- Required
       , Param "type"           True ""            -- Required
       , Param "admin_id"       True ""  
       ]

createOfferScheduleDaily =
  Call "offer_offer"
       "createOfferScheduleDaily"
       "POST"
       [ Param "daily_end_time"   True ""    -- Required
       , Param "daily_start_time" True ""    -- Required
       , Param "offer_id"         True ""    -- Required
       , Param "schedule_end"     True ""    -- Required  
       , Param "schedule_start"   True ""    -- Required 
       , Param "admin_id"         True ""  
       ]  

createOfferScheduleRate =
  Call "offer_offer"
       "createOfferScheduleRate"
       "POST"
       [ Param "affiliate_payout" True ""    -- Required
       , Param "merchant_payout"  True ""     -- Required
       , Param "offer_id"         True ""            -- Required
       , Param "schedule_start"   True ""      -- Required
       , Param "admin_id"         True ""  
       ]  

createTextCreative =
  Call "offer_offer"
       "createTextCreative"
       "POST"
       [ Param "offer_id" True ""    -- Required
       , Param "url"      True ""     -- Required
       , Param "text"     True ""             
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
       [ Param "offer_id" True ""] -- Required  

getCreatives =
  Call "offer_offer"
       "getCreatives"
       "GET"
       [ Param "offer_id" True ""] -- Required  
  
getDeviceTypes =
  Call "offer_offer"
       "getDeviceTypes"
       "GET"
       []  

getOffer =
  Call "offer_offer"
       "getOffer"
       "GET"
       [ Param "category"     True ""
       , Param "converts_on"  True ""
       , Param "device_type"  True ""
       , Param "limit"        True ""
       , Param "merchant_id"  True "" -- empty
       , Param "name"         True ""
       , Param "offer_id"     True ""
       , Param "offer_type"   True ""
       , Param "orderby"      True ""
       , Param "page"         True ""
       , Param "sort"         True ""
       , Param "status"       True "107"
       , Param "tracking_type" True ""
       , Param "traffic_type"  True ""
       ]

-- | Returns all blacklisted affiliates for the offer ID provided.
--   
getOfferBlacklist =
  Call "offer_offer"
       "getOfferBlacklist"
       "GET"
       [Param "offer_id" True ""] -- Required

getOfferBrowserLanguageAllowed =
    Call "offer_offer"
         "getOfferBrowserLanguageAllowed"
         "GET"
         [ Param "offer_id" True ""] -- Required
  
getOfferBrowserLanguageBlocked =
    Call "offer_offer"
         "getOfferBrowserLanguageBlocked"
         "GET"
         [ Param "offer_id" True ""] -- Required

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
        [ Param "offer_id" True ""] -- Required

-- | Return Int
--  
getOfferCount = 
  Call "offer_offer"
       "getOfferCount"
       "GET"
       [ Param "category"      True ""
       , Param "converts_on"   True ""
       , Param "device"        True ""
       , Param "merchant_id"   True ""
       , Param "name"          True ""
       , Param "offer_type"    True ""
       , Param "status"        True ""
       , Param "tracking_type" True ""
       , Param "traffic_type"  True ""  
       ]

getOfferCountry = 
  Call "offer_offer"
       "getOfferCountry"
       "GET"
        [ Param "offer_id" True ""] -- Required

getOfferCustomAffiliateCap = 
  Call "offer_offer"
       "getOfferCustomAffiliateCap"
       "GET"
        [ Param "offer_id" True ""] -- Required

getOfferCustomAffiliatePayout =
  Call "offer_offer"
       "getOfferCustomAffiliatePayout"
       "GET"
       [ Param "offer_id" True ""]  

getOfferDeviceType =
  Call "offer_offer"
       "getOfferDeviceType"
       "GET"
       [ Param "offer_id" True ""]  

getOfferOptimization =
  Call "offer_offer"
       "getOfferOptimization"
       "GET"
       [ Param "offer_id" True ""]

getOfferPrivate =
  Call "offer_offer"
       "getOfferPrivate"
       "GET"
       [ Param "offer_id" True ""]
  
getOfferSchedule = 
  Call "offer_offer"
       "getOfferSchedule"
       "GET"
       [ Param "offer_id" True ""]
    
-- | Returns array of all states targeted for the offer_id provided.
-- 
getOfferState =
  Call "offer_offer"
       "getOfferState"
       "GET"
       [ Param "offer_id" True ""] -- Required

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
       [ Param "offer_id" True ""] -- Required
  
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
       [ Param "admin_id" True ""] -- Required
  
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
       [ Param "affiliate_id" True ""]

removeOfferBrowserLanguageAllowed =
  Call "offer_offer"
       "removeOfferBrowserLanguageAllowed"
       "POST"
       [ Param "offer_id" True ""]  -- Required
  
removeOfferBrowserLanguageBlocked =
  Call "offer_offer"
       "removeOfferBrowserLanguageBlocked"
       "POST"
       []
  
removeOfferCountry =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True ""] -- Required

removeOfferCustomAffiliateCap =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True ""] -- Required
  
removeOfferCustomAffiliatePayout =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True ""] -- Required

removeOfferDeviceType = 
  Call "offer_offer"
       "removeOfferDeviceType"
       "POST"
       [ Param "device_id" True ""] -- Required
       
removeOfferOptimization =
    Call "offer_offer"
       "removeOfferOptimization"
       "POST"
       [ Param "offer_id"      True "" -- Required
       , Param "affiliate_id"  True "" 
       ]
       
removeOfferPrivate =
  Call "offer_offer"
       "removeOfferPrivate"
       "POST"
       [ Param "offer_id"     True "" -- Required
       , Param "affiliate_id" True "" 
       ]

removeOfferSchedule =
  Call "offer_offer"
       "removeOfferSchedule"
       "POST"
       [ Param "offer_id"    True "" -- Required
       , Param "schedule_id" True "" 
       ]
  
removeOfferScheduleRate =
  Call "offer_offer"
       "removeOfferScheduleRate"
       "POST"
       [ Param "offer_id"    True "" -- Required
       , Param "schedule_id" True "" 
       ]

removeOfferState =
  Call "offer_offer"
       "removeOfferState"
       "POST"
       [ Param "offer_id" True "" -- Required
       , Param "state"    True "" 
       ]
  
removeOfferTrafficType =
  Call "offer_offer"
       "removeOfferTrafficType"
       "POST"
       [ Param "offer_id"        True "" -- Required
       , Param "traffic_type_id" True "" 
       ]

updateOffer =
  Call "offer_offer"
       "updateOffer"
       "POST"
       [ Param "offer_id"True "" -- Required
       , Param "basic_proxy_filter"     True "" 
       , Param "block_ip"               True "" 
       , Param "break_frame"            True "" 
       , Param "captcha"                True "" 
       , Param "cap_redirect"           True "" 
       , Param "category"               True "" 
       , Param "click_frequency"        True "" 
       , Param "click_frequency_subnet" True "" 
       , Param "click_frequency_unit"   True "" 
       , Param "converts_at"            True "" 
       , Param "cookie_life"            True "" 
       , Param "daily_aff_cap"          True "" 
       , Param "daily_cap"              True "" 
       , Param "geo_redirect"           True "" 
       , Param "hide_lead_rate"         True "" 
       , Param "hourly_cap"             True "" 
       , Param "intense_proxy_filter"   True "" 
       , Param "lead_rate"              True "" 
       , Param "maxmind"                True "" 
       , Param "merchant_id"            True "" 
       , Param "merchant_paying"        True "" 
       , Param "monthly_aff_cap"        True "" 
       , Param "monthly_cap"            True "" 
       , Param "name"                   True "" 
       , Param "name_private"           True "" 
       , Param "pixel_type"             True "" 
       , Param "preview"                True "" 
       , Param "private"                True "" 
       , Param "redirect"               True "" 
       , Param "reject_info"            True "" 
       , Param "requirements"           True "" 
       , Param "select"                 True "" 
       , Param "select_by"              True "" 
       , Param "select_who"             True "" 
       , Param "status"                 True "" 
       , Param "subnet"                 True "" 
       , Param "time_zone"              True "" 
       , Param "total_aff_cap"          True "" 
       , Param "total_cap"              True "" 
       , Param "tracking_type"          True "" 
       , Param "type"                   True "" 
       , Param "weekly_aff_cap"         True "" 
       , Param "weekly_cap"             True "" 
       ]
  
updateOfferSchedule =
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ Param "datetime_start" True  "" -- Required
       , Param "offer_id"       True  "" -- Required
       , Param "datetime_end"   False ""
       , Param "new_rate"       False ""
       , Param "status"         False ""  
       ]
       
updateTrackingLink = 
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ Param "offer_id"     True "" -- Required
       , Param "tracking_url" True "" -- Required  
       ]
