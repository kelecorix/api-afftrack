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
       , Param "offer_id"    True ""   -- Required
       ]
  
addOfferCountry = 
  Call "offer_offer"
       "addOfferCountry"
       "POST"
       [ Param "country"  True ""   -- Required
       , Param "offer_id" True ""   -- Required
       , Param "enforce"  False ""  
       ]

addOfferCustomAffiliateCap =
  Call "offer_offer"
       "addOfferCustomAffiliateCap"
       "POST"
       [ Param "affiliate_id" True ""    -- Required
       , Param "offer_id"     True ""    -- Required
       , Param "type"         True ""    -- Required  
       , Param "day"          False ""  
       , Param "month"        False ""  
       , Param "total"        False ""  
       , Param "week"         False ""  
       ]
       
addOfferCustomAffiliatePayout =
  Call "offer_offer"
       "addOfferCustomAffiliatePayout"
       "POST"
       [ Param "affiliate_id"     True ""    -- Required
       , Param "affiliate_payout" True ""    -- Required
       , Param "offer_id"         True ""    -- Required  
       , Param "merchant_payout"  False ""    
       ]  

addOfferDeviceType =
  Call "offer_offer"
       "addOfferDeviceType"
       "POST"
       [ Param "device_id" True ""        -- Required
       , Param "offer_id"  True ""        -- Required  
       ]  
  
addOfferOptimization =
  Call "offer_offer"
       "addOfferOptimization"
       "POST"
       [ Param "affiliate_id" True ""     -- Required
       , Param "offer_id"     True ""     -- Required
       , Param "percent"      True ""     -- Required  
       ]  
  
addOfferPrivate =
  Call "offer_offer"
       "addOfferPrivate"
       "POST"
       [ Param "affiliate_id" True ""     -- Required
       , Param "offer_id"     True ""     -- Required
       ]    

addOfferState =
  Call "offer_offer"
       "addOfferState"
       "POST"
       [ Param "offer_id" True ""     -- Required
       , Param "state"    True ""     -- Required
       ]    

addOfferTrafficType =
  Call "offer_offer"
       "addOfferTrafficType"
       "POST"
       [ Param "banner"   True ""     -- Required
       , Param "offer_id" True ""     -- Required
       , Param "url"      False ""  
       ]    

createBannerCreative = 
  Call "offer_offer"
       "createBannerCreative"
       "POST"
       [ Param "offer_id"        True ""      -- Required
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
       , Param "name"            False ""
       , Param "name_private"    False ""
       , Param "note"            False ""
       , Param "preview"         False ""
       , Param "requirements"    False ""  
       ]

createOfferSchedule =
  Call "offer_offer"
       "createOfferSchedule"
       "POST"
       [ Param "offer_id"       True ""  -- Required
       , Param "schedule_start" True ""  -- Required
       , Param "type"           True ""  -- Required
       , Param "admin_id"       False ""  
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
       , Param "admin_id"         False ""  
       ]  

createOfferScheduleRate =
  Call "offer_offer"
       "createOfferScheduleRate"
       "POST"
       [ Param "affiliate_payout" True ""    -- Required
       , Param "merchant_payout"  True ""    -- Required
       , Param "offer_id"         True ""    -- Required
       , Param "schedule_start"   True ""    -- Required
       , Param "admin_id"         False ""  
       ]  

createTextCreative =
  Call "offer_offer"
       "createTextCreative"
       "POST"
       [ Param "offer_id" True ""    -- Required
       , Param "url"      True ""     -- Required
       , Param "text"     False ""             
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
       [ Param "category"      False ""
       , Param "converts_on"   False ""
       , Param "device_type"   False ""
       , Param "limit"         False ""
       , Param "merchant_id"   False "" -- empty
       , Param "name"          False ""
       , Param "offer_id"      False ""
       , Param "offer_type"    False ""
       , Param "orderby"       False ""
       , Param "page"          False ""
       , Param "sort"          False ""
       , Param "status"        False "" -- 107 active
       , Param "tracking_type" False ""
       , Param "traffic_type"  False ""
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
       [ Param "category"      False ""
       , Param "converts_on"   False ""
       , Param "device"        False ""
       , Param "merchant_id"   False ""
       , Param "name"          False ""
       , Param "offer_type"    False ""
       , Param "status"        False ""
       , Param "tracking_type" False ""
       , Param "traffic_type"  False ""  
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
       , Param "affiliate_id"  False "" 
       ]
       
removeOfferPrivate =
  Call "offer_offer"
       "removeOfferPrivate"
       "POST"
       [ Param "offer_id"     True "" -- Required
       , Param "affiliate_id" False "" 
       ]

removeOfferSchedule =
  Call "offer_offer"
       "removeOfferSchedule"
       "POST"
       [ Param "offer_id"    True "" -- Required
       , Param "schedule_id" False "" 
       ]
  
removeOfferScheduleRate =
  Call "offer_offer"
       "removeOfferScheduleRate"
       "POST"
       [ Param "offer_id"    True "" -- Required
       , Param "schedule_id" False "" 
       ]

removeOfferState =
  Call "offer_offer"
       "removeOfferState"
       "POST"
       [ Param "offer_id" True "" -- Required
       , Param "state"    False "" 
       ]
  
removeOfferTrafficType =
  Call "offer_offer"
       "removeOfferTrafficType"
       "POST"
       [ Param "offer_id"        True "" -- Required
       , Param "traffic_type_id" False "" 
       ]

updateOffer =
  Call "offer_offer"
       "updateOffer"
       "POST"
       [ Param "offer_id"               True "" -- Required
       , Param "basic_proxy_filter"     False "" 
       , Param "block_ip"               False "" 
       , Param "break_frame"            False "" 
       , Param "captcha"                False "" 
       , Param "cap_redirect"           False "" 
       , Param "category"               False "" 
       , Param "click_frequency"        False "" 
       , Param "click_frequency_subnet" False "" 
       , Param "click_frequency_unit"   False "" 
       , Param "converts_at"            False "" 
       , Param "cookie_life"            False "" 
       , Param "daily_aff_cap"          False "" 
       , Param "daily_cap"              False "" 
       , Param "geo_redirect"           False "" 
       , Param "hide_lead_rate"         False "" 
       , Param "hourly_cap"             False "" 
       , Param "intense_proxy_filter"   False "" 
       , Param "lead_rate"              False "" 
       , Param "maxmind"                False "" 
       , Param "merchant_id"            False "" 
       , Param "merchant_paying"        False "" 
       , Param "monthly_aff_cap"        False "" 
       , Param "monthly_cap"            False "" 
       , Param "name"                   False "" 
       , Param "name_private"           False "" 
       , Param "pixel_type"             False "" 
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
