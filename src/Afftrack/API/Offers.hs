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
import Data.Text
import Afftrack.API.Common
import Afftrack.API.Types

--------------------------------------------------------------------------------

addBrowserLanguageBlocked :: [Text] -> Call 
addBrowserLanguageBlocked params = 
  Call "offer_offer"
       "addBrowserLanguageBlocked"
       "POST"
       [ Param "language" True (getValue params 0)   -- Required
       , Param "offer_id" True (getValue params 1)  -- Required
       ]

addOfferBlacklist :: [Text] -> Call 
addOfferBlacklist params =
  Call "offer_offer"
       "addOfferBlacklist"
       "POST"
       [ Param "affiliate_id" True  (getValue params 0)   -- Required
       , Param "offer_id"     True  (getValue params 1)   -- Required
       , Param "reason"       False (getValue params 2)  
       ]

addOfferBrowserLanguageAllowed :: [Text] -> Call 
addOfferBrowserLanguageAllowed params =
  Call "offer_offer"
       "addOfferBrowserLanguageAllowed"
       "POST"
       [ Param "language" True (getValue params 0)   -- Required
       , Param "offer_id" True (getValue params 1)   -- Required
       ]

addOfferCategory :: [Text] -> Call      
addOfferCategory params =
  Call "offer_offer"
       "addOfferCategory"
       "POST"
       [ Param "category_id" True (getValue params 0)   -- Required
       , Param "offer_id"    True (getValue params 1)   -- Required
       ]

addOfferCountry :: [Text] -> Call      
addOfferCountry params = 
  Call "offer_offer"
       "addOfferCountry"
       "POST"
       [ Param "country"  True (getValue params 0)   -- Required
       , Param "offer_id" True (getValue params 1)   -- Required
       , Param "enforce"  False (getValue params 2)  
       ]

addOfferCustomAffiliateCap :: [Text] -> Call
addOfferCustomAffiliateCap params =
  Call "offer_offer"
       "addOfferCustomAffiliateCap"
       "POST"
       [ Param "affiliate_id" True (getValue params 0)    -- Required
       , Param "offer_id"     True (getValue params 1)    -- Required
       , Param "type"         True (getValue params 2)    -- Required  
       , Param "day"          False (getValue params 3)  
       , Param "month"        False (getValue params 4)
       , Param "total"        False (getValue params 5)  
       , Param "week"         False (getValue params 6)  
       ]

addOfferCustomAffiliatePayout :: [Text] -> Call      
addOfferCustomAffiliatePayout params =
  Call "offer_offer"
       "addOfferCustomAffiliatePayout"
       "POST"
       [ Param "affiliate_id"     True (getValue params 0)    -- Required
       , Param "affiliate_payout" True (getValue params 1)    -- Required
       , Param "offer_id"         True (getValue params 2)    -- Required  
       , Param "merchant_payout"  False (getValue params 3)    
       ]  

addOfferDeviceType :: [Text] -> Call
addOfferDeviceType params =
  Call "offer_offer"
       "addOfferDeviceType"
       "POST"
       [ Param "device_id" True (getValue params 0)        -- Required
       , Param "offer_id"  True (getValue params 1)        -- Required  
       ]  

addOfferOptimization :: [Text] -> Call
addOfferOptimization params =
  Call "offer_offer"
       "addOfferOptimization"
       "POST"
       [ Param "affiliate_id" True (getValue params 0)     -- Required
       , Param "offer_id"     True (getValue params 1)     -- Required
       , Param "percent"      True (getValue params 2)     -- Required  
       ]  
addOfferPrivate :: [Text] -> Call 
addOfferPrivate params =
  Call "offer_offer"
       "addOfferPrivate"
       "POST"
       [ Param "affiliate_id" True (getValue params 0)     -- Required
       , Param "offer_id"     True (getValue params 1)     -- Required
       ]    

addOfferState :: [Text] -> Call
addOfferState params =
  Call "offer_offer"
       "addOfferState"
       "POST"
       [ Param "offer_id" True (getValue params 0)     -- Required
       , Param "state"    True (getValue params 1)     -- Required
       ]    

addOfferTrafficType :: [Text] -> Call
addOfferTrafficType params =
  Call "offer_offer"
       "addOfferTrafficType"
       "POST"
       [ Param "banner"   True (getValue params 0)     -- Required
       , Param "offer_id" True (getValue params 1)     -- Required
       , Param "url"      False (getValue params 2)  
       ]    

createBannerCreative :: [Text] -> Call
createBannerCreative params = 
  Call "offer_offer"
       "createBannerCreative"
       "POST"
       [ Param "offer_id"        True (getValue params 0)      -- Required
       , Param "traffic_type_id" True (getValue params 1)     -- Required
       ]

createOffer :: [Text] -> Call
createOffer params = 
  Call "offer_offer"
       "createOffer"
       "POST"
       [ Param "admin_id"        True (getValue params 0)      -- Required
       , Param "category"        True (getValue params 1)     -- Required
       , Param "converts_at"     True (getValue params 2)     -- Required
       , Param "lead_rate"       True (getValue params 3)     -- Required
       , Param "merchant_id"     True (getValue params 4)     -- Required
       , Param "merchant_paying" True (getValue params 5)     -- Required
       , Param "name"            False (getValue params 6)
       , Param "name_private"    False (getValue params 7)
       , Param "note"            False (getValue params 8)
       , Param "preview"         False (getValue params 9)
       , Param "requirements"    False (getValue params 10)
       ]

createOfferSchedule :: [Text] -> Call
createOfferSchedule params =
  Call "offer_offer"
       "createOfferSchedule"
       "POST"
       [ Param "offer_id"       True (getValue params 0)  -- Required
       , Param "schedule_start" True (getValue params 1)  -- Required
       , Param "type"           True (getValue params 2)  -- Required
       , Param "admin_id"       False (getValue params 3)  
       ]

createOfferScheduleDaily :: [Text] -> Call
createOfferScheduleDaily params =
  Call "offer_offer"
       "createOfferScheduleDaily"
       "POST"
       [ Param "daily_end_time"   True (getValue params 0)    -- Required
       , Param "daily_start_time" True (getValue params 1)    -- Required
       , Param "offer_id"         True (getValue params 2)    -- Required
       , Param "schedule_end"     True (getValue params 3)    -- Required  
       , Param "schedule_start"   True (getValue params 4)    -- Required 
       , Param "admin_id"         False (getValue params 5)  
       ]  

createOfferScheduleRate :: [Text] -> Call
createOfferScheduleRate params =
  Call "offer_offer"
       "createOfferScheduleRate"
       "POST"
       [ Param "affiliate_payout" True (getValue params 0)    -- Required
       , Param "merchant_payout"  True (getValue params 1)    -- Required
       , Param "offer_id"         True (getValue params 2)    -- Required
       , Param "schedule_start"   True (getValue params 3)    -- Required
       , Param "admin_id"         False (getValue params 4)  
       ]  

createTextCreative :: [Text] -> Call
createTextCreative params =
  Call "offer_offer"
       "createTextCreative"
       "POST"
       [ Param "offer_id" True (getValue params 0)    -- Required
       , Param "url"      True (getValue params 1)     -- Required
       , Param "text"     False (getValue params 2)             
       ]    

getBrowserLanguages :: [Text] -> Call      
getBrowserLanguages params =
  Call "offer_offer"
       "getBrowserLanguages"
       "GET"
       []

getConvertsOn :: [Text] -> Call
getConvertsOn params =
  Call "offer_offer"
       "getConvertsOn"
       "GET"
       []

getCreativeCounts :: [Text] -> Call      
getCreativeCounts params =
  Call "offer_offer"
       "getCreativeCounts"
       "GET"
       [ Param "offer_id" True (getValue params 0)] -- Required  

getCreatives :: [Text] -> Call
getCreatives params =
  Call "offer_offer"
       "getCreatives"
       "GET"
       [ Param "offer_id" True (getValue params 0)] -- Required  

getDeviceTypes :: [Text] -> Call 
getDeviceTypes params =
  Call "offer_offer"
       "getDeviceTypes"
       "GET"
       []  

getOffer :: [Text] -> Call  
getOffer params =
  Call "offer_offer"
       "getOffer"
       "GET"
       [ Param "category"      False (getValue params 0)
       , Param "converts_on"   False (getValue params 1)
       , Param "device_type"   False (getValue params 2)
       , Param "limit"         False (getValue params 3)
       , Param "merchant_id"   False (getValue params 4) -- empty
       , Param "name"          False (getValue params 5)
       , Param "offer_id"      False (getValue params 6)
       , Param "offer_type"    False (getValue params 7)
       , Param "orderby"       False (getValue params 8)
       , Param "page"          False (getValue params 9)
       , Param "sort"          False (getValue params 10)
       , Param "status"        False (getValue params 11) -- 107 active
       , Param "tracking_type" False (getValue params 12)
       , Param "traffic_type"  False (getValue params 13)
       ]

-- | Returns all blacklisted affiliates for the offer ID provided.
--
getOfferBlacklist :: [Text] -> Call
getOfferBlacklist params =
  Call "offer_offer"
       "getOfferBlacklist"
       "GET"
       [Param "offer_id" True (getValue params 0)] -- Required

getOfferBrowserLanguageAllowed :: [Text] -> Call
getOfferBrowserLanguageAllowed params =
    Call "offer_offer"
         "getOfferBrowserLanguageAllowed"
         "GET"
         [ Param "offer_id" True (getValue params 0)] -- Required

getOfferBrowserLanguageBlocked :: [Text] -> Call
getOfferBrowserLanguageBlocked params =
    Call "offer_offer"
         "getOfferBrowserLanguageBlocked"
         "GET"
         [ Param "offer_id" True (getValue params 0)] -- Required

-- | All offer categories returned.
--
getOfferCategories :: [Text] -> Call
getOfferCategories params =
  Call "offer_offer"
       "getOfferCategories"
       "GET"
       []

-- | Returns all categories listed for the offer ID provided.
--
getOfferCategory :: [Text] -> Call
getOfferCategory params = 
  Call "offer_offer"
       "getOfferCategory"
       "GET"
        [ Param "offer_id" True (getValue params 0)] -- Required

-- | Return Int
--
getOfferCount :: [Text] -> Call
getOfferCount params = 
  Call "offer_offer"
       "getOfferCount"
       "GET"
       [ Param "category"      False (getValue params 0)
       , Param "converts_on"   False (getValue params 1)
       , Param "device"        False (getValue params 2)
       , Param "merchant_id"   False (getValue params 3)
       , Param "name"          False (getValue params 4)
       , Param "offer_type"    False (getValue params 5)
       , Param "status"        False (getValue params 6)
       , Param "tracking_type" False (getValue params 7)
       , Param "traffic_type"  False (getValue params 8)  
       ]

getOfferCountry :: [Text] -> Call
getOfferCountry params = 
  Call "offer_offer"
       "getOfferCountry"
       "GET"
        [ Param "offer_id" True (getValue params 0)] -- Required

getOfferCustomAffiliateCap :: [Text] -> Call
getOfferCustomAffiliateCap params = 
  Call "offer_offer"
       "getOfferCustomAffiliateCap"
       "GET"
        [ Param "offer_id" True (getValue params 0)] -- Required

getOfferCustomAffiliatePayout :: [Text] -> Call
getOfferCustomAffiliatePayout params =
  Call "offer_offer"
       "getOfferCustomAffiliatePayout"
       "GET"
       [ Param "offer_id" True (getValue params 0)]  

getOfferDeviceType :: [Text] -> Call
getOfferDeviceType params =
  Call "offer_offer"
       "getOfferDeviceType"
       "GET"
       [ Param "offer_id" True (getValue params 0)]  

getOfferOptimization :: [Text] -> Call
getOfferOptimization params =
  Call "offer_offer"
       "getOfferOptimization"
       "GET"
       [ Param "offer_id" True (getValue params 0)]

getOfferPrivate :: [Text] -> Call
getOfferPrivate params =
  Call "offer_offer"
       "getOfferPrivate"
       "GET"
       [ Param "offer_id" True (getValue params 0)]

getOfferSchedule :: [Text] -> Call
getOfferSchedule params = 
  Call "offer_offer"
       "getOfferSchedule"
       "GET"
       [ Param "offer_id" True (getValue params 0)]
    
-- | Returns array of all states targeted for the offer_id provided.
--
getOfferState :: [Text] -> Call
getOfferState params =
  Call "offer_offer"
       "getOfferState"
       "GET"
       [ Param "offer_id" True (getValue params 0)] -- Required

getOfferStatus :: [Text] -> Call
getOfferStatus params =
  Call "offer_offer"
       "getOfferStatus"
       "GET"
       []
       
getOfferTargeting :: [Text] -> Call
getOfferTargeting params =
  Call "offer_offer"
       "getOfferTargeting"
       "GET"
       []
       
getOfferTrafficType :: [Text] -> Call
getOfferTrafficType params =
  Call "offer_offer"
       "getOfferTrafficType"
       "GET"
       [ Param "offer_id" True (getValue params 0)] -- Required

getOfferTypes :: [Text] -> Call  
getOfferTypes params =
  Call "offer_offer"
       "getOfferTypes"
       "GET"
       []
       
getPixelTypes :: [Text] -> Call  
getPixelTypes params =
  Call "offer_offer"
       "getPixelTypes"
       "GET"
       []

getTestLink:: [Text] -> Call       
getTestLink params =
  Call "offer_offer"
       "getTestLink"
       "GET"
       [ Param "offer_id" True (getValue params 0)] -- Required
                                            -- NB: API states that admin_id, required, but this is probably a typo
       
getTrackingTypes :: [Text] -> Call 
getTrackingTypes params =
  Call "offer_offer"
       "getTrackingTypes"
       "GET"
       []
       
getTrafficTypes :: [Text] -> Call       
getTrafficTypes params =   
  Call "offer_offer"
       "getTrafficTypes"
       "GET"
       []
       
removeOfferBlacklist :: [Text] -> Call
removeOfferBlacklist params =
  Call "offer_offer"
       "removeOfferBlacklist"
       "POST"
       [ Param "affiliate_id" True (getValue params 0)]
       
removeOfferBrowserLanguageAllowed :: [Text] -> Call
removeOfferBrowserLanguageAllowed params =
  Call "offer_offer"
       "removeOfferBrowserLanguageAllowed"
       "POST"
       [ Param "offer_id" True (getValue params 0)]  -- Required
       
removeOfferBrowserLanguageBlocked :: [Text] -> Call  
removeOfferBrowserLanguageBlocked params =
  Call "offer_offer"
       "removeOfferBrowserLanguageBlocked"
       "POST"
       []
       
removeOfferCountry :: [Text] -> Call  
removeOfferCountry params =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True (getValue params 0)] -- Required
       
removeOfferCustomAffiliateCap :: [Text] -> Call
removeOfferCustomAffiliateCap params =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True (getValue params 0)] -- Required
       
removeOfferCustomAffiliatePayout :: [Text] -> Call 
removeOfferCustomAffiliatePayout params =
  Call "offer_offer"
       "removeOfferCountry"
       "POST"
       [ Param "offer_id" True (getValue params 0)] -- Required
       
removeOfferDeviceType :: [Text] -> Call
removeOfferDeviceType params = 
  Call "offer_offer"
       "removeOfferDeviceType"
       "POST"
       [ Param "device_id" True (getValue params 0)] -- Required
       
removeOfferOptimization :: [Text] -> Call       
removeOfferOptimization params =
    Call "offer_offer"
       "removeOfferOptimization"
       "POST"
       [ Param "offer_id"      True (getValue params 0) -- Required
       , Param "affiliate_id"  False (getValue params 1) 
       ]
       
removeOfferPrivate :: [Text] -> Call       
removeOfferPrivate params =
  Call "offer_offer"
       "removeOfferPrivate"
       "POST"
       [ Param "offer_id"     True (getValue params 0) -- Required
       , Param "affiliate_id" False (getValue params 1) 
       ]
       
removeOfferSchedule :: [Text] -> Call
removeOfferSchedule params =
  Call "offer_offer"
       "removeOfferSchedule"
       "POST"
       [ Param "offer_id"    True (getValue params 0) -- Required
       , Param "schedule_id" False (getValue params 1) 
       ]
       
removeOfferScheduleRate :: [Text] -> Call  
removeOfferScheduleRate params =
  Call "offer_offer"
       "removeOfferScheduleRate"
       "POST"
       [ Param "offer_id"    True (getValue params 0) -- Required
       , Param "schedule_id" False (getValue params 1) 
       ]
       
removeOfferState :: [Text] -> Call
removeOfferState params =
  Call "offer_offer"
       "removeOfferState"
       "POST"
       [ Param "offer_id" True (getValue params 0) -- Required
       , Param "state"    False (getValue params 1) 
       ]
       
removeOfferTrafficType :: [Text] -> Call  
removeOfferTrafficType params =
  Call "offer_offer"
       "removeOfferTrafficType"
       "POST"
       [ Param "offer_id"        True (getValue params 0) -- Required
       , Param "traffic_type_id" False (getValue params 1) 
       ]
       
updateOffer :: [Text] -> Call
updateOffer params =
  Call "offer_offer"
       "updateOffer"
       "POST"
       [ Param "offer_id"               True (getValue params 0) -- Required
       , Param "basic_proxy_filter"     False (getValue params 1) 
       , Param "block_ip"               False (getValue params 2) 
       , Param "break_frame"            False (getValue params 3)
       , Param "captcha"                False (getValue params 4)
       , Param "cap_redirect"           False (getValue params 5) 
       , Param "category"               False (getValue params 6) 
       , Param "click_frequency"        False (getValue params 7) 
       , Param "click_frequency_subnet" False (getValue params 8)
       , Param "click_frequency_unit"   False (getValue params 9) 
       , Param "converts_at"            False (getValue params 10) 
       , Param "cookie_life"            False (getValue params 11) 
       , Param "daily_aff_cap"          False (getValue params 12) 
       , Param "daily_cap"              False (getValue params 13) 
       , Param "geo_redirect"           False (getValue params 14)
       , Param "hide_lead_rate"         False (getValue params 15) 
       , Param "hourly_cap"             False (getValue params 16) 
       , Param "intense_proxy_filter"   False (getValue params 17) 
       , Param "lead_rate"              False (getValue params 18) 
       , Param "maxmind"                False (getValue params 19)
       , Param "merchant_id"            False (getValue params 20)
       , Param "merchant_paying"        False (getValue params 21)
       , Param "monthly_aff_cap"        False (getValue params 22) 
       , Param "monthly_cap"            False (getValue params 23) 
       , Param "name"                   False (getValue params 24) 
       , Param "name_private"           False (getValue params 25) 
       , Param "pixel_type"             False (getValue params 26) 
       , Param "preview"                True (getValue params 26) 
       , Param "private"                True (getValue params 27) 
       , Param "redirect"               True (getValue params 28) 
       , Param "reject_info"            True (getValue params 29) 
       , Param "requirements"           True (getValue params 30) 
       , Param "select"                 True (getValue params 31) 
       , Param "select_by"              True (getValue params 32) 
       , Param "select_who"             True (getValue params 33) 
       , Param "status"                 True (getValue params 34) 
       , Param "subnet"                 True (getValue params 35) 
       , Param "time_zone"              True (getValue params 36) 
       , Param "total_aff_cap"          True (getValue params 37) 
       , Param "total_cap"              True (getValue params 38) 
       , Param "tracking_type"          True (getValue params 39) 
       , Param "type"                   True (getValue params 40) 
       , Param "weekly_aff_cap"         True (getValue params 41) 
       , Param "weekly_cap"             True (getValue params 42) 
       ]

updateOfferSchedule :: [Text] -> Call       
updateOfferSchedule params =
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ Param "datetime_start" True  (getValue params 0) -- Required
       , Param "offer_id"       True  (getValue params 1) -- Required
       , Param "datetime_end"   False (getValue params 2)
       , Param "new_rate"       False (getValue params 3)
       , Param "status"         False (getValue params 4)  
       ]

updateTrackingLink :: [Text] -> Call
updateTrackingLink params = 
  Call "offer_offer"
       "updateOfferSchedule"
       "POST"
       [ Param "offer_id"     True (getValue params 0) -- Required
       , Param "tracking_url" True (getValue params 1) -- Required  
       ]
