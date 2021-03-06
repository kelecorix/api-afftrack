{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Affiliate.Affiliate where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Data.Text
import Afftrack.API.Common
import Afftrack.API.Types

--------------------------------------------------------------------------------

-- | This action will return the stats for all campaigns you are running for the time period provided.
-- 
getStatsSummary :: [Text] -> Call 
getStatsSummary params = 
  Call ""
       "stats_summary"
       "GET"
       [ Param "sd"  True (getValue params 0)
       , Param "sm"  True (getValue params 1)
       , Param "sy"  True (getValue params 2)
       , Param "ed"  True (getValue params 3)
       , Param "em"  True (getValue params 4)
       , Param "ey"  True (getValue params 5)
       , Param "ey"  True (getValue params 6)
       ]

-- | This action will return detailed conversion information for the time period.
-- 
getStatsDetail :: [Text] -> Call 
getStatsDetail params = 
  Call ""
       "stats_detail"
       "GET"
       [ Param "sd"  True (getValue params 0)
       , Param "sm"  True (getValue params 1)
       , Param "sy"  True (getValue params 2)
       , Param "ed"  True (getValue params 3)
       , Param "em"  True (getValue params 4)
       , Param "ey"  True (getValue params 5)
       , Param "ey"  True (getValue params 6)
       , Param "pid" True (getValue params 7)
       ]

-- | This action will return a detailed listing of campaigns and the parameters for each campaign
-- including page requested, count requested, total count of offers available, offer_id, offer_name,
-- offer_category, offer_allows_incent, offer_traffic_allowed, offer_tracking_type, offer_converts_on,
-- offer_commission, offer_requirements, preview_link.
getOffers :: [Text] -> Call 
getOffers params = 
  Call ""
       "offers"
       "GET"
       [ Param "keyword"    False (getValue params 0)
       , Param "id"         False (getValue params 1)
       , Param "page"       False (getValue params 2)
       , Param "limit"      False (getValue params 3)
       , Param "cat"        False (getValue params 4)
       , Param "type"       False (getValue params 5)
       , Param "coverts_at" False (getValue params 6)
       ]

-- | This action will return a detailed listing of all private campaigns you have access to and
-- the parameters for each campaign including page requested, count requested, total count of offers
-- available, offer_id, offer_name, offer_category, offer_allows_incent, offer_traffic_allowed,
-- offer_countries_allowed, offer_tracking_type, offer_converts_on, offer_commission, offer_requirements,
-- preview_link.
getPrivateOffers :: [Text] -> Call 
getPrivateOffers params = 
  Call ""
       "private_offers"
       "GET"
       [ Param "keyword"    False (getValue params 0)
       , Param "id"         False (getValue params 1)
       , Param "page"       False (getValue params 2)
       , Param "limit"      False (getValue params 3)
       , Param "cat"        False (getValue params 4)
       , Param "type"       False (getValue params 5)
       , Param "coverts_at" False (getValue params 6)
       ]

-- | This action will return the status of an offer request i.e. Approved, Pending or Denied.
-- You can submit an explanation of how you will run traffic to this offer with the answer parameter.
-- If Denied, you can re-apply with a new answer.
getOfferRequestStatus :: [Text] -> Call 
getOfferRequestStatus params = 
  Call ""
       "offer_request"
       "GET"
       [ Param "id"     True (getValue params 0)
       , Param "answer" False (getValue params 1)
       ]

-- | Return all device targeting related to offer ID
-- 
getOfferTargeting :: [Text] -> Call 
getOfferTargeting params = 
  Call ""
       "offer_targeting"
       "GET"
       [ Param "id"  True (getValue params 0)
       ]

-- | Returns all countries related to offer ID
-- 
getOfferCountries :: [Text] -> Call 
getOfferCountries params = 
  Call ""
       "offer_countries"
       "GET"
       [ Param "id"  True (getValue params 0)
       ]

-- | Returns all traffic types related to offer ID
-- 
getOfferTrafficTypes :: [Text] -> Call 
getOfferTrafficTypes params = 
  Call ""
       "offer_traffic_types"
       "GET"
       [ Param "id"  True (getValue params 0)
       ]

-- | Returns all categories related to offer ID
-- 
getOfferCategories :: [Text] -> Call 
getOfferCategories params = 
  Call ""
       "offer_categories"
       "GET"
       [ Param "id"  True (getValue params 0)
       ]

-- | Returns all budget caps related to offer ID
-- 
getOfferCaps :: [Text] -> Call 
getOfferCaps params = 
  Call ""
       "offer_caps"
       "GET"
       [ Param "id"  True (getValue params 0)
       ]

-- | This action will return a listing of Category IDs that can be used in conjunction
-- with the "cat" parameter on the offers API.
getOffersCategories :: [Text] -> Call 
getOffersCategories params = 
  Call ""
       "offers_cats"
       "GET"
       [
       ]

-- | This action will return a listing of Country Codes that are used in conjunction
-- with the "offer_countries" parameter on the offers API.
getOffersCountries :: [Text] -> Call 
getOffersCountries params = 
  Call ""
       "offers_countries"
       "GET"
       [
       ]

-- | This action will return a listing of targets that are used in conjunction with the "offer_targeting" parameter on the offers API.
getOffersTargets :: [Text] -> Call 
getOffersTargets params = 
  Call ""
       "offers_targets"
       "GET"
       [
       ]

-- | This action will return a listing of Traffic Type IDs that can be used in conjunction with the "type" parameter on the offers API.
getOffersTrafficTypes :: [Text] -> Call 
getOffersTrafficTypes params = 
  Call ""
       "offers_traffic_types"
       "GET"
       [
       ]       

-- | This action will return a listing of Status IDs that can be used in conjunction with the "status" parameter on the offers API.
getOffersStatuses :: [Text] -> Call 
getOffersStatuses params = 
  Call ""
       "offers_statuses"
       "GET"
       [
       ]

-- | This action will return a listing of Converts At IDs that can be used in conjunction with the "converts_at" parameter on the offers API.
getOffersConverts :: [Text] -> Call 
getOffersConverts params = 
  Call ""
       "offers_converts_at"
       "GET"
       [
       ]

-- | This action will return a plain text string of either ACTIVE or INACTIVE
offerCheck :: [Text] -> Call 
offerCheck params = 
  Call ""
       "offers_check"
       "GET"
       [ Param "pid"  True (getValue params 0)
       ]

-- | This action will return current cap usage for an offer if a cap is in place for that offer.
getOfferCap :: [Text] -> Call 
getOfferCap params = 
  Call ""
       "cap"
       "GET"
       [ Param "pid"  True (getValue params 0)
       ]

-- | This action will return a json string with the banner, text and HTML creatives.
getOfferCreatives :: [Text] -> Call 
getOfferCreatives params = 
  Call ""
       "offer_creatives"
       "GET"
       [ Param "pid"  True (getValue params 0)
       ]

-- | This action will return a json string with the banner, text and HTML creatives.
getOffersCreativesEmail :: [Text] -> Call 
getOffersCreativesEmail params = 
  Call ""
       "offers_email"
       "GET"
       [ Param "pid"  True (getValue params 0)
       ]
