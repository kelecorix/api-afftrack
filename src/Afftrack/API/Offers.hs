{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Offers
       ( Offer(..)
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


