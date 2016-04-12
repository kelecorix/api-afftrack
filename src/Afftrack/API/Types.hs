{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Types
       ( Auth(..)
       , Resp(..)
       , Offer(..)
       , parseOffer  
       ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Control.Applicative

-----------------------------------------------------------------------------

data TrafficType = INCENT | NONINCENT

-- | Core parameters that builds up
--   auth url to acces methods
-- 
--   endpoint = "http://tracking.affiliate.net/apiv4/"
--   key      = "9bb137a344cdec76c5830a0ef6d2e38e"
--   api      =  endpoint ++ "?key=" ++ key ++ "&format=json"
data Auth =
  Auth { endpoint :: T.Text
       , key      :: T.Text
       , format   :: T.Text  
       } deriving (Show)

-- | Offer type used for Brand API
--  
data Offer =
  Offer { id         :: T.Text
        , name       :: T.Text
        , link       :: T.Text
        , linkStatus :: T.Text
        , payout     :: T.Text
        , cap        :: T.Text  
        , merchantId :: T.Text
        , destination:: T.Text  
        } deriving (Generic, Show)

parseOffer v =
  Offer <$> v .: "program_pid"          <*>
            v .: "program_name"         <*>
            v .: "program_preview_link" <*>
            v .: "program_link_status"  <*>
            v .: "program_adv_paying"   <*>
            v .: "program_daily_cap"    <*>
            v .: "program_mid"          <*>
            v .: "program_preview_link"

instance FromJSON Offer where
  parseJSON (Object v) = parseOffer v
  parseJSON _          = empty  

data Resp =
  Resp { datas   :: [Object]
       , success :: Bool
       , page    :: Maybe Int
       , total   :: Maybe Int
       , url     :: T.Text
       , pages   :: Maybe Int
       , limit   :: Maybe Int  
       } deriving (Generic, Show)

parseResponse v = 
    Resp <$> v .: "data"       <*>
             v .: "success"    <*>
             v .: "page"       <*>
             v .: "total"      <*>
             v .: "request_url"<*>
             v .: "total_pages"<*>
             v .: "limit"
             
instance FromJSON Resp where
  parseJSON (Object v) = parseResponse v
  parseJSON _          = empty  
