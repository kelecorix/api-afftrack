{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Types
       ( Auth(..)
       , Resp(..)
       , Offer(..)  
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

data Offer =
  Offer { id         :: T.Text
        , name       :: T.Text
        , link       :: T.Text
        , linkStatus :: T.Text
        , payout     :: T.Text
        , cap        :: T.Text  
        , merchantId :: T.Text
        } deriving (Generic, Show)

instance FromJSON Offer where
  parseJSON (Object v) =
    Offer <$> v .: "program_pid"          <*>
              v .: "program_name"         <*>
              v .: "program_preview_link" <*>
              v .: "program_link_status"  <*>
              v .: "program_adv_paying"   <*>
              v .: "program_daily_cap"    <*>
              v .: "program_mid"
    -- A non-Object value is of the wrong type, so fail.
  parseJSON _        = empty  

data Resp =
  Resp { datas   :: [Offer]
       , success :: Bool
       , page    :: Int
       , total   :: Int
       , url     :: T.Text
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
