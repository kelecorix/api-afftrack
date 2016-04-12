{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Affiliate.Types
       ( Offer(..)  
       , parseOffer  
       ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Control.Applicative

-----------------------------------------------------------------------------

-- | Offer type used for Affiliate API
--  
data Offer =
  Offer { id            :: T.Text
        , name          :: T.Text
        , allow_incent  :: T.Text
        , tracking_type :: T.Text
        , converts_on   :: T.Text
        , comission     :: T.Text  
        , requirements  :: T.Text
        , status        :: T.Text
        , link          :: T.Text
        , program_type  :: T.Text
        , time_zone     :: T.Text  
        } deriving (Generic, Show)

parseOffer v =
  Offer <$> v .: "offer_id"            <*>
            v .: "offer_name"          <*>
            v .: "offer_allows_incent" <*>
            v .: "offer_tracking_type" <*>
            v .: "offer_converts_on"   <*>
            v .: "offer_commission"    <*>
            v .: "offer_requirements"  <*>
            v .: "offer_status"        <*>
            v .: "preview_link"        <*>
            v .: "offer_program_type"  <*>
            v .: "offer_time_zone"      

instance FromJSON Offer where
  parseJSON (Object v) = parseOffer v
  parseJSON _          = empty  
