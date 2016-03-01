module Afftrack.API.Merchant
       ( addBrowserLanguageBlocked
       )
       where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS

import Afftrack.API.Common

--------------------------------------------------------------------------------

addBrowserLanguageBlocked = 
  Call "mer_merchant"
       "addMerchantBlacklist"
       "POST"
       [ Param "affiliate_id" True ""   -- Required
       , Param "merchant_id"  True ""   -- Required
       ]

