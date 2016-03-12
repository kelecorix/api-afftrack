{-# LANGUAGE OverloadedStrings #-}
module Afftrack.API.Merchant
       ( addBrowserLanguageBlocked
       , createMerchant
       , getMerchant
       , getMerchantBlacklist  
       )
       where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Data.Text
import Afftrack.API.Common

--------------------------------------------------------------------------------

addBrowserLanguageBlocked = 
  Call "mer_merchant"
       "addMerchantBlacklist"
       "POST"
       [ Param "affiliate_id" True ""   -- Required
       , Param "merchant_id"  True ""   -- Required
       ]

createMerchant = 
  Call "mer_merchant"
       "createMerchant"
       "POST"
       [ Param "added_by"          True ""   -- Required
       , Param "admin_id"          True ""   -- Required
       , Param "billing_company"   True ""   -- Required
       , Param "billing_email"     True ""   -- Required
       , Param "company"           True ""   -- Required
       , Param "email"             True ""   -- Required
       , Param "address"           False ""
       , Param "address2"          False ""
       , Param "aim"               False ""
       , Param "billing_address"   False ""
       , Param "billing_address2"  False ""
       , Param "billing_aim"       False ""
       , Param "billing_cell"      False ""
       , Param "billing_city"      False ""
       , Param "billing_contact"   False ""
       , Param "billing_country"   False ""
       , Param "billing_fax"       False ""
       , Param "billing_phone"     False ""
       , Param "billing_postal"    False ""
       , Param "billing_skype"     False ""
       , Param "billing_state"     False ""
       , Param "cell"              False ""
       , Param "city"              False ""
       , Param "contact"           False ""
       , Param "country"           False ""
       , Param "fax"               False ""
       , Param "mailing_address"   False ""
       , Param "mailing_address2"  False ""
       , Param "mailing_city"      False ""
       , Param "mailing_country"   False ""
       , Param "mailing_postal"    False ""
       , Param "mailing_state"     False ""
       , Param "phone"             False ""
       , Param "portal_notes"      False ""
       , Param "portal_notes2"     False ""
       , Param "portal_notes3"     False ""
       , Param "portal_password"   False ""
       , Param "portal_password2"  False ""
       , Param "portal_password3"  False ""
       , Param "portal_url"        False ""
       , Param "portal_url2"       False ""
       , Param "portal_url3"       False ""
       , Param "portal_username"   False ""
       , Param "portal_username2"  False ""
       , Param "portal_username3"  False ""
       , Param "postal"            False ""
       , Param "skype"             False ""
       , Param "state"             False ""         
       ]

getMerchant =
  Call "mer_merchant"
       "getMerchant"
       "GET"
       [ Param "limit"       False ""   
       , Param "merchant_id" False ""  
       , Param "orderby"     False ""
       , Param "page"        False ""
       , Param "sort"        False ""
       , Param "status"      False ""
       ]  

getMerchantBlacklist =
  Call "mer_merchant"
       "getMerchant"
       "GET"
       [ Param "merchant_id" True  "" ]  
  
