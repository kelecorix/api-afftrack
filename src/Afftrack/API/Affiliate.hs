module Afftrack.API.Affiliate
       ( createAffiliate
       , getAffiliate
       , getAffiliateCount
       , getAffiliateEmails
       , getAffiliatePaymentMethods
       , getAffiliatePaymentTerms
       , getAffiliateStatus
       , getAffiliateTypes
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

createAffiliate = 
  Call "aff_affiliate"
       "createAffiliate"
       "POST"
       [ Param "aff_manager" True ""   -- Required
       , Param "company"  True ""   -- Required
       , Param "country"  True ""   -- Required
       , Param "email"  True ""   -- Required
       , Param "address"  False ""
       , Param "aim"  False ""
       , Param "break_frame"  False ""
       , Param "captcha"  False ""
       , Param "cb_email"  False ""
       , Param "city"  False ""
       , Param "description"  False ""
       , Param "first"  False ""
       , Param "intense_proxy"  False ""
       , Param "last"  False ""
       , Param "link_append"  False ""
       , Param "maxmind"  False ""
       , Param "network"  False ""
       , Param "ow"  False ""
       , Param "pay_method"  False ""
       , Param "pay_term"  False ""
       , Param "phone"  False ""
       , Param "postal"  False ""
       , Param "select"  False ""
       , Param "skype"  False ""
       , Param "state"  False ""
       , Param "status"  False ""
       , Param "tx"  False ""
       , Param "type"  False ""
       , Param "w9"  False ""
       , Param "yahoo"  False ""  
       ]

getAffiliate = 
  Call "aff_affiliate"
       "getAffiliate"
       "GET"
       [ Param "affiliate_id" False ""
       , Param "limit"        False ""
       , Param "orderby"      False ""
       , Param "page"         False ""
       , Param "sort"         False ""
       , Param "status"       False ""  
       ]

getAffiliateCount = 
  Call "aff_affiliate"
       "getAffiliateCount"
       "GET"
       [ Param "admin_id"   False ""
       , Param "company"    False ""
       , Param "email"      False ""
       , Param "firstLast"  False ""
       , Param "status"     False ""
       , Param "type"       False ""
       , Param "website"    False ""  
       ]

getAffiliateEmails =
  Call "aff_affiliate"
       "getAffiliateEmails"
       "GET"
       [ Param "affiliate_id" True ""
       ]

getAffiliatePaymentMethods =
  Call "aff_affiliate"
       "getAffiliatePaymentMethods"
       "GET"
       [ Param "N/A" True ""
       ]

getAffiliatePaymentTerms =
  Call "aff_affiliate"
       "getAffiliatePaymentTerms"
       "GET"
       [ Param "N/A" True ""
       ]

getAffiliateStatus =
  Call "aff_affiliate"
       "getAffiliateStatus"
       "GET"
       [ Param "N/A" True ""
       ]

getAffiliateTypes =
  Call "aff_affiliate"
       "getAffiliateTypes"
       "GET"
       [ Param "N/A" True ""
       ]
