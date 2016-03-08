module Afftrack.API.Affiliate where
       ( createAffiliate
       , getAffiliate
       ) 
       where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS

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
       "POST"
       [ Param "affiliate_id" False ""
       , Param "limit"        False ""
       , Param "orderby"      False ""
       , Param "page"         False ""
       , Param "sort"         False ""
       , Param "status"       False ""  
       ]
