{-# LANGUAGE OverloadedStrings #-}
module Afftrack.API.AdminManager
       ( getAdmins
       , getAdminStatus  
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

getAdmins = 
  Call "admin_managers"
       "getAdmins"
       "GET"
       [ Param "limit"   False "" 
       , Param "orderby" False ""
       , Param "page"    False ""
       , Param "sort"    False ""
       , Param "status"  False ""  
       ]

getAdminStatus =
  Call "admin_managers"
       "getAdminStatus"
       "GET"
       [
       ]
