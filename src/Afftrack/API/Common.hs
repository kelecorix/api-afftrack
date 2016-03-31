{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Afftrack.API.Common
       ( Call(..)
       , Param(..)  
       , buildParams
       , buildParams'  
       , buildRequest
       , buildRequest'
       , getValue  
) where

import GHC.Generics
import Afftrack.API.Types
import Data.Aeson
import Control.Applicative
import Network.HTTP.Client
import Data.Text as Text
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import Safe

--------------------------------------------------------------------------------

data Call =
  Call { target :: Text
       , action :: Text
       , meth   :: Text  
       , param  :: [Param] -- name, required, value
       } deriving (Generic, Show)

data Param =
  Param { name     :: Text
        , required :: Bool 
        , value    :: Text
        } deriving (Show)

-- | Convert action parameters
--   to concatenated stringx
buildParams :: [(Text, Text)] -> Text
buildParams [] = ""
buildParams p  =
  Text.concat $ buildParamsAux p
    where buildParamsAux []           = []
          buildParamsAux ((p1,p2):ps) =
            case Text.null p2 of
              True  -> buildParamsAux ps
              False ->
                case p2=="-" of
                  True  -> Text.concat ["&", p1, "="] : buildParamsAux ps
                  False -> Text.concat ["&", p1, "=", p2] : buildParamsAux ps

buildParams' :: [Param] -> Text
buildParams' [] = ""
buildParams' ps =
  Text.concat $ buildParamsAux ps
    where buildParamsAux  []                = []
          buildParamsAux ((Param n r v):ps) =
            case r of
              False ->
                case Text.null v of
                  True  -> buildParamsAux ps
                  False -> Text.concat ["&", n, "=", v] : buildParamsAux ps
              True  -> -- this is required field
                Text.concat ["&", n,"=", v] : buildParamsAux ps

buildRequest :: String -> String -> String -> RequestBody -> IO Request
buildRequest url ps meth body = do
  nakedRequest <- parseUrl (url++ps)
  return (nakedRequest { method = (BS.pack meth), requestBody = body })  

buildRequest' :: Auth -> Call -> RequestBody -> IO Request
buildRequest' a c b = undefined 

getValue :: [Text] -> Int -> Text
getValue params i = do 
  (!!) params i
      
