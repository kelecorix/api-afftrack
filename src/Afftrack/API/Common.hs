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

import           Afftrack.API.Types
import           Control.Applicative
import qualified Control.Exception as E
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import           Data.Text as Text
import           GHC.Generics
import           Network.HTTP.Client
import           Safe

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
getValue params i = fromMaybe "" $ safeNth i params
      
safeNth :: Int -> [a] -> Maybe a
safeNth 0 list = headMay list
safeNth pos (x:xs) | pos < 0   = Nothing
                   | otherwise = safeNth (pos-1) xs
