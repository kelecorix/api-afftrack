module Afftrack.API.Types where

import -- Aeson

type Offer =
  Offer { program_pid :: Int
        , program_mid :: Int
        , program_name:: Text
        , program_name_private::Int  
        }
