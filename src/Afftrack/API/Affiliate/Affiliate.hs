module Afftrack.API.Affiliate.Affiliate where


--------------------------------------------------------------------------------

getStatsSummary :: [Text] -> Call 
getStatsSummary params = 
  Call ""
       "stats_summary"
       "GET"
       [ Param "sd"  True (getValue params 0)
       , Param "sm"  True (getValue params 1)
       , Param "sy"  True (getValue params 2)
       , Param "ed"  True (getValue params 3)
       , Param "em"  True (getValue params 4)
       , Param "ey"  True (getValue params 5)
       , Param "ey"  True (getValue params 6)
       , Param "pid" False (getValue params 7)
       ]

getOffers :: [Text] -> Call 
getOffers params = 
  Call ""
       "offers"
       "GET"
       [ Param "keyword"    False (getValue params 0)
       , Param "id"         False (getValue params 1)
       , Param "page"       False (getValue params 2)
       , Param "limit"      False (getValue params 3)
       , Param "cat"        False (getValue params 4)
       , Param "type"       False (getValue params 5)
       , Param "coverts_at" False (getValue params 6)
       ]

getPrivateOffers :: [Text] -> Call 
getPrivateOffers params = 
  Call ""
       "private_offers"
       "GET"
       [ Param "keyword"    False (getValue params 0)
       , Param "id"         False (getValue params 1)
       , Param "page"       False (getValue params 2)
       , Param "limit"      False (getValue params 3)
       , Param "cat"        False (getValue params 4)
       , Param "type"       False (getValue params 5)
       , Param "coverts_at" False (getValue params 6)
       ]

getOfferRequestStatus :: [Text] -> Call 
getOfferRequestStatus params = 
  Call ""
       "offer_request"
       "GET"
       [ Param "id"     True (getValue params 0)
       , Param "answer" True (getValue params 1)
       ]



