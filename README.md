# api-afftrack

This package is a set of binding to work with http://www.afftrack.com software build for mobile affiliate networks to help them manage advertisement offers.

This bindning support both types of API:
- Brand     - company internal API for integration with other services
- Affiliate - company external API for integration with other affiliate nwtworks

subdirs named respectivly, consider simple example of usage

```haskell
 
sendRequest :: Auth -> AffCom.Call -> HttpClient.RequestBody -> IO Resp
sendRequest auth call body =
  do
     manager  <- HttpClient.newManager HttpClient.defaultManagerSettings
     
     let api    = T.concat [endpoint auth,"?key=", key auth, "&format=json"]
         path   = case T.null (AffCom.target call) of
                    False -> T.concat [api,"&target=",(AffCom.target call),"&action=",(AffCom.action call)]
                    True  -> T.concat [api,"&action=", (AffCom.action call)]  -- Affiliate API
         params = AffCom.buildParams' $ AffCom.param call           
         query  = T.concat [path, params]
         
     request  <- AffCom.buildRequest (T.unpack path) (T.unpack params) (T.unpack $ AffCom.meth call) body
     response <- HttpClient.httpLbs request manager

     let mobj = eitherDecode (HttpClient.responseBody response)    
  
     case mobj of
       Left s ->
         do
           print "can't decode -\n"
           print $ s 
           return $ Resp [] False Nothing Nothing "" Nothing Nothing
       Right obj ->
         do
           let prs = obj :: Resp
           return $ prs
```

where call is name of method you wanted to use and param is parameters to use withing call, to be sure which one is which address official API or look into source code for more details