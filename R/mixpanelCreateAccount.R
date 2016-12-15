mixpanelCreateAccount <- function(
  name,         # Arbitrary name.
  token,        # Mixpanel token.
  key,          # API key of the Mixpanel project.
  secret,       # API Secret of the Mixpanel project.
  customEvents, # If you have custom events, add a data.frame to assign 
                # readable names using the format: 
                #   data.frame(custom_event_id=c(121212, ...), event_name=c("Event One", ...))
  mongoDBname,  # Used when imported into a MongoDB database. 
  dataPath,     # File path to store exported raw data.
  RDataPath     # File path to store R raw data.
  ) {
  obj = list(name=name, token=token, apiKey=key, apiSecret=secret)
  
  if (!missing(customEvents))
    obj$customEvents = customEvents
  
  if (!missing(mongoDBname))
    obj$mongoDBname = mongoDBname
  
  if (!missing(dataPath))
    obj$dataPath = dataPath
  
  if (!missing(RDataPath))
    obj$RDataPath = RDataPath
  
  class(obj) = "mixpanelAccount"
  obj
}
