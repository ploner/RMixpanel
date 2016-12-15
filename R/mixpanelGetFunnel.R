mixpanelGetFunnel <- function(
  account,            # Mixpanel account with credentials and evt. custom event names. 
  funnel,             # Name <or> ID of the funnel.
  from,
  to=from,
  verbose=TRUE,       # Level of verbosity.
  ...                 # Additional arguments to Mixpanel API. E.g.
                      # >> interval=5
                      # >> unit="week"
                      # >> len=90
                      # the first step in the funnel. May not be greater than 90 days.
) {
  ## Extract funnel ID when <funnel> is name instead of ID.
  funnelList <- mixpanelGetFunnelList(account)
  if(!funnel %in% funnelList$funnel_id)
    funnel <- funnelList$funnel_id[funnelList$name == funnel]
  
  args = list(...)
  args$funnel_id <- funnel
  args$from_date = createDateSequence(from)
  args$to_date = createDateSequence(to)
  
  data = mixpanelGetData(account, "funnels/", args, data=TRUE, verbose=verbose)
  
  ## Returns a list of funnels per time interval.
  data = jsonlite::fromJSON(data)
  res <- lapply(data$data, "[[", "steps")
  class(res) <- c("funnel", "list")
  
  if("customEvents" %in% names(account)) {
    for(iFunnel in 1:length(res)) {
      funnel <- res[[iFunnel]]
      
      if("custom_event" %in% colnames(funnel)) {
        for(i in which(!is.na(funnel$custom_event)))
          funnel$event[i] <- funnel$goal[i] <- customEventNameDecode(account, funnel$custom_event_id[i]) 
      
        res[[iFunnel]] <- funnel
      }
    }
  }
  
  res
}

