mixpanelGetFunnel <- function(
  account,
  funnel,             # Name <or> ID of the funnel.
  from,
  to=from,
  customEvents,       # If the funnel includes custom events, add a data.frame to assign 
                      # readable names using the format: 
                      #   data.frame(custom_event_id=c(121212, ...), event_name=c("Event One", ...))
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
  
  if(!missing("customEvents")) {
    ## Update all funnels.
    for(iFunnel in 1:length(res)) {
      funnel <- res[[iFunnel]]
      
      ## Replace ID with name.
      isCustom <- !is.na(funnel$custom_event)
      inds <- match(funnel$custom_event_id[isCustom], customEvents$custom_event_id)
      funnel$event[isCustom] <- funnel$goal[isCustom] <- customEvents$event_name[inds] 
      
      res[[iFunnel]] <- funnel
    }
    
  }
  
  res
}

