mixpanelGetFunnel <- function(
  account,
  funnel,                 # Name <or> ID of the funnel.
  from,
  to=from,
                          # the first step in the funnel. May not be greater than 90 days.
  ...                     # Additional arguments to Mixpanel API. E.g.
                          # >> interval=5
                          # >> unit="week"
                          # >> len=90
) {
  ## Extract funnel ID when <funnel> is name instead of ID.
  funnelList <- mixpanelGetFunnelList(account)
  if(!funnel %in% funnelList$funnel_id)
    funnel <- funnelList$funnel_id[funnelList$name == funnel]
  
  args = list(...)
  args$funnel_id <- funnel
  args$from_date = createDateSequence(from)
  args$to_date = createDateSequence(to)
  
  data = mixpanelGetData(account, "funnels/", args, data=TRUE)
  
  ## Returns a list of funnels per time interval.
  data = jsonlite::fromJSON(data)
  res <- lapply(data$data, "[[", "steps")
  class(res) <- c("funnel", "list")
  res
}

