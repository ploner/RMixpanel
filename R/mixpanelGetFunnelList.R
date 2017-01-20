mixpanelGetFunnelList <- function(
  account,
  verbose=TRUE
) {
  data = mixpanelGetData(
    account, "funnels/list/", args=list(), data=TRUE, verbose=verbose)
  data = jsonlite::fromJSON(data)
  data
}
