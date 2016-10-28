mixpanelGetFunnelList <- function(
  account
) {
  data = mixpanelGetData(account, "funnels/list/", args=list(), data=TRUE)
  data = jsonlite::fromJSON(data)
  data
}

