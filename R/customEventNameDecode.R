## "121212" -> "Custom Name".
customEventNameDecode <- function(
  account,
  event
) {
  if("customEvents" %in% names(account)) {
    ind <- which(event == account$customEvents$custom_event_id)
    if(length(ind))
      event <- account$customEvents$event_name[ind] 
  }
  event
}
