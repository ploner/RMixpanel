## "Custom Name" -> "$custom_event:######".
customEventNameEncode <- function(
  account,
  event
  ) {
  if("customEvents" %in% names(account)) {
    code <- account$customEvents$custom_event_id[account$customEvents$event_name == event]
    if(length(code))
      event <- paste0("$custom_event:", code)
  }
  event
}
