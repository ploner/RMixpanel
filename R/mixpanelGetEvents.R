mixpanelGetEvents <- function(
  account, 
  event,               # Array of event names. If empty, all events are returned.
  from,                # Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
  to=from,             # End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
  
  daysPerBlock=10,     # Choose a smaller value if too much events.
  select=TRUE,         # If a vector of column names, only specified columns are selected.
  verbose=TRUE,        # Level of verbosity.
  df=FALSE,            # Clean data and return data.frame instead of character matrix.
  encoding="UTF-8",    # See fork https://github.com/jzking/RMixpanel.
  ...                  # Additional arguments to Mixpanel API beside of <event>, <date_from>, <date_to>
                       # E.g. where='properties["$os"]=="iPhone OS"'
) {
  args = list(...)
  if (!missing(event))
    args$event = arrayRtoJSON(event)
  
  dates = createDateSequence(from, to) 
  alldata = matrix(NA, 0, 0)
  
  while (TRUE) {
    if (length(dates) == 0)
      break
    
    n = min(daysPerBlock, length(dates))
    args$from_date = dates[1]
    args$to_date = dates[n]
    
    if(verbose)
      cat("*** Load events from", dates[1], "to", dates[n], "\n")
    data = mixpanelGetData(account, "export/", args, data=TRUE, verbose=verbose, encoding=encoding)
    if(length(data) > 0)
      alldata = merge.matrix(alldata, eventsJson2RMatrix(data, select))
    
    dates = dates[-(1:n)] # Update dates for next iteration.
  }
  if(verbose)
    cat(".\n")
  
  if (nrow(alldata) > 0) {
    alldata <- getFlatMatrix(alldata)
    if(df) {
      alldata <- data.frame(alldata, check.names=FALSE, stringsAsFactors=FALSE)
      alldata$time <- as.numeric(alldata$time)
      if("EventTimestamp" %in% colnames(alldata))
        alldata$EventTimestamp <- as.numeric(alldata$EventTimestamp)
    }
  }
  
  alldata
}
