mixpanelGetEventsFromFiles = function(
  account,
  from,
  to=from,
  eventNames=c(),       # Import only selected events?
  select=TRUE,          # Requested column names.
  blocksize=500000,     # Block size in reading files from disk.
  df=FALSE,             # Clean data and return data.frame instead of character matrix.
  verbose=TRUE
) {
  dates = createDateSequence(from, to)
  alldata = matrix(NA, 0, 0)
  
  for (date in dates) {
    if(verbose)
      cat("#########", date, "########", date(), "\n")
    
    txtFilePath = paste(account$dataPath, "/", "events-", date, ".txt", sep="")
    con <- file(txtFilePath, "r", blocking=TRUE)
    
    block = 1
    while (TRUE) {
      
      if(verbose)
        cat("### Scan block", block, "of events  -", date(), "\n")
      data = readLines(con, n=blocksize)
      if(length(data) == 0 || (length(data) == 1 && data == ""))
        break
      
      if(verbose)
        cat("### Subset, parse & merge   -", date(), "\n")
      if (length(eventNames) > 0) {
        inds = rep(FALSE, length(data))
        for (name in eventNames) 
          inds =  inds | grepl(name, data, fixed=TRUE)
        data = data[inds]
      }
      
      newdata = eventsJson2RMatrix(data, select)
      
      if (length(eventNames) > 0) {
        ## Only needed in the case event names were not handled by grep.
        ind = (newdata[, "event"] %in% eventNames)
        alldata = merge.matrix(alldata, newdata[ind, , drop=FALSE])
      } else {
        alldata = merge.matrix(alldata, newdata)
      }
      
      block = block + 1
    }
    close(con)
  }
  
  if(verbose)
    cat("### Flatten matrix            -", date(), "\n")
  if (nrow(alldata) > 0) {
    alldata <- getFlatMatrix(alldata)
    if(df) {
      alldata <- data.frame(alldata, check.names=FALSE, stringsAsFactors=FALSE)
      alldata$time <- as.numeric(alldata$time)
      if("EventTimestamp" %in% colnames(alldata))
        alldata$EventTimestamp <- suppressWarnings(as.numeric(alldata$EventTimestamp))
    }
  }
  
  if(verbose)
    cat("### Done.                     -", date(), "\n")
  alldata
}
