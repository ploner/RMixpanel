mixpanelJQLQuery <- function(
  account,      # Mixpanel account.
  jqlString,    # Option (1): JQL script as string.
  jqlScripts,   # Option (2): List of JQL script file names.
  paths=".",    # Path to search JS files.
  columnNames,  # Column names for the resulting data.frame.
  toNumeric=c() # Column indices which should be converted to numeric.
) {
  ## Write all JQL code into 1 script file.
  filePath = paste("temp_", uuid::UUIDgenerate(), ".js", sep="")
  on.exit( { unlink(filePath) } )
  
  if(!missing(jqlString)) {
    cat(jqlString, file=filePath)
    
  } else {
    for(i in 1:length(jqlScripts))
      for(path in paths) {
        fn <- file.path(path, jqlScripts[i])
        if(file.exists((fn)))
          cat(readLines(fn), file=filePath, append=(i>1))
      }
  }
  
  ## Perform query by CURL.
  curlCall <- paste0("curl https://mixpanel.com/api/2.0/jql ",
                     "-u ", account$apiSecret, ": ",
                     "--data-urlencode script@", filePath)
  jsonRes <- system(curlCall, intern=TRUE)
  
  ## Parse to data.frame.
  rawRes <- jsonlite::fromJSON(jsonRes)
  res <- c()
  for(i in 1:length(rawRes)) {
    if (class(rawRes[[i]]) == "data.frame")
      res <- cbind(res, rawRes[[i]])
    else
      res <- cbind(res, unlist(rawRes[[i]]))
  }

  res <- as.data.frame(res, stringsAsFactors=FALSE)
  if(!missing(columnNames))
    colnames(res) <- columnNames
  
  if (length(toNumeric) && toNumeric[1] < 0)
    toNumeric <- (1:ncol(res))[toNumeric]
  for(i in toNumeric)
    res[, i] <- as.numeric(res[, i])
  res
}
