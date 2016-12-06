mixpanelGetSegmentation <- function(
  account,
  event,                   # Must be included. E.g. 'Video Start'.
  from,
  to=from,
  unit="day",              # 
  type="unique",           # This can be "general", "unique", or "average".
  on='properties["$os"]',  # Array of up to 2 segmentation properties. An empty array returns un-segmented counts.
  action,                  # Could be "sum" or "average". If given, 1st property is aggregated by this function.
  verbose=TRUE,
  ...                      # Additional arguments to Mixpanel API.
) {
  args = list(...)
  args$event = event
  args$from_date = createDateSequence(from)
  args$to_date = createDateSequence(to)
  args$unit = unit
  args$type = type
  
  segmentDim = length(on)
  outDim = segmentDim + 1
  hasAction <- !missing("action")
  
  if (segmentDim > 2)
    stop("Up to 2 segmentation variables are handled by API.")
  
  if (hasAction) {
    args$action = action
    ## Convert to numeric for aggregation function SUM, AVG, ...
    on[1] = paste('number(', on[1], ')', sep='')
    ## Aggregation reduces dimension count.
    outDim = outDim - 1
  }
  
  if (segmentDim == 2) {
    args$inner = on[1]
    args$outer = on[2]
    data = mixpanelGetData(account, "segmentation/multiseg", args, data=TRUE, verbose=verbose)
    values = jsonlite::fromJSON(data)$data$values

  } else if(hasAction) {
    args$on = on 
    data = mixpanelGetData(account, "segmentation/sum", args, data=TRUE, verbose=verbose)
    values = list(jsonlite::fromJSON(data)$results)
    names(values) <- event
    
  } else {
    args$on = on 
    data = mixpanelGetData(account, "segmentation/", args, data=TRUE, verbose=verbose)
    values = jsonlite::fromJSON(data)$data$values
  }

  if (outDim == 3) {
    outerNames = names(values)
    innerNames = names(values[[1]])
    timeNames = names(values[[1]][[1]])
    
    kOuter = length(outerNames)
    kInner = length(innerNames)
    kTimes = length(timeNames)
    
    data = array(unlist(values), c(kTimes, kInner, kOuter), dimnames=list(timeNames, innerNames, outerNames))
    data[order(timeNames), , , drop=FALSE]
    
  } else { # outDim == 2 or 1.
    if(length(values) == 0)
      return(matrix(NA, 0, 0))
      
    timeLabels = names(values[[1]])
    n = length(timeLabels)
    groups = names(values)
    k = length(groups)
    
    data = matrix(unlist(values), n, k, byrow=FALSE, dimnames=list(timeLabels, groups))
    data[order(timeLabels), , drop=FALSE]
  }
}
