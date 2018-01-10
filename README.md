[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/RMixpanel)](https://cran.r-project.org/package=RMixpanel)

## RMixpanel - Mixpanel API client for R


The package RMixpanel provides an interface from R to Mixpanel's API endpoints 
(see https://mixpanel.com/help/reference/data-export-api and https://mixpanel.com/help/reference/exporting-raw-data). 
For the most frequently used API endpoints (segmentation, retention, funnel, engage, export, etc.) custom methods 
make the parameterization more convenient and do the conversion from JSON to a corresponding R data.frame or R matrix. Furthermore it is possible to update or delete user profiles.

### Features

- Authentication by constructing the necessary request URL using the Mixpanel project's API token, secret and key.
- Access to any of Mixpanel's Export API's by a general method (`mixpanelGetData`).
- Easy parameterization and result parsing for the following API requests:
  - `segmentation/`: get the segmentation matrix using `mixpanelGetSegmentation`. 
  - `retention/`: get the retention matrix using `mixpanelGetRetention`.
  - `addiction/`: get the addiction matrix using `mixpanelGetAddiction`.
  - `funnel/`: get funnel data using `mixpanelGetFunnel`.
  - `engage/`: 
    - get the requested people profiles using `mixpanelGetProfiles`.
    - update or delete a people profile using `mixpanelUpdateProfile`.
  - `stream/query/`: get events of selected people profiles using `mixpanelGetEventsForProfiles`.
  - `export/`: get event data as R matrix using `mixpanelGetEvents`.
  - `jql/`: perform custom queries using `mixpanelJQLQuery`.
- Get people profile count for custom queries using `mixpanelGetProfilesCount`. 
- Pagination for the endpoint `export/`. This allows querying data for long time spans using multiple requests.  
- Different levels of verbosity (log).
- Pre-selection of desired properties for event and profile requests. This lessens the amount of parsed data especially 
if the property count varies over requested events or people profiles. 

### Installation

``` r
require(devtools)
devtools::install_github("ploner/RMixpanel")
require(RMixpanel)
```

or

``` r
install.packages("RMixpanel")
require(RMixpanel)
```

### Dependencies

The package depends on
- jsonlite
- uuid
- RCurl 
- base64enc


### Examples

#### Set-up account in R

In order to use the various methods of this package, we need to save the account data of the Mixpanel Project into an R object of class `mixpanelAccount`. The next examples all make use of this account object. 

``` r
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
> account = mixpanelCreateAccount("ProjectName",
                                  token="c12f3...",
                                  secret="167e7e...", 
                                  key="553c55...")
> class(account)
[1] "mixpanelAccount"
```

#### Weekly retentions as percentages

``` r
> retentions <- mixpanelGetRetention(account, born_event="AppInstall", event="WatchedItem", 
                                     from=20150701, to=20151101, unit="week")
> print(retentions)
## Example output:
## Retention Matrix
## Row names are Cohort Start Dates. Column names are Periods (0 -> 0 to 1 units)
##            Count         0         1         2         3       ... ...
## 2015-06-29    17  94.11765 29.411765 29.411765 29.411765 ...
## 2015-07-06    38 100.00000 31.578947 18.421050 ...       
...
```

#### Number of people profiles matching some conditions

``` r
> mixpanelGetProfilesCount(account, where='properties["KPI1"] > 1.32')
## Example output:
## 21987   
```

#### Show histogram of KPI1 for selected people profiles 

Given the people profiles have two properties named KPI1 and KPI2, the following lines of code will load these properties for all profiles matching the query `KPI1 >= 1.32` and fill an R data.frame with the corresponding data. The `hist` method could be used to generate a histogram of one of the KPI's. 

More complex queries including logical operators and typecasts can be generated using the syntax described on [Mixpanel's documentation](https://mixpanel.com/help/reference/data-export-api#segmentation-expressions).

``` r
> profiles = mixpanelGetProfiles(account, where='properties["KPI1"] > 1.32', 
                               select=c("KPI1", "KPI2"))
> print(profiles)
## Example output:
##      distinct_id   KPI1    KPI2  
## [1,] "D1FED2..."    1.37   1.09 
## [2,] "4441C5..."    2.11  -0.12
## ...

> hist(as.numeric(profiles[, "KPI1"]))
```


#### Update or delete selected profiles

Remove property `KPI1` when the value is larger than 1000:
``` r
> profiles = mixpanelGetProfiles(account, where='properties["KPI1"] > 1000')
> distinctIDs = profiles[, "distinct_id"]
> for (distinctID in distinctIDs)
>   mixpanelUpdateProfile(account, distinctID, data=list("$unset"="KPI1"))
```

Delete all profiles where `KPI1` is not set:
``` r
> profiles = mixpanelGetProfiles(account, where='not properties["KPI1"]')
> distinctIDs = profiles[, "distinct_id"]
> for (distinctID in distinctIDs)
>   mixpanelUpdateProfile(account, distinctID, data=list("$delete"=""))
```

Add a random value between 1 and 10 called `bucket` to all people profiles:
``` r
> profiles = mixpanelGetProfiles(account)
> distinctIDs = profiles[, "distinct_id"]
> for (distinctID in distinctIDs)
>   mixpanelUpdateProfile(account, distinctID, 
      data=list("$set"=list(bucket=jsonlite::unbox(sample(10, 1)))))
```


#### Get funnel data by using the general export method

The general method `mixpanelGetData` allows to call all available API endpoints of the export API. However, the result is not parsed into R objects. Calling `jsonlite::fromJSON(data)` on the resulting data would do some parsing, but usually more 
postprocessing is needed. 

Here an example without transforming the resulting JSON into handy R objects:

``` r
## Get list of funnels.
> mixpanelGetData(account, method="funnels/list/", args=list(), data=TRUE)
## Example output:
## [1] "[{\"funnel_id\": 1011888, \"name\": \"My first funnel\"}, 
##       {\"funnel_id\": 1027999, \"name\": \"User journey funnel\"}]"
      
## Get data about a certain funnel.
> mixpanelGetData(account, method = "funnels/", args = list(funnel_id="1027999", unit="week"), 
                  data = TRUE)
## Example output:
## [1] "{\"meta\": {\"dates\": [\"2015-11-04\", \"2015-11-11\"]}, 
##   \"data\": {\"2015-11-11\": 
##    {\"steps\": [
##      {\"count\": 7777, \"step_conv_ratio\": 1, \"goal\": \"AppInstall\", \"overall_conv_ratio\":1, 
##        \"avg_time\": null, \"event\": \"AppInstall\"}, 
##      {\"count\": 555, \"avg_time\": 111, \"goal\": \"OpenedView\", \"overall_conv_ratio\": 0.77, 
##        \"selector\": \"(properties[\\\"status\\\"] == \\\"loggedin\\)\", 
##        \"step_conv_ratio\": 0.06964335860713283, \"event\": \"OpenedView\"}, 
##      {\"count\": 333, \"avg_time\": 222, ...
##   ...
```


#### JQL: simple in-line query 

The JQL Query language opens a wide spectrum of possibilities. As a simple example we extract the event count per user ('distinct_id'). The Mixpanel JQL API Reference can be found on https://mixpanel.com/help/reference/jql/api-reference.   

``` r
jqlQuery <- '
function main() {
  return Events({
    from_date: "2016-01-01",
    to_date: "2016-12-31"
  })
  .groupByUser(mixpanel.reducer.count())
}'

res <- mixpanelJQLQuery(account, jqlQuery,
                        columnNames=c("distinctID", "Count"), toNumeric=2)
hist(res$Count)
```

#### Get DAU using JQL 

Here we show how to calculate the metric Daily Active Users (DAU) when the user ID is different from the distinct_id. First write the JQL query and save it into a file named jqlDAU.js:

``` js
function today(addDays) {
  var day = new Date(); 
  day.setDate(day.getDate() + (addDays || 0));
  return day.toISOString().substr(0, 10);
}

function main() {
  return Events({
    from_date: today(dayFrom),
    to_date: today(dayTo)
  })
  .groupBy(["properties.UserID", getDay], function(count, events) {
    count = count || 0;
    return count + events.length;
  })
  .groupBy(["key.1"], mixpanel.reducer.count());
}
```

The parameters <dayFrom> and <dayTo> define the date range. As you may see, they are not defined in the JQL script. To be transparant, we add them directly in the final R call. Setting them to -7 and -1 gives the DAU values for the last 7 whole days:

``` r
mixpanelJQLQuery(account, jqlString="dayFrom=-7; dayTo=-1;", jqlScripts="jqlDAU.js")
```

