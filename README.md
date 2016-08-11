## RMixpanel - Mixpanel API client for R


The package RMixpanel provides an interface from R to Mixpanel's API endpoints 
(see https://mixpanel.com/docs/api-documentation/data-export-api and https://mixpanel.com/help/reference/http). 
For the most frequently used API endpoints (segmentation, retention, engage, export, etc.) custom methods 
make the parameterization more convenient and do the conversion from JSON to a corresponding R data.frame or R matrix. Furthermore it is possible to update or delete user profiles.

### Features

- Authentication by constructing the necessary request URL using the Mixpanel project's API token, secret and key.
- Access to any of Mixpanel's Export API's by a general method (`mixpanelGetData`).
- Easy parameterization and result parsing for the following API requests:
  - `segmentation/`: get the segmentation matrix using `mixpanelGetSegmentation`. 
  - `retention/`: get the retention matrix using `mixpanelGetRetention`.
  - `addiction/`: get the addiction matrix using `mixpanelGetAddiction`.
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
- digest
- jsonlite
- uuid
- RCurl 


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
> mixpanelGetRetention(account, born_event="AppInstall", event="WatchedItem", 
                       from=20150701, to=20151101, unit="week", percentages=TRUE)
## Example output:
##            count         0         1         2         3       ... ...
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

More complex queries including logical operators and typecasts can be generated using the syntax described on [Mixpanel's documentation](https://mixpanel.com/docs/api-documentation/data-export-api#segmentation-expressions).

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
>   mixpanelUpdateProfile(account, distinctID, "$unset"="KPI1")
```

Delete all profiles where `KPI1` is not set:
``` r
> profiles = mixpanelGetProfiles(account, where='not properties["KPI1"]')
> distinctIDs = profiles[, "distinct_id"]
> for (distinctID in distinctIDs)
>   mixpanelUpdateProfile(account, distinctID, "$delete"="")
```

Add a random value between 1 and 10 called `bucket` to all people profiles:
``` r
> profiles = mixpanelGetProfiles(account)
> distinctIDs = profiles[, "distinct_id"]
> for (distinctID in distinctIDs)
>   mixpanelUpdateProfile(account, distinctID, "$set"=list(bucket=jsonlite::unbox(sample(10, 1))))
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


#### Perform a custom JQL Query 

The JQL Query language opens a wide spectrum of possibilities. Here we show only a very basic example - getting the event count per user ('distinct_id'). For more complex queries consult also the Mixpanel JQL API Reference: https://mixpanel.com/help/reference/jql/api-reference#api/transformations/reduce.   

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
