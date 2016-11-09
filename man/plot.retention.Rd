\name{plot.retention}
\alias{plot.retention}

\title{Plot Retention Rates}

\description{Plot Retention Rates as a line chart. Multiple chart types are available.}

\usage{\method{plot}{retention}(x, type="all", percentages=TRUE, omitLast=TRUE,
  colors, durations, y, ...)}

\arguments{
\item{x}{Object of class \code{retention}, as loaded by \code{mixpanelGetRetention}.}
\item{type}{Character vector. If "all", all chart types are plotted. Valid values are \code{"cohortsByDate"}, \code{"cohortsByAge"}, \code{"byDuration"}.}
\item{percentages}{Use retention counts or percentages? Default is \code{TRUE}.}
\item{omitLast}{Omit last value per series? Useful for Mixpanel data, because last value is usually based on partial data!}
\item{colors}{Colors for lines. Optional.}
\item{durations}{Selected Durations (index). Default is c(1, 2, 5, 10).}
\item{y}{Not used.}
\item{...}{Not used.}
}

\references{\url{https://mixpanel.com/help/reference/data-export-api#retention}}

\author{Meinhard Ploner}

\seealso{\code{\link{mixpanelGetRetention}}}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
ret = mixpanelGetRetention(account, born_event="AppInstall", event="WatchedItem", 
                     from=20150701, to=20151101, unit="week")
                     
par(mfrow=c(2, 2))
plot(ret, type="all")
par(mfrow=c(1, 1))
}}

\keyword{mixpanel}
\keyword{addiction}
\keyword{retention}
\keyword{plot}

