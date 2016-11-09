\name{print.retention}
\alias{print.retention}

\title{Print Retention Matrix}

\description{Print Retention Matrix as counts or percentages.}

\usage{\method{print}{retention}(x, percentages=TRUE, silent=FALSE, ...)}

\arguments{
\item{x}{Object of class \code{retention}, as loaded by \code{mixpanelGetRetention}.}
\item{percentages}{Boolean. Indicates if retain counts should be shown as percentages or raw count. default is \code{TRUE}, returning percentages (0-100).}
\item{silent}{If \code{TRUE}, the table is returned without printing anything.}
\item{...}{Not used.}
}

\value{Matrix consisting of one row for each cohort. The first column holds the cohort sizes, the following columns the counts for subsequent periods. If \code{percentages} is \code{TRUE}, the counts are percentages (except first column).}

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
print(ret, percentages=FALSE)
print(ret, percentages=TRUE)
}}

\keyword{mixpanel}
\keyword{addiction}
\keyword{retention}
