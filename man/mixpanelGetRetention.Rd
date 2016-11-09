\name{mixpanelGetRetention}
\alias{mixpanelGetRetention}

\title{Retention Report}

\description{Get retention data from Mixpanel API. The resulting object can be printed nicely using \code{print.retention}.}

\usage{mixpanelGetRetention(account, event, from, to, unit, retentionType, 
  intervalCount, verbose=TRUE, ...)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{event}{Event name. If empty, all events are returned.}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{unit}{Defaults to 'day'.}
\item{retentionType}{\code{birth} (=First time) or \code{compounded} (=Recurring). Defaults to 'birth'.}
\item{intervalCount}{Number of intervals per cohort to return.}
\item{verbose}{If \code{TRUE} more output is generated.}
\item{...}{ Additional arguments to Mixpanel API. E.g. \code{born_event='Event name'}  (needed for retention type 'birth'), or \code{born_where='properties["Prop name"]=="value"'}.}
}

\value{Object of class \code{retention} including 
\item{dates}{Start dates of each cohort.}
\item{cohortCount}{Size of cohorts (people count).}
\item{retainCount}{Number of retained people.}
\item{retainPerc}{Percentage of retained people.}
}

\references{\url{https://mixpanel.com/help/reference/data-export-api#retention}}

\author{Meinhard Ploner}

\seealso{\code{\link{mixpanelGetAddiction}}}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
mixpanelGetRetention(account, born_event="AppInstall", event="WatchedItem", 
                     from=20150701, to=20151101, unit="week")
}}

\keyword{mixpanel}
\keyword{addiction}
\keyword{retention}
