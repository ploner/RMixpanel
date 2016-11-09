\name{mixpanelGetAddiction}
\alias{mixpanelGetAddiction}

\title{Addiction Report}

\description{Get matrix of addiction data from Mixpanel API.}

\usage{mixpanelGetAddiction(account, event, from, to, unit, percentages, addictionUnit, ...)}

\arguments{
\item{account}{A mixpanel account, as created by \code{mixpanelCreateAccount}.}
\item{event}{Event name. If empty, all events are returned.}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{unit}{Defaults to 'day'.}
\item{percentages}{Output as counts or percentages?}
\item{addictionUnit}{Sub time unit for addiction calculation. }
\item{...}{ Additional arguments to Mixpanel API.}
}

\value{Matrix consisting of one row for each cohort. The first column holds the cohort sizes, the following columns the number of profiles having tracked events within the specified time interval.}

\references{\url{https://mixpanel.com/help/reference/data-export-api#retention}}

\author{Meinhard Ploner}

\seealso{\code{\link{mixpanelGetRetention}}}

\keyword{mixpanel}
\keyword{addiction}
\keyword{retention}
