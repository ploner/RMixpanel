\name{mixpanelGetSegmentation}
\alias{mixpanelGetSegmentation}

\title{Segmentation Report}

\description{Get a matrix of events segmented by 1 or 2 properties. Event data is taken from the Mixpanel Segmentation API.}

\usage{mixpanelGetSegmentation(account, event, from, to, unit, type, on, action, verbose, ...)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{event}{Event name. If empty, all events are returned. Custom events are supported, if \code{account} is set up correctly using \code{mixpanelCreateAccount.}}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{unit}{Defaults to 'day'.}
\item{type}{This can be "general", "unique", or "average".}
\item{on}{Array of up to 2 property names for segmentation. An empty array returns un-segmented counts. E.g. 'properties["$os"]'}.
\item{action}{Could be "sum" or "average". If given, 1st property listed in \code{on} is aggregated by this function.}
\item{verbose}{If \code{FALSE} less output is generated.}
\item{...}{Additional arguments to Mixpanel API. See also \url{https://mixpanel.com/help/reference/data-export-api#segmentation}.}
}

\value{2 or 3-dimensional array showing event counts. The first dimension represents the time in the specified unit. The other dimension(s) are determined by the segmention properties. Usually the Mixpanel API does not return more than 60 or 100 slots per segmentation property. For more data, use the events API (\code{mixpanelGetEvents}).}

\references{
\url{https://mixpanel.com/help/reference/data-export-api#segmentation}
}

\author{Meinhard Ploner}

\keyword{mixpanel}
\keyword{segmentation}

