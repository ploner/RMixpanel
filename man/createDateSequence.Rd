\name{createDateSequence}
\alias{createDateSequence}

\title{Generate And Normalize Sequence Of Dates.}

\description{Generate a range of dates and normalize it. Used by other methods of this package.}

\usage{createDateSequence(from, to=from)}

\arguments{
  \item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
  \item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
}

\value{Array of daily dates in format yyyy-mm-dd as needed for use in Mixpanel's API.'}

\references{\url{https://mixpanel.com/help/reference/data-export-api}}

\author{Meinhard Ploner}

\keyword{mixpanel}

