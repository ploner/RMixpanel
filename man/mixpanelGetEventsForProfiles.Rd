\name{mixpanelGetEventsForProfiles}
\alias{mixpanelGetEventsForProfiles}

\title{Get Events For Specified Profiles}

\description{Get a matrix filled with event data for specified people profiles.}

\usage{mixpanelGetEventsForProfiles(account, distinctIDs, from, to, verbose, encoding="UTF-8", ...)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{distinctIDs}{Array of profile IDs.}
\item{from}{Date in format yyyy-mm-dd, where month number starts with 0.}
\item{to}{Date in format yyyy-mm-dd, where month number starts with 0.}
\item{verbose}{Parameter passed to \code{mixpanelGetData}. Boolean.}
\item{encoding}{JSON Encoding. Default is UTF8.}
\item{...}{List of additional arguments being included in the Mixpanel API request. E.g. limit=5, ...}
}

\value{A character matrix showing one event per line. The first column represents the name of the event, the other column the event properties.}

\author{Meinhard Ploner}

\seealso{
\code{\link{mixpanelGetProfiles}},
\code{\link{mixpanelGetEvents}}
}

\keyword{mixpanel}
\keyword{events}
