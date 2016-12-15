\name{mixpanelCreateAccount}
\alias{mixpanelCreateAccount}

\title{Create Mixpanel Account}

\description{Create object holding meta data of a Mixpanel project.}

\usage{mixpanelCreateAccount(name, token, key, secret, customEvents, 
  mongoDBname, dataPath, RDataPath)}

\arguments{
  \item{name}{Can be arbitrary.}
  \item{token}{API token of Mixpanel project.}
  \item{key}{API key of Mixpanel project.}
  \item{secret}{API secret of Mixpanel project.}
  \item{customEvents}{If you have custom events, add a \code{data.frame} to assign readable names using the format: \code{data.frame(custom_event_id=c(121212, ...), event_name=c("Event One", ...))}.}
  \item{mongoDBname}{Optional. Name of data base for usage in mongoDB. Used in future versions of the package. }
  \item{dataPath}{Optional. Path to store raw data of events and people.}
  \item{RDataPath}{Optional. Path to store R rda files.}
}

\value{An object holding all the values specified as parameters.}

\author{Meinhard Ploner}

\seealso{
\code{\link{mixpanelGetEvents}}, 
\code{\link{mixpanelGetProfiles}}, 
\code{\link{mixpanelGetSegmentation}}, 
\code{\link{mixpanelGetRetention}}
}

\keyword{mixpanel}
\keyword{account}
