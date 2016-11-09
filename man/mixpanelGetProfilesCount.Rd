\name{mixpanelGetProfilesCount}
\alias{mixpanelGetProfilesCount}

\title{Get Profiles Count From Mixpanel API}

\description{Get the number of profiles (also called 'users' or 'people') conforming to the requested query from the Mixpanel People API. }

\usage{mixpanelGetProfilesCount(account, where = "", verbose=TRUE)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{where}{Expression, see \url{https://mixpanel.com/help/reference/data-export-api#segmentation-expressions}.}
\item{verbose}{If \code{TRUE} more output is generated.}
}

\value{The number of profiles conforming to the query.}

\seealso{\code{\link{mixpanelGetProfiles}}}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
                                
mixpanelGetProfilesCount(account, where='properties["KPI1"] > 1.32')
}}

\references{\url{https://mixpanel.com/help/reference/data-export-api#people-analytics}}

\author{Meinhard Ploner}

\keyword{mixpanel}
\keyword{people}
