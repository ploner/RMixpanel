\name{mixpanelGetProfiles}
\alias{mixpanelGetProfiles}

\title{Get Profiles From Mixpanel API}

\description{Get a matrix filled with profiles (also called 'users' or 'people') taken from the Mixpanel People API. }

\usage{mixpanelGetProfiles(account, where="", select, maxPage=100000, verbose=TRUE)}

\arguments{
  \item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
  \item{where}{Expression, see \url{https://mixpanel.com/help/reference/data-export-api#segmentation-expressions}.}
  \item{select}{If \code{"select"} is an array of column names, only the specified properties are selected. If \code{TRUE}, all columns are selected.}
  \item{maxPage}{The API delivers usually about 1000 users per page. Setting this parameter can limit the response for testing purposes. A value of 9 returns page 0 to page 9, i.e. 10000 profiles.}
  \item{verbose}{If \code{FALSE} less output is generated.}
}

\value{A character matrix showing one profile per line. The first column represents the distinct ID of the profile, the other column the profile properties.  }

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
                                
profiles = mixpanelGetProfiles(account, where='properties["KPI1"] > 1.32', 
                             select=c("KPI1", "KPI2"))
print(profiles)
}}

\references{\url{https://mixpanel.com/help/reference/data-export-api#people-analytics}}

\author{Meinhard Ploner}


\seealso{\code{\link{mixpanelGetEvents}}}

\keyword{mixpanel}
\keyword{people}
