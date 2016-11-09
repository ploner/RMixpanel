\name{mixpanelGetData}
\alias{mixpanelGetData}

\title{General Method For Mixpanel API Requests}

\description{This method handles the authentication and download of data from the Mixpanel API as well as data persistance. The method is used by many of the more specialized methods of this package.}

\usage{
mixpanelGetData(account, method="export/", args, validitySeconds=60, verbose=TRUE, 
  loadFromFile=FALSE, fileName="", data=FALSE, retryCount=0)
}

\arguments{
  \item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
  \item{method}{API method (route).}
  \item{args}{Arguments as described in \url{https://mixpanel.com/help/reference/data-export-api}.}
  \item{validitySeconds}{Expiration of API request.}
  \item{verbose}{If \code{FALSE}, less information is shown on the console. }
  \item{loadFromFile}{If \code{TRUE}, persisted data is loaded instead of doing a http request.}
  \item{fileName}{File name to persist data (defaults to './temp.txt'). If supplied, account$dataPath is used as path.}
  \item{data}{If \code{FALSE}, data is downloaded, but not returned as function argument.}
  \item{retryCount}{How many times a failed request should be retried?}
}

\value{If \code{data==TRUE}, the method returns the response of the API request (raw character vector). Otherwise nothing is returned.}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
                                
mixpanelGetData(account, method="funnels/list/", args=list(), data=TRUE)
}}

\references{\url{https://mixpanel.com/help/reference/data-export-api}}

\author{Meinhard Ploner}

\keyword{authentication}
\keyword{mixpanel}

