\name{mixpanelGetFunnelList}
\alias{mixpanelGetFunnelList}

\title{Funnel List}

\description{Get list of all defined funnels from Mixpanel API.}

\usage{mixpanelGetFunnelList(account, verbose=TRUE)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{verbose}{If \code{TRUE} more output is generated.}
}

\value{A data.frame with 1 row for each funnel. Columns include 
\item{funnel_id}{ID of the funnel.}
\item{name}{Name of the funnel.}
}

\references{\url{https://mixpanel.com/help/reference/data-export-api#funnels}}

\author{Meinhard Ploner}

\seealso{\code{\link{mixpanelGetFunnel}}}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
mixpanelGetFunnelList(account)
}}

\keyword{mixpanel}
\keyword{funnel}
