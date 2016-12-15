\name{mixpanelGetFunnel}
\alias{mixpanelGetFunnel}

\title{Funnel}

\description{Get funnel data from Mixpanel API. Custom events are names accordingly, if \code{account} is set up correctly using \code{mixpanelCreateAccount.}}

\usage{mixpanelGetFunnel(account, funnel, from, to=from, verbose=TRUE, ...)}

\arguments{
\item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
\item{funnel}{Either name or ID of the funnel. See also \code{mixpanelGetFunnelList}.}
\item{from}{Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive. }
\item{to}{End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.}
\item{verbose}{If \code{TRUE} more output is generated.}
\item{...}{ Additional arguments to Mixpanel API. E.g. \code{interval=14}, \code{unit='week'}, \code{interval=90}, ...}
}

\value{Object of class \code{funnel} including a list of \code{data.frame}'s representing the funnels for each time frame.}

\references{\url{https://mixpanel.com/help/reference/data-export-api#funnels}}

\author{Meinhard Ploner}

\seealso{\code{\link{mixpanelGetFunnelList}}}

\examples{
\dontrun{
## Fill in here the API token, key and secret as found on 
## www.mixpanel.com - Account -> Projects. 
account = mixpanelCreateAccount("ProjectName",
                                token="c12g3...",
                                secret="168e7e...", 
                                key="543c55...")
mixpanelGetFunnel(account, "Funnel001", from=20161001, to=20161101, unit="week")
}}

\keyword{mixpanel}
\keyword{funnel}

