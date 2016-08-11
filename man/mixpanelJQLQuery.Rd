\name{mixpanelJQLQuery}
\alias{mixpanelJQLQuery}

\title{Query the Mixpanel JQL API}

\description{This method performs a JQL Query with some custom script as parameter. The query string can be given either as character vector or saved into a file and passed via file name.}

\usage{
mixpanelJQLQuery(account, jqlString, jqlScripts, path=".", columnNames, toNumeric=c())
}

\arguments{
  \item{account}{A mixpanel account, as defined in \code{mixpanelCreateAccount}.}
  \item{jqlString}{JQL script as string.}
  \item{jqlScripts}{List of JQL script file names.}
  \item{paths}{Paths to search JS files.}
  \item{columnNames}{Column names for the resulting data.frame. }
  \item{toNumeric}{Column indices which should be converted to numeric.}
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
                                
## Simple query: number of events for each 'distinct_id'.
jqlQuery <- '
function main() {
  return Events({
    from_date: "2016-01-01",
    to_date: "2016-12-31"
  })
  .groupByUser(mixpanel.reducer.count())
}'

res <- mixpanelJQLQuery(account, jqlQuery,
                        columnNames=c("distinctID", "Count"), toNumeric=2)
hist(res$Count)
}}

\references{\url{https://mixpanel.com/help/reference/jql}}

\author{Meinhard Ploner}

\keyword{JQL}
\keyword{query}
\keyword{mixpanel}
