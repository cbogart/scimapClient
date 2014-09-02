\name{disableScimap}
\alias{disableScimap}
\title{
Revoke permission for usage tracking
}
\description{
Revokes permission for the \code{scimapRegister()} function to 
send usage statistics to \code{sci-net-map.isri.cmu.edu}.

This is run by hand in interactive mode; there is no
need to put in a script.  Once you have revoked permission,
the scimapClient package will not track your usage again
until you run enableScimap()
}
\usage{
disableScimap()
}
\value{
Does not return a value
}
\author{
Christopher Bogart <cbogart@cs.cmu.edu>
}
\references{
See your usage and others' at \url{http://sci-net-map.isri.cmu.edu}
}


\seealso{
\code{\link{scimapRegister}}
}
\examples{
## Type this in interactive mode if you change your mind about
## allowing the scimapClient package to collect usage data.
library(scimapClient)
\dontrun{disableScimap()}
}