\name{enableScimap}
\alias{enableScimap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Enable Scimap reporting
}
\description{
Gives permission for the \code{scimapRegister()} function to 
send usage statistics to sci-net-map.isri.cmu.edu.

This only needs to be called once; not in every session.
It only works in interactive mode; do not call it from a 
script.
}
\usage{
enableScimap()
}
\value{
Does not return a value.
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
## Type this in interactive mode after installing this package, to enable it to
## send your usage data.  You never need to call it after that.
library(scimapClient)
\dontrun{enableScimap()}
}

