\name{isEnabledScimap}
\alias{isEnabledScimap}
\title{
Does \code{scimapClient} have permission to track usage? 
}
\description{
Checks if the \code{scimapRegister()} function has permission
to track usage.  

Permission can be granted or revoked with \code{enableScimap()}
or \code{disableScimap()} from the interactive prompt.

If permission is not given, the \code{scimapRegister()} function
returns silently without doing anything.  If permission is
granted, \code{scimapRegister()} sends usage information, but also
returns silently.
}
\usage{
isEnabledScimap()
}

\value{
Returns boolean
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
## Type this in interactive mode to see whether
## the scimapClient package is enabled to collect usage data.
library(scimapClient)
isEnabledScimap()
}