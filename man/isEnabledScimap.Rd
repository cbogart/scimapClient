\name{isEnabledScimap}
\alias{isEnabledScimap}
\title{
Does \code{scimapClient} have permission to track usage? 
}
\description{
Checks if the package has permission
to track usage.  

Permission can be granted or revoked with \code{enableScimap()}
or \code{disableScimap()} from the interactive prompt.

If permission is not given, the \code{scimapRegister()} function
returns silently without doing anything.  If permission is
granted, the package sends usage information, but also
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
See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
}

\seealso{
\code{\link{previewPacket}}
}
\examples{
## Type this in interactive mode to see whether
## the scimapClient package is enabled to collect usage data.
\dontrun{library(scimapClient)}
\dontrun{isEnabledScimap()}
}
