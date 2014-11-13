\name{scimapClient-package}
\alias{scimapClient-package}
\alias{scimapClient}
\docType{package}
\title{
Collects and reports usage data on R packages
}
\description{
Authors of scientific software packages do not always have
good ways of documenting how widely used their work is, and
understanding how it is used in conjunction with other packages.

scimapClient allows users of these packages to help:
it sends anonymous usage tracking information about the R packages
you use to a server which allows anyone to see how 
widely used and installed these packages are.  This is helpful
for demonstrating the usefulness of these packages to 
their employers and funding agencies.

To disable, please call \code{disableScimap()} from the command line. 

This tracking is voluntary and anonymous.  To disable tracking
type \code{disableScimap()} from the interactive prompt; to reenable
it type \code{enableScimap()}.  If tracking is disabled, this
function will do nothing.
}
\details{
\tabular{ll}{
Package: \tab scimapClient\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2014-08-29\cr
License: \tab BSD\cr
}

}
\author{
Maintainer: Chris Bogart <cbogartdenver@gmail.com>
}
\references{
See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
}

\keyword{ package }
\seealso{
\code{\link{previewPacket}}
}
\examples{
##
##  Please insert this either at the top of each script,
##  or better yet in your profile to automatically load each time
##  you run R.
\dontrun{library(scimapClient)}

##  Call this to toggle scimap off if you do not want to send data.
\dontrun{scimapDisable()}

##  Call this to reenable scimap
\dontrun{scimapEnable()}

## Call this to see what kind of information is sent
\dontrun{previewPacket()}

}
