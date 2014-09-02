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

To use, please call \code{enableScimap()} from the command line, just once, after
you install the package, to indicate consent.  Then place a call 
to the \code{scimapRegister()} function 
at the very top level of scripts or functions that you write, 
in such a way that it's called ONCE for every time you run an analysis, but
after packages are loaded (e.g. with \code{library()} or \code{require()}).  
Please don't put \code{scimapRegister()} inside a loop, or it will overcount your
usage.

This tracking is voluntary and anonymous.  To enable tracking
type \code{enableScimap()} from the interactive prompt; to disable
it type \code{disableScimap()}.  If tracking is disabled, this
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
See your usage and others' at \url{http://sci-net-map.isri.cmu.edu}
}

\keyword{ package }
\seealso{
\code{\link{scimapRegister}}
}
\examples{
library(scimapClient)

##  Call this the VERY first time you use it, to grant permission.
##  It only works in interactive mode, and is not necessary ever again.
\dontrun{scimapEnable()}

## Call this at the top of your main function
\dontrun{scimapRegister()}

## If your main function is called something uninteresting like "run_stuff", 
## you can include a more informative name:
\dontrun{scimapRegister(programName="BayesianGenomicAstroseismicAnalysis_v1.2")}
}
