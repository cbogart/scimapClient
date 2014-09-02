\name{scimapRegister}
\alias{scimapRegister}
\title{
Send usage information to sci-net-map.isri.cmu.edu
}
\description{
Sends anonymous usage tracking information about R packages
to a server which allows authors of packages to track how
widely used and installed they are.  This is helpful
for demonstrating the usefulness of these packages to 
their employers and funding agencies.

To use this function, please place it at the very top level
of scripts or functions that you write, in such a way that
it's called ONCE for every time you run an analysis.  Please
don't put it inside a loop, or it will overcount your
usage.

This tracking is voluntary and anonymous.  To enable tracking
type \code{enableScimap()} from the interactive prompt; to disable
it type \code{disableScimap()}.  If tracking is disabled, this
function will do nothing.
}
\usage{
scimapRegister(programName = topCall)
}
\arguments{
  \item{programName}{
    Optionally you may register a name for your script; otherwise the
    name of your top-level function on the call stack will be used.
}
}
\details{
Sends a JSON record to sci-net-map.isri.cmu.edu, using a TCP/IP
connection to port 7778.

The record contains the following information:
\describe{
\item{account:}{ A string of 25 random digits unique to this installation of R }
\item{exec:}{ The name of the top-level function or executable }
\item{startEpoch:}{ The current time }
\item{platform:}{ The operating system, version, and hardware type }
\item{packages:}{ The output of system(), and packageVersion() for each package listed. }
}
}
\value{
Returns immediately with no value, whether enabled or disabled.
}
\author{
Christopher Bogart <cbogart@cs.cmu.edu>
}
\references{
See your usage and others' at \url{http://sci-net-map.isri.cmu.edu}
}

\seealso{
\code{\link{enableScimap}}
\code{\link{disableScimap}}
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
