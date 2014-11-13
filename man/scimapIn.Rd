\name{scimapIn}
\alias{scimapOut}
\alias{scimapIn}
\title{
Instrument functions using scimap
}
\description{
For package authors who would like to instrument specific functions
within their packages. You may want to track just major functions that
are called once at a top level by users; 
instrumenting functions that are called recursively, or that
are called many many times in loops can cause a performance hit.

Call scimapIn() at the top of a function, and scimapOut() at the
bottom, to instrument its use.

Users who install your package will then not need to specifically
take steps to enable scimapClient: calling these functions will
collect usage data without their intervention.  (They can still
disable this by calling \code{\link{disableScimap}} just one time from the
R command line)
}
\usage{
scimapIn(caller)
scimapOut(caller)
}
\arguments{
  \item{caller}{Optional; the name of the function being instrumented.  If not supplied, the 
functions can determine this information on their own; but specify it manually if the function
name is not the way you want it recorded; for example if there are duplicate names for local
functions in different contexts that you want to track separately.}
}
\value{
Does not return a value.
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
\dontrun{
require(scimapClient)

yourFunction <- function() {
   scimapIn()
   ...your code here...
   scimapOut()
}

tempname <- function() {
   scimapIn("MoreMemorableName")
   ...your code here...
   scimapOut("MoreMemorableName")
}
}
}


