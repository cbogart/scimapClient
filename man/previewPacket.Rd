\name{previewPacket}
\alias{previewPacket}
\title{
Assemble usage information for transmission to scisoft-net-map.isri.cmu.edu
}
\description{
Packages up anonymous usage tracking information about R packages
to a server which allows authors of packages to track how
widely used and installed they are.  This is helpful
for demonstrating the usefulness of these packages to 
their employers and funding agencies.

You can call this function after running some of your
own code in order to see what kind of information scimapClient
will send to scisoft-net-map.isri.cmu.edu (as a packet over
a TCP/IP connection, to port 7778). If you don't like what
you see, you can disable the send by typing disableScimap() to 
turn off this monitoring library.

This tracking is voluntary and anonymous.  To enable tracking
type \code{enableScimap()} from the interactive prompt; to disable
it type \code{disableScimap()}.  If tracking is disabled, this
function will do nothing.
}
\usage{
previewPacket()
}
\details{
Creates a JSON record.

The record contains the following information:
\describe{
\item{account:}{ A string of 25 random digits unique to this installation of R }
\item{job:}{ The account number plus a start time }
\item{startEpoch:}{ The current time }
\item{platform:}{ The operating system, version, and hardware type }
\item{packages:}{ The output of system(), and packageVersion() for each package listed. }
\item{dependencies:}{ Static and dynamic dependencies between functions and packages }
}
}
\value{
Returns an object representing the information that woudl be sent
}
\author{
Christopher Bogart <cbogart@cs.cmu.edu>
}
\references{
See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
}

\seealso{
\code{\link{enableScimap}}
\code{\link{disableScimap}}
}
