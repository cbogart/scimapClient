\name{getScimapId}
\alias{getScimapId}
\title{
Return your unique installation ID for scimap usage tracking
}
\description{
\code{scimapRegister()} identifies each installation of R with
a unique ID to track usage/installation statistics.
}
\usage{
getScimapId()
}

\value{
Returns a string of 25 decimal digits.  This
is a random but fixed number with no meaning.
except as a unique identifier.
}

\author{
Christopher Bogart <cbogart@cs.cmu.edu>
}

\seealso{
\code{\link{scimapRegister}}
}
\examples{
## Type this in interactive mode if you change your mind about
## allowing the scimapClient package to collect usage data.
library(scimapClient)
getScimapId()

## Returns a value like "58340194758475847582"
}