\name{addUserMetadata}
\alias{addUserMetadata}
\alias{deanonymize}
\title{
Upload custom metadata to scisoft-net-map.isri.cmu.edu
}
\description{
The scimapClient package automatically uploads R usage and dependency
information about the packages you used, at the end of each R session,
or at the next prompt after an hour of idle time. 

These two functions optionally attach metadata to that packet.  
Currently the information

The addUserMetadata function lets you supply arbitrary information to be
associated with each R session.  It could be used to implement an online lab notebook,
helping you navigate and interpret an online record of your own work in R.
If you wish to keep your data anonymous, don't put anything in the metadata
that will allow others to identify you.

However, if you are willing to be identified,
the deanonymize function lets you associate your name, website, and publications
page (if you have one) with your R session. We may infer the same user for other 
R sessions run by the same R installation.  Deanonymizing is optional, 
and you can fill just some of the fields if you like.

The advantage of deanonymizing is that you can help package authors find 
literature that may have relied on their package. This can help
them justify the time they spend maintaining packages, or help them
allocate their time to supporting packages that are most useful to scientists.
}
\usage{
addUserMetadata(metadata)
deanonymize(name, webpage, pubspage)
}
\arguments{
 \item{metadata}{An R object (a list with named elements); this will be converted to JSON by \code{\link{toJSON}}}
 \item{name}{Your name, as shown in author lists (if you publish papers), or "" if you don't want to give a name}
 \item{webpage}{A website about you, or "" if you don't want to give a site}
 \item{pubspage}{A website that lists your publications, such as an academic publications list page or your Google Scholar citations page; or "" if you don't want to give a site}
}
\value{
Neither returns a value
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
\dontrun{library(scimapClient)}
\dontrun{addUserMetadata(list(project="Arrow trajectory analysis", version="v5.4"))}
\dontrun{deanonymize("Chris Bogart", "http://quetzal.bogarthome.net/", "http://scholar.google.com/citations?user=FQSWa4sAAAAJ&hl=en")}
}
