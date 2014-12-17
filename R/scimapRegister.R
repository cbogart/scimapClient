#' @docType package
#' @name scimapClient
#' @title Collects and reports usage data on R packages
#' @details
#' Authors of scientific software packages do not always have
#' good ways of documenting how widely used their work is, and
#' understanding how it is used in conjunction with other packages.
#' 
#' \code{scimapClient} allows users of these packages to help:
#' it sends anonymous usage tracking information about the R packages
#' you use to a server which allows anyone to see how
#' widely used and installed these packages are.  This is helpful
#' for demonstrating the usefulness of these packages to
#' their employers and funding agencies.
#' 
#' To disable, please call \code{\link{disableScimap}()} from the command line.
#' 
#' This tracking is voluntary and anonymous.  To disable tracking
#' type \code{\link{disableScimap}()} from the interactive prompt; to reenable
#' it type \code{\link{enableScimap}()}.  If tracking is disabled, this
#' function will do nothing.
#'
#' Please consider including this package by default all the time, by adding
#' the following to your .RProfile:
#'
#' \code{options(defaultPackages=c(getOption("defaultPackages"),"scimapClient"))}
#'
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#' @examples
#' ##
#' ##  Please insert this either at the top of each script,
#' ##  or better yet in your profile to automatically load each time
#' ##  you run R.
#' library(scimapClient)
#' 
#' ##  Call this to toggle scimap off if you do not want to send data.
#' \dontrun{scimapDisable()}
#' 
#' ##  Call this to reenable scimap
#' \dontrun{scimapEnable()}
#' 
#' ## Call this to see what kind of information is sent
#' previewPacket()
#' 
NULL


reghost = "scisoft-net-map.isri.cmu.edu"
regport = 7778

# Local variables.
jobinf <- new.env()
jobinf$jobID = ""
jobinf$stack = list()
jobinf$staticDeps = list()
jobinf$dynamicDeps = list()
jobinf$weakDeps = list()
jobinf$dynamicPackageDeps = list()
jobinf$weakPackageDeps = list()
jobinf$userMetadata = list()
jobinf$last = ""
jobinf$lastreporttime <- Sys.time()
jobinf$lastreportdeps <- list(pkgT=list(), dynDeps=list(), dynPackDeps=list(), weakDeps = list(), weakPackDeps=list(), userMetadata=list())
jobinf$startup <- 0
jobinf$sessionDisabled <- TRUE


#  title joinlists
#
#  description Combine two lists
#
#  details Merge lists with named fields, concatenating as lists the contents
#  of those fields.
#
#  examples
#
#     joinlists(list(x=1, y=list(3,4)), list(y=7, z=list(4,5)))
#
#   returns
#
#     list(x=1, y=list(3,4,7), z=list(4,5))
#
joinlists <- function(l1, l2) {
    headers <- unique(unlist(list(names(l1), names(l2))));
    newdata <- lapply(headers, function (k) unlist(list(l1[[k]],l2[[k]])));
    return (setNames(newdata, headers));
}

#' @title Add user metadata
#' 
#' @description Upload custom metadata to \code{scisoft-net-map.isri.cmu.edu}
#'
#' @details The \code{scimapClient} package automatically uploads R usage and dependency
#' information about the packages you used, at the end of each R session,
#' or at the next prompt after an hour of idle time.
#' 
#' These two functions optionally attach metadata to that packet.
#' 
#' The \code{addUserMetadata} function lets you supply arbitrary information to be
#' associated with each R session.  It could be used to implement an online lab notebook,
#' helping you navigate and interpret an online record of your own work in R.
#' If you wish to keep your data anonymous, don't put anything in the metadata
#' that will allow others to identify you.
#' 
#' @param metadata An R object (a list with named elements); 
#'     this will be converted to JSON by \code{\link{toJSON}}
#'
#' @examples
#' \dontrun{addUserMetadata(list(project="Arrow trajectory analysis", version="v5.4"))}
#'
addUserMetadata <- function(metadata) {
    jobinf$userMetadata <- joinlists(jobinf$userMetadata, metadata)
}

#' @title Associate personal identity with usage data
#'
#' @details
#' Adds name, webpage, and publications page metadata to the usage packet.
#' You could do this with \code{\link{addUserMetadata}}; this function simply 
#' uses standard metadata field names to do so.
#'
#' We (the operators of \code{scisoft-net-map.isri.cmu.edu}) may infer the same user for other
#' R sessions run by the same R installation.  Deanonymizing is optional,
#' and you can fill just some of the fields if you like.
#' 
#' The advantage of deanonymizing is that you can help package authors find
#' literature that may have relied on their package. This can help
#' them justify the time they spend maintaining packages, or help them
#' allocate their time to supporting packages that are most useful to scientists.
#'
#' @param name Your name, as shown in author lists (if you publish papers), 
#'     or "" if you don't want to give a name
#' @param webpage A website about you, 
#'     or "" if you don't want to give a site
#' @param pubspage A website that lists your publications, 
#'     such as an academic publications list page 
#'     or your Google Scholar citations page; 
#'     or "" if you don't want to give a site
#'
#' @examples
#' library(scimapClient)
#' \dontrun{deanonymize("Chris Bogart", "http://quetzal.bogarthome.net/", 
#'     "http://scholar.google.com/citations?user=FQSWa4sAAAAJ&hl=en")}
#'
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#' @seealso \code{\link{previewPacket}}
#'
deanonymize <- function(name, webpage, pubspage) {
    addUserMetadata(list("ssnm_name"=name, "ssnm_webpage"=webpage, "ssnm_pubspage"=pubspage))
}

#  
# Get a "JobID", which is unique and specific to this session.
#
getJobId <- function() {
   if (jobinf$jobID == "") {
       jobinf$jobID = paste(getScimapId(), 
                     as.character(as.integer(Sys.time())), 
                     sep="")
   }
   return(jobinf$jobID)
}

#
#  When this library is loaded, arrange to send a packet 
#    just before closing, OR when someone returns to a prompt
#    after an hour of idle time.
#
e <- new.env()
.onAttach <- function(lib, pkg, ...) {
    if (interactive() && !isEnabledScimap()) {
       packageStartupMessage("Package scimapClient is loaded, but disabled.")
       packageStartupMessage("Type enableScimap() to enable sending of anonymous summaries of what packages")
       packageStartupMessage("you use to ", reghost);
       packageStartupMessage("Type ?scimapClient for more information.")
    } else if (isEnabledScimap() && stats::runif(1) > .9) {
       packageStartupMessage("Package scimapClient is loaded and enabled.")
       packageStartupMessage("It will send anonymous summaries of what packages")
       packageStartupMessage("you use to ", reghost);
       packageStartupMessage("Type ?scimapClient for more information.")
       packageStartupMessage("disableScimap() to disable this package")
    }
}
.onLoad <- function(lib, pkg, ...) {
    jobinf$startup <- Sys.time()

    # Send a packet just before R shuts down
    reg.finalizer(e, function (obj) {
        thisreportdeps <- justDependencies()
        if (!identical(thisreportdeps, jobinf$lastreportdeps)) {
            scimapRegister(thisreportdeps, Sys.time(), quiet=FALSE)
        } 
        jobinf$lastreportdeps <- thisreportdeps
        jobinf$lastreporttime <- Sys.time()
    }, onexit=TRUE)

    # At R prompt, if it's been an hour since the last check,
    # then send a packet.  Trying to capture the situation where a user
    # leaves R running more or less permanently, and their "sessions" are
    # just bursts of activity separated by idle time.
    #
    # Note that if they're not deliberately unloading libraries, 
    # they'll keep building up, so the latter sessions are less of an 
    # accurate representation of what "goes together".
    tcm <- taskCallbackManager()
    reporter <- function(expr, value, ok, visible) {

        # Uncomment this to ignore sessions where the original packages
        #  loaded are the ONLY ones that ever load
        #
        #if (identical(jobinf$lastreportdeps, list())) {
        #    jobinf$lastreportdeps = justDependencies()
        #}

        thisreporttime <- Sys.time()
        if (thisreporttime-jobinf$lastreporttime > 3600) {  ##### Only check if it's been an hour since last prompt
            thisreportdeps <- justDependencies()
            if (!identical(thisreportdeps, jobinf$lastreportdeps)) {   ### Only send if something's changed
                scimapRegister(thisreportdeps, thisreporttime, quiet=TRUE)
            } 
            jobinf$lastreportdeps <- thisreportdeps
            jobinf$lastreporttime <- thisreporttime
        } 
        TRUE
    }
    tcm$add(reporter, name="reporter")
}

#' @title Enable tracking for this session
#'
#' @description Set the installation ID and enable tracking for this session
#'
#' @details This should be called from the user's .Rprofile; ideally the
#'   randomID value passed in should always be the same for a particular R
#'   user. 
#'
#'   Generate the code for .Rprofile by running enableScimap()
#'
enableTracking <- function(randomID) {
    jobinf$sessionDisabled = FALSE;
    jobinf$scimapID = randomID;
}

#' @title Get scimap unique/anonymous ID for your installation of R
#'
#' @description Return your unique installation ID for scimap usage tracking
#'
#' @details The scimapClient package identifies each installation of R with 
#' a unique ID to track usage/installation statistics.  
#'
#' This ID is made up of
#' a random but fixed number stored in your R package directory (in a file called
#' \code{scimap_unique_id}), hashed against
#' a few bits of your username: so it is unique to your username on your machine,
#' but your username or machine cannot be inferred from the ID.
#'
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#'
getScimapId <- function() {
   return(jobinf$scimapID);
}

generateScimapId <- function() {
    return(paste(sample(c(0:9),25,replace=TRUE),sep="",collapse=""));
}


#' @title Enable or revoke permission for usage tracking
#' @details Enable or revoke permission for the package to 
#'  send usage statistics to \code{scisoft-net-map.isri.cmu.edu}.
#'
#'  This is run by hand in interactive mode; there is no
#'  need to put in a script.  Once you have revoked permission with \code{disableScimap},
#'  the scimapClient package will not track your usage again
#'  until you run \code{enableScimap()}
#'
#'  You can temporarily revoke permission (just until R is stopped and restarted)
#'  with \code{disableScimapThisSession}.
#'
#'  (The \code{disableScimap} function saves a file called \code{scimap_permission_denied}
#'  in your R package directory so that the package will remember your revocation between sessions)
#'
#' @return Returns a string of 25 decimal digits.  This
#'    is a random but fixed number with no meaning.
#'    except as a unique identifier.
#'
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#'
#' @seealso \code{\link{previewPacket}}
#'
enableScimap <-
function() {
    if (!interactive()) {
       cat("The function enableScimap() only works in interactive mode.")
       return()
    }
    if (isEnabledScimap()) {
       cat("Scimap is already enabled.  To disable it, remove the scimap-related lines ",
           "from your profile file (", rProfileFile(), ")");
       return();
    }
    
cat("This package sends anonymous usage tracking information about R packages
to a server that allows authors of packages to track how
widely used and installed they are.  This is helpful
for demonstrating the usefulness of these packages to 
their employers and funding agencies.  You can see that data
online at ", reghost, "

This tracking is voluntary and anonymous. See the help page for
previewPacket() for more information (type: help(previewPacket) )
on what exactly is sent.

If you agree to this, a packet of information will be sent 
after every session; it will also add the
following code to your .Rprofile (", rProfileFile(), ") to
remember that you granted permission.

", rProfileCode(), "

")
    
    ok <- readline(paste("Is it OK to change .RProfile and send anonymous usage reports? (y/n) ", sep=""))
    if (substr(ok, 1, 1) == "y" || substr(ok, 1, 1) == "Y") {
       if (jobinf$sessionDisabled) {
           jobinf$sessionDisabled <- FALSE
       } 
       write(paste("\n\n", rProfileCode()), file=rProfileFile(), append=TRUE)
       cat("***Done! Usage reporting enabled.***\n")
    } else {
       cat("***Not enabled.  No usage reports will be sent.***\n")
    }
}

rProfileFile <- function() { return(file.path("~", ".Rprofile")); }

rProfileCode <- function() {
    return(paste("##BEGIN_ENABLE_SCIMAP
    options(defaultPackages=c(getOption(\"defaultPackages\"),\"scimapClient\"))

    setHook(packageEvent(\"scimapClient\", \"onLoad\"),
         function(libname, pkgname) {
              enableTracking(randomID=\"", generateScimapId(), "\"); } );
##END_ENABLE_SCIMAP", sep=""));
}


#' @describeIn enableScimap Permanently disable sending of packets
disableScimap <-
function() {
    cat("To disable usage reporting, remove the scimap-related lines ",
           "from your profile file (", rProfileFile(), ")");
    #cat("Usage reporting has been disabled.\nCall enableScimap() to renable it at any time.\n")
}

#' @describeIn enableScimap Disable sending packets just for this session (until R is closed and reopened)
disableScimapThisSession <- 
function() {
    jobinf$sessionDisabled <- TRUE
}


#' @title Does \code{scimapClient} have permission to track usage? 
#' 
#' @description Checks if the package has permission to track usage.  
#' 
#' @details
#' Permission can be granted or revoked with \code{\link{enableScimap}()}
#' or \code{disableScimap()} from the interactive prompt.
#' 
#' If permission is not given, the \code{scimapRegister()} function
#' returns silently without doing anything.  If permission is
#' granted, the package sends usage information, but also
#' returns silently.
#'
#' Tracking is automatically disabled if the environment variable R_TESTS is
#' set (even to ""); this prevents spurious packets from being sent during testing.
#' It's also disabled while packages are being installed (by checking for the
#' environment variable R_PACKAGE_NAME).
#'
#' @return boolean
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#' @seealso \code{\link{previewPacket}}
#' @examples 
#' ## Type this in interactive mode to see whether
#' ## the scimapClient package is enabled to collect usage data.
#' isEnabledScimap() 
#' # returns TRUE or FALSE

isEnabledScimap <-
function() {
    # Enabled if:
    #   - hasn't been temporarily disabled
    #   - we're not running tests.  Obviously, this means we can't test "isEnabledScimap" :-)  
    #   - we're not installing a package.  Prevents spurious packets during package installs
    return(!jobinf$sessionDisabled && 
           is.na(Sys.getenv("R_TESTS", unset=NA)) && 
           is.na(Sys.getenv("R_PACKAGE_NAME", unset=NA)))
}

#
# Look up version of a package and append to the name: 
#   appendVersion("stats") returns "stats/1.0.2"
#
appendVersion <- function(pkgname) {
    return (paste(pkgname, utils::packageVersion(pkgname), sep="/"));
}

#
# Returns a list where keys are all the currently loaded packages,
# and values are list of dependencies for each key
#
defaultPackages <- c(getOption("defaultPackages"), "base", "scimapClient", "RJSONIO", "tools")
getPackageDependencies <- function() {
        ip <- utils::installed.packages()
        deps = list()
        for (p in setdiff(loadedNamespaces(), defaultPackages)) {
            deps[[appendVersion(p)]] = 
               lapply( 
                  c(setdiff(tools::package_dependencies(p, db=ip)[[p]], defaultPackages)),
               appendVersion);
        }
        return(deps)
}

#
#  Returns just the package/dependency portion of the
#  packet that will be sent
#
justDependencies <- 
function() {
   deps <- list(
      pkgT = getPackageDependencies(),
      dynDeps = jobinf$dynamicDeps,
      dynPackDeps = jobinf$dynamicPackageDeps,
      weakDeps = jobinf$weakDeps,
      weakPackDeps = jobinf$weakPackageDeps,
      userMetadata = jobinf$userMetadata
   )
   return(deps)
}

#' @title Assemble usage information for transmission to \url{http://scisoft-net-map.isri.cmu.edu}
#'
#' @description Packages up anonymous usage tracking information about R packages
#' to a server which allows authors of packages to track how
#' widely used and installed they are.  This is helpful
#' for demonstrating the usefulness of these packages to
#' their employers and funding agencies.
#'
#' @details
#' You can call this function after running some of your
#' own code in order to see what kind of information scimapClient
#' will send to scisoft-net-map.isri.cmu.edu (as a packet over
#' a TCP/IP connection, to port 7778). If you don't like what
#' you see, you can disable the send by typing disableScimap() to
#' turn off this monitoring library.
#' 
#' This tracking is voluntary and anonymous.  To enable tracking
#' type \code{enableScimap()} from the interactive prompt; to disable
#' it type \code{disableScimap()}.  If tracking is disabled, this
#' function will do nothing.
#'
#' Creates a JSON record.
#' 
#' The record contains the following information:
#' \describe{
#' \item{account:}{ A string of 25 random digits unique to this installation of R }
#' \item{job:}{ The account number plus a start time }
#' \item{startEpoch:}{ The current time }
#' \item{platform:}{ The operating system, version, and hardware type }
#' \item{packages:}{ The output of system(), and packageVersion() for each package listed. }
#' \item{dependencies:}{ Static and dynamic dependencies between functions and packages }
#' }
#' @return Returns an object representing the information that would be sent
#' @references See your usage and others' at \url{http://scisoft-net-map.isri.cmu.edu}
#' @seealso \code{\link{enableScimap}}
#'
previewPacket <- function() {
    thisreportdeps <- justDependencies();
    time <- Sys.time();
    return(scimapPacket(thisreportdeps, time));
}

# 
# Assemble the packet with context information and package usage/dependencies
#
scimapPacket <-
function(deps, t) {
    unixtime <- as.character(as.integer(t))
    fmtTime <- format(t, "%a %b %d %H:%M:%S %Y")
    jobrec <- list(
          scimapInfoVersion = 3,
          user = getScimapId(),
          #bootEpoch = getBootEpoch(),
          startEpoch = as.character(as.integer(jobinf$startup)),
          startTime = format(jobinf$startup, "%a %b %d %H:%M:%S %Y"),
          endEpoch = unixtime,
          endTime = fmtTime,
          platform = list(
              system = Sys.info()[["sysname"]],
              rversion = paste("R",R.version[["major"]], ".", R.version[["minor"]], sep=""),
              version = Sys.info()[["release"]],
              hardware = Sys.info()[["machine"]]),
          jobID = getJobId(),
          pkgT = deps$pkgT,
          dynDeps = deps$dynDeps,
          dynPackDeps = deps$dynPackDeps,
          weakDeps = deps$weakDeps,
          weakPackDeps = deps$weakPackDeps,
          userMetadata = deps$userMetadata
    )
    return(RJSONIO::toJSON(jobrec));
}

#
#  Make a TCP connection to host "reghost" at port "regport", and
#  send the usage status packet
#
scimapRegister <- function(deps, thetime, quiet = FALSE) {
    topCall <- toString(sys.call(which=1)); 
    if (isEnabledScimap()) {
        result = tryCatch({
          a <- make.socket(reghost, regport)
          on.exit(close.socket(a))
          write.socket(a, scimapPacket(deps, thetime))
        }, error = function(e) {
          cat("scimapClient couldn't upload usage data to", reghost, "\n")
        })
    }
}
