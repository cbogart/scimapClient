
reghost = "scisoft-net-map.isri.cmu.edu"
regport = 7778


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


addUserMetadata <- function(metadata) {
    jobinf$userMetadata = merge(jobinf$userMetadata, metadata);
}

deanonymize <- function(name, webpage, pubspage) {
    addUserMetadata(list("ssnm_name"=name, "ssnm_webpage"=webpage, "ssnm_pubspage"=pubspage))
}

#getBootEpoch <- function() {
#    return(system("stat -t /proc/1 | awk '{print $14}'"))
#}

getJobID <- function() {
   if (jobinf$jobID == "") {
       jobinf$jobID = paste(getScimapId(), 
                     as.character(as.integer(Sys.time())), 
                     sep="")
   }
   return(jobinf$jobID)
}

deps.identical <- function(dep1, dep2) {
   return(identical(dep1,dep2));
}

e <- new.env()
.onLoad <- function(lib, pkg, ...) {
    if (interactive() && isEnabledScimap() && jobinf$startup == 0 && stats::runif(1) > .9) {
       packageStartupMessage("Note: Package scimapClient will send anonymized usage stats to ", reghost)
       packageStartupMessage("  You can disable this forever with 'disableScimap()'")
       packageStartupMessage("  Visit ", reghost, " to see what R packages your community uses")
    }
    jobinf$startup <- Sys.time()
    reg.finalizer(e, function (obj) {
        thisreportdeps <- justDependencies()
        if (!deps.identical(thisreportdeps, jobinf$lastreportdeps)) {
            scimapRegister(thisreportdeps, Sys.time(), quiet=FALSE)
        } 
        jobinf$lastreportdeps <- thisreportdeps
        jobinf$lastreporttime <- Sys.time()
    }, onexit=TRUE)
    tcm <- taskCallbackManager()
    reporter <- function(expr, value, ok, visible) {

        # Uncomment this to ignore sessions where the original packages
        #  loaded are the ONLY ones that ever load
        #
        #if (deps.identical(jobinf$lastreportdeps, list())) {
        #    jobinf$lastreportdeps = justDependencies()
        #}

        thisreporttime <- Sys.time()
        if (thisreporttime-jobinf$lastreporttime > 3600) {  ##### Only check if it's been an hour since last prompt
            thisreportdeps <- justDependencies()
            if (!deps.identical(thisreportdeps, jobinf$lastreportdeps)) {   ### Only send if something's changed
                scimapRegister(thisreportdeps, thisreporttime, quiet=TRUE)
            } 
            jobinf$lastreportdeps <- thisreportdeps
            jobinf$lastreporttime <- thisreporttime
        } 
        TRUE
    }
    tcm$add(reporter, name="reporter")
}

idfile <- function() { return (paste(system.file(package="scimapClient"),"../scimap_unique_id",sep="/")) }
disabledFile <- function() { return (paste(system.file(package="scimapClient"),"../scimap_permission_denied",sep="/")) }

getScimapId <- function() {
   return(convolute(Sys.info()[["user"]], getInstallId()))
}

convolute <- function(userid, installid) {
    if (userid == "") { return(installid); }
    z = rep(0, 5)
    for(i in 0:4) {
      z[i+1] = as.integer(substr(installid, i*5+1, i*5+5))
    }
    idparts = as.integer(charToRaw(userid))
    for(i in 1:length(idparts)) {
       z1 = i %% 5 + 1
       z[z1] = z[z1] * (bitwAnd(idparts[i], 15)+2)
    }
    return(paste(z, collapse=""));
}


getInstallId <- function() {
    if (file.exists(idfile())) {
        id <- scan(file=idfile(), what=character(), quiet=TRUE)
    } else {
        id <- paste(sample(c(0:9),25,replace=TRUE),sep="",collapse="")
        write(id, file=idfile())
    }
    return(id)
}

enableScimap <-
function() {
    if (!interactive()) {
       cat("The function enableScimap() only works in interactive mode.")
       return()
    }
    if (isEnabledScimap()) {
       cat("Already enabled.\nCall disableScimap() if you want to disable it.\n")
       return()
    }
    
cat("This package sends anonymous usage tracking information about R packages
to a server that allows authors of packages to track how
widely used and installed they are.  This is helpful
for demonstrating the usefulness of these packages to 
their employers and funding agencies.  You can see that data
online at ", reghost, "

This tracking is voluntary and anonymous. See the help page for
previewPacket() for more information (type: help(previewPacket) )

")
    
    ok <- readline(paste("Is it OK to send anonymous usage reports to ", reghost, "? (y/n) ", sep=""))
    if (substr(ok, 1, 1) == "y" || substr(ok, 1, 1) == "Y") {
       if (!isEnabledScimap()) {
           file.remove(disabledFile())
       }
       cat("***Done! Usage reporting enabled.***")
    } else {
       write("Disabled",file=disabledFile())
       cat("***Disabled.  No usage reports will be sent.***\n")
    }
}

isEnabledScimap <-
function() {
    return(!file.exists(disabledFile()))
}

disableScimap <-
function() {
    write("Disabled",file=disabledFile())
    cat("Usage reporting has been disabled.\nCall enableScimap() to renable it at any time.\n")
}


note <- function(array, key, value) {
    if (is.null(key) || is.null(value)) {
        return(array);
    }
    if (key == "" || value == "") {
        return(array);
    }
    if (key == value) {
        return(array);
    }
    if (!key %in% names(array)) {
        array[[key]] = c()
    }
    array[[key]] = unique(c(array[[key]], value))
    return(array)
}

guessPackage <- function(f) {
   tryCatch({
       if (typeof(f) == "character" && f == "") {
          return("")
       }
       if (typeof(f) == "character") {
          f = get(f)
       }
       e1 <- environment(f)
       if (environmentName(e1) == "") {
          return (environmentName(parent.env(e1)));
       } else {
          return (environmentName(e1));
       }
   },{
       return(NULL);
   })
}

scimapIn <- function(caller=parent) {
    parent = toString(sys.call(which=-1))
   
    if (!is.null(jobinf$last)) {
        jobinf$weakDeps = note(jobinf$weakDeps, toString(caller), toString(jobinf$last));
        jobinf$weakPackageDeps = note(jobinf$weakPackageDeps, guessPackage(caller), guessPackage(jobinf$last))
    }
    if (length(jobinf$stack) > 0) {
        jobinf$dynamicDeps = note(jobinf$dynamicDeps, toString(jobinf$stack[[length(jobinf$stack)]]), toString(caller));
        jobinf$dynamicPackageDeps = note(jobinf$dynamicPackageDeps, guessPackage(jobinf$stack[[length(jobinf$stack)]]), guessPackage(caller));
    }
    jobinf$stack = c(jobinf$stack, caller)
    jobinf$last = NULL
}

scimapOut <- function(caller=parent) {
    parent = toString(sys.call(which=-1))

    length(jobinf$stack) <- length(jobinf$stack)-1
    jobinf$last = caller
}

appendVersion <- function(pkgname) {
    return (paste(pkgname, utils::packageVersion(pkgname), sep="/"));
}

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

previewPacket <- function() {
    thisreportdeps <- justDependencies();
    time <- Sys.time();
    return(scimapPacket(thisreportdeps, time));
}

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
          jobID = getJobID(),
          pkgT = deps$pkgT,
          dynDeps = deps$dynDeps,
          dynPackDeps = deps$dynPackDeps,
          weakDeps = deps$weakDeps,
          weakPackDeps = deps$weakPackDeps,
          userMetadata = deps$userMetadata
    )
    return(RJSONIO::toJSON(jobrec));
}

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
