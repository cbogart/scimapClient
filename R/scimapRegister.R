
#reghost = "sci-net-map.isri.cmu.edu"
reghost = "0.0.0.0"
regport = 7778

jobinf <- new.env()
jobinf$jobID = ""
jobinf$stack = list()
jobinf$staticDeps = list()
jobinf$dynamicDeps = list()
jobinf$weakDeps = list()
jobinf$dynamicPackageDeps = list()
jobinf$weakPackageDeps = list()
jobinf$userRunMetadata = list()
jobinf$last = ""

getJobID <- function() {
   if (jobinf$jobID == "") {
       jobinf$jobID = paste(getScimapId(), 
                     as.character(as.integer(Sys.time())), 
                     sep="")
   }
   return(jobinf$jobID)
}

sendRegistrationInfo <- function() {
   topCall <- toString(sys.call(which=1)); 
   scimapRegister(topCall)
}

e <- new.env()
.onLoad <- function(lib, pkg, ...) {
    reg.finalizer(e, function (obj) {
        sendRegistrationInfo()
    }, onexit=TRUE)
}

idfile <- function() { return (paste(system.file(package="scimapClient"),"../scimap_unique_id",sep="/")) }
disabledFile <- function() { return (paste(system.file(package="scimapClient"),"../scimap_permission_denied",sep="/")) }

getScimapId <- function() {
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
online at http://sci-net-map.isri.cmu.edu

This tracking is voluntary and anonymous. See the help page for
scimapRegister() for more information (type: help(scimapRegister) )

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
   if (typeof(f) == "character") {
      return("");
   }
   e1 <- environment(f)
   if (environmentName(e1) == "") {
      return (environmentName(parent.env(e1)));
   } else {
      return (environmentName(e1));
   }
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

getPackageDependencies <- function() {
        ip <- installed.packages()
        deps = list()
        for (p in (.packages())) { 
            deps[[paste(p, packageVersion(p), sep="/")]] = c(tools::package_dependencies(p, db=ip)[[p]])
        }
        deps[[paste("R/", R.version[["major"]], ".", R.version[["minor"]], sep="")]] = c("utils")   
        return(deps)
}

scimapPacket <-
function(programName) {
    t <- Sys.time()
    unixtime <- as.character(as.integer(t))
    fmtTime <- format(t, "%a %b %e %H:%M:%S %Y")
    jobrec <- list(
          scimapInfoVersion = 3,
          user = getScimapId(),
          startEpoch = unixtime,
          startTime = fmtTime,
          platform = list(
              system = Sys.info()[["sysname"]],
              version = Sys.info()[["release"]],
              hardware = Sys.info()[["machine"]]),
          exec = programName, 
          jobID = getJobID(),
          pkgT = getPackageDependencies(),
          dynDeps = RJSONIO::toJSON(jobinf$dynamicDeps),
          dynPackDeps = RJSONIO::toJSON(jobinf$dynamicPackageDeps),
          weakDeps = RJSONIO::toJSON(jobinf$weakDeps),
          weakPackDeps = RJSONIO::toJSON(jobinf$weakPackageDeps)
    )
    return(RJSONIO::toJSON(jobrec));
}

scimapRegister <- function(programName = topCall) {
    topCall <- toString(sys.call(which=1)); 
    if (isEnabledScimap()) {
        result = tryCatch({
          a <- make.socket(reghost, regport)
          on.exit(close.socket(a))
          write.socket(a, scimapPacket(programName))
        }, error = function(e) {
          cat("Could not upload usage data to ", reghost, ".\n")
        })
    }
}
