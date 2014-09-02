
reghost = "sci-net-map.isri.cmu.edu"
regport = 7778

idfile <- function() { return (paste(system.file(package="scimapClient"),"install_unique_id",sep="/")) }
okfile <- function() { return (paste(system.file(package="scimapClient"),"permission_granted",sep="/")) }

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
    if (!interactive()) {
       cat("The function enableScimap() only works in interactive mode.")
       return()
    }
    if (isEnabledScimap()) {
       cat("Already enabled.\nCall disableScimap() if you want to disable it.\n")
       return()
    }
    if (substr(ok, 1, 1) == "y" || substr(ok, 1, 1) == "Y") {
       write("OK",file=okfile())
       cat("***Done! Usage reporting enabled.***
       
     Please insert a call to scimapRegister() in your scripts such that they 
     will be called ONCE per run, after R loads all the packages the script 
     will use.\n")
    } else {
       if (isEnabledScimap()) {
           file.remove(okfile())
       }
       cat("***Not enabled.  No usage reports will be sent.***\n")
    }
}

isEnabledScimap <-
function() {
    return(file.exists(okfile()))
}

disableScimap <-
function() {
    file.remove(okfile())
    cat("Usage reporting has been disabled.\nCall enableScimap() to renable it at any time.\n")
}

scimapRegister <-
function(programName=topCall ) {
    if (isEnabledScimap()) {
        topCall <- toString(sys.call(which=1)); 
        uniqueId = getScimapId()
        username <- uniqueId  #Sys.info()["user"]
        commandline <- commandArgs()
        t <- Sys.time()
        unixtime <- as.character(as.integer(t))
        fmtTime <- format(t, "%a %b %e %H:%M:%S %Y")
        deps = c(lapply( (.packages()), function (p) {
              return (paste(p, packageVersion(p), sep="/"))
          } ), paste("R/", R.version[["major"]], ".", R.version[["minor"]], sep=""))   
        jobrec <- list(
          user = username,
          startEpoch = unixtime,
          startTime = fmtTime,
          platform = list(
              system = Sys.info()[["sysname"]],
              version = Sys.info()[["release"]],
              hardware = Sys.info()[["machine"]]),
          account = uniqueId,
          exec = programName, 
          jobID = paste(username,unixtime, sep=""),
          pkgT = deps
        )
        result = tryCatch({
          a <- make.socket(reghost, regport)
          on.exit(close.socket(a))
          write.socket(a, RJSONIO::toJSON(jobrec))
        }, error = function(e) {
          cat("scimapRegister connection failure: not a problem.\n")
        })
    }
}
