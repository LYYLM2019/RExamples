# purpose: write a function to:
# - attempt package load
# - if not then attempt package install & load
checkInstallPackage = function(packName) {
  tryCatch(library(packName), 
           error = function(errCondOuter) {
             message(paste0("No such package: ", packName, "\n Attempting to install."))
             tryCatch({
               install.packages(packName)
               library(packName, character.only = TRUE)               
             }, 
             error = function(errCondInner) {
               message("Unable to install packages. Exiting!\n")
             },
             warning = function(warnCondInner) {
               message(warnCondInner)
             })
           },
           warning = function(warnCondOuter) {
             message(warnCondOuter)
           },
           finally = {
             paste0("Done processing package: ", packName)
           })
}

# for packages that exist on given repo
invisible(lapply(c("EnsembleBase", 
                   "fastcluster",
                   "glarma",
                   "partools"), checkInstallPackage))


# for packages that do not exist
checkInstallPackage("blabla")
