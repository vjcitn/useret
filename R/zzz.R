#' onLoad
#' @name onLoad
#' @rdname infrastructure
#' @export np
#' @export pd
#' @export h5py
#' @export sk
.onLoad = function(libname, pkgname) {
  packageStartupMessage("Setting up python infrastructure, including numpy with convert=FALSE ...")
  np <<- import("numpy", delay_load=TRUE, convert=FALSE)
  pd <<- import("pandas", delay_load=TRUE)
  h5py <<- import("h5py", delay_load=TRUE)
  sk <<- import("sklearn.decomposition", delay_load=TRUE)
  packageStartupMessage("Done.")
}
