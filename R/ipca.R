#' interface to incremental PCA from sklearn: mono implies monolithic data handling
#' @import reticulate
#' @param x either character string to HDF5 matrix or R matrix
#' @param n_comp integer number of PC to compute
#' @param batch_size 
#' @param \dots not used
#' @examples
#' d = data.matrix(iris[,1:4])
#' p = prcomp(d)
#' dim(p$x)
#' h5targ = tempfile()
#' require(rhdf5)
#' h5createFile(h5targ)
#' h5write(d, h5targ, "iris")
#' tst = ipca_mono(h5targ, n_comp=4L)
#' tstm = tst@fit_transform
#' colnames(tstm) = paste0("ip", 1:4)
#' pairs(cbind(p$x, tstm))
#' @export
ipca_mono = function(x, n_comp=2L, batch_size=25L, ...) {
  n_comp = as.integer(n_comp) # be sure
  batch_size = as.integer(batch_size) # be sure
  message("initializing python infrastructure...")
  np <- import("numpy", convert=FALSE)
  pd <- import("pandas")
  sk <- import("sklearn.decomposition")
  h5py <- import("h5py")
  message("done.")
  tr = np$matrix
  tr = tr$transpose # cannot compose?
  if (is(x, "character")) {
     x_ref = try(h5py$File(x))
     if (inherits(x_ref, "try_error")) stop(paste0("h5py$File fails on '", deparse(substitute(x)), "'"))
     x_ref = x_ref$items()[[1]][[2]]  # API for h5py$File
     x_ref = tr( np$matrix(x_ref) )
     }
  else if (is(x, "matrix")) x_ref = np$matrix(x) # no transpose if not HDF5
  ans = sk$IncrementalPCA(n_components=n_comp, batch_size=batch_size)
  new("ipca", ref=ans, fit_transform=ans$fit_transform( x_ref ), dataref=x_ref)
}
setOldClass("sklearn.decomposition.incremental_pca.IncrementalPCA")
setClass("ipca", representation(ref="sklearn.decomposition.incremental_pca.IncrementalPCA",
                     fit_transform="matrix", dataref="ANY" ))
setMethod("show", "ipca", function(object) 
  cat("instance of ipca with precomputed fit_transform and data reference\n"))

# to modularize the approach for remote data in HDF5, define a class
# that manages shape and reference info

setOldClass("h5py._hl.files.File")
setClass("H5pymat", representation(filename="character", handle="h5py._hl.files.File",
 shape = "ANY", dataref = "ANY")) # the ANY can be made more specific

#' create H5py matrix container
#' @param fn file name for HDF5 matrix
#' @export
H5pymat = function(fn) {
  h5py <- import("h5py")
  f1 = h5py$File(fn)
  item = f1$items()
  dataref = item[[1]][[2]] #
  shape = unlist(dataref$shape)
  new("H5pymat", filename=fn, handle=f1, dataref=dataref, shape=shape)
}

setMethod("show", "H5pymat", function(object) {
 cat("H5pymat instance.\n")
 cat("file: ", object@filename, "\n")
 cat("shape:\n")
 print(object@shape)
})

setGeneric("getRowChunk", function(x, start, end) standardGeneric("getRowChunk"))
setMethod("getRowChunk", c("H5pymat", "integer", "integer"), 
   function(x, start, end) {
   stopifnot(start>0, end<=x@shape[2])
   np <- import("numpy", convert=FALSE)
   np$take( x@dataref, np$arange(start-1L, end), 1L )
   })  

setGeneric("ipca", function(src, ncomp, batch_size) standardGeneric("ipca"))
#' use chunks from remote HDF5 source to compute incremental PCA
#' @param src H5pymat instance
#' @param ncomp number of PCs to compute
#' @param batch_size number of records per batch
#' @aliases ipca
#' @examples
#' d = data.matrix(iris[,1:4])
#' p = prcomp(d)
#' dim(p$x)
#' h5targ = tempfile()
#' require(rhdf5)
#' h5createFile(h5targ)
#' h5write(d, h5targ, "iris")
#' hmat = H5pymat(h5targ)
#' tst = ipca(hmat, ncomp=4L, batch_size=50L)
#' dim(tst)
#' @exportMethod ipca
setMethod("ipca", c("H5pymat", "integer", "integer"), function(src, ncomp, batch_size) {
  nr = src@shape[2]
  np <- import("numpy", convert=FALSE)
  pd <- import("pandas")
  h5py <- import("h5py")
  sk <- import("sklearn.decomposition")
  ipca = sk$IncrementalPCA(n_components=ncomp, batch_size=batch_size)
  starts = as.integer(seq(1, nr, batch_size))
  ends = as.integer(c(starts[-1]-1, nr))
# build incremental PCA components
  for (i in 1:length(starts)) 
    ipca$partial_fit( getRowChunk(src, starts[i], ends[i])$T )
# construct the reexpressed data by chunk
  res = lapply(1:length(starts), function(x) ipca$transform( getRowChunk(src, starts[x], ends[x])$T ))
  do.call(rbind, res) # should also return the python structure
})


