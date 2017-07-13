
#def bc_ipca( mat, ncomp, chsize ):
#   ipca = IncrementalPCA(n_components=ncomp, batch_size=chsize)
#   return ipca.fit_transform(mat)
#
#def bc_chunked_ipca( mat, chunk_size, ncomp, bsize ):

#' use sklearn.incrementalPCA interfaced to package-local python module
#' @param R matrix
#' @param n_comp number of PCs to compute
#' @param batch_size number of records handled in each batch
#' @examples
#' # small example
#' dd = data.matrix(iris[,1:4])
#' Rpcs = prcomp(dd)
#' Ppcs = biocIPCA(dd, 4, 10)
#' cor(Rpcs$x, Ppcs$rotated)
#' # bigger
#' data(bano5k)
#' bpc = prcomp(bano5k)
#' Pbpc = biocIPCA(bano5k, 5, 1000) # need mem profiling
#' cor(bpc$x[,1:5], Pbpc$rotated)
#' Pbpc[[1]]$explained_variance_
#' Pbpc2 = biocIPCA(bano5k, 5, 5000) # may have higher mem cost
#' cor(bpc$x[,1:5], Pbpc2$rotated) # better
#' Pbpc2[[1]]$explained_variance_
#' @export
biocIPCA = function(mat, n_comp, batch_size) {
 n_comp = as.integer(n_comp)
 batch_size = as.integer(batch_size)
 bc_incr = 
   import_from_path("bc_incr", system.file("pysrc", package="useret"))
 ans = bc_incr$bc_ipca(mat, n_comp, batch_size)
 list(ipcaObj = ans[[1]], rotated=ans[[2]], call=match.call())
}

#' @rdname biocIPCA
#' @aliases biocPCA
#' @export
biocPCA = function(mat, n_comp) {
 n_comp = as.integer(n_comp)
 bc_sk = 
   import_from_path("bc_incr", system.file("pysrc", package="useret"))
 ans = bc_sk$bc_pca(mat, n_comp)
 list(pcaObj = ans[[1]], rotated=ans[[2]], call=match.call())
}

#' @rdname biocIPCA
#' @aliases biocPCA_chunked
#' @note biocIPCA_chunked uses partial_fit over all chunks.
#' @export
biocIPCA_chunked = function(mat, chunk_size, n_comp, batch_size) {
  warning("july 13 -- this only works if chunk_size is nrow")
  ai = as.integer
 bc_sk = 
   import_from_path("bc_incr", system.file("pysrc", package="useret"))
  ans = bc_sk$bc_chunked_ipca(mat, ai(chunk_size), ai(n_comp), ai(batch_size))
  list(pcaObj = ans[[1]], rotated=ans[[2]], call=match.call())
}
