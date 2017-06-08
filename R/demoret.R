#' demonstrate using reticulate inside R
#' @export
myPydemo = function() {
 os <- import("os")
 os$chdir("/private/tmp")
 os$getcwd()
}

#' getting a constant array from python to R
#' @param size integer giving overall python size of array
#' @param dims array dimensions in python idiomatic order
#' @export
demArr = function(size=25L, dims=c(4L, 3L, 2L)) {
 np = import("numpy")
 np$reshape( np$arange(1,size), dims, "F" )
}

#' import matrix from R
#' @param m standard matrix
r2py_shape = function(m) {
 np = import("numpy")
 mymat_inpy = np$array( as.numeric(data.matrix(m)) )
 mymat_inpy = np$reshape( mymat_inpy, c(4L, 150L), "F" )
 np$shape( mymat_inpy )
}
   
