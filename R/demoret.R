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
