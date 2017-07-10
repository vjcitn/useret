#'perform incremental pca on a hdf5 file
#'@import reticulate
#'@param filepath path to hdf5 file
#'@export
incremental_pca <- function(filepath){
  np <- import("numpy")
  pd <- import("pandas")
  sk <- import("sklearn.decomposition")
  h5py <- import("h5py")
  f1 = h5py$File(filepath)
  item = f1$items()
  data = item[[1]][[2]]
  n = data$shape[[1]]
  chunk_size = 100L
  ipca = sk$IncrementalPCA(n_components=10L, batch_size=16L)
  nchunk <- ifelse(n %% chunk_size == 0, n/chunk_size, 1+trunc(n/chunk_size))
  for(i in 1:nchunk){
     i0 <- (i-1)*chunk_size+1
     i1 <- ifelse(i*chunk_size > n , n, i*chunk_size)
     ir <- i0:i1
     submat <- numpyMatrix[ir,]
     ipca$partial_fit(data)
  }
}