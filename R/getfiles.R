#'get hdf5 files from working directory
#'@param filepath path to the directory having the hdf5 files
getfiles <- function(filepath){
  setwd(filepath)
  myfiles = dir()
  myfiles
}