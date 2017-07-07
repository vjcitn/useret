#'combine hdf5 datasets from multiple files into a new hdf5 file
#'@import reticulate
#'@param tenxfiles collection of tenxfiles 
#'@export
combine_files <- function(tenxfiles){
  h5py = import("h5py")
  #create a new, empty hdf5 file
  output_file = h5py$File("/Users/reshg/Desktop/combined_file.h5")
  total_cols = 0 
   
  for(i in 1:length(tenxfiles)){
    #print(tenxfiles[i])
    #grab the data from the file
    item = myfile[i]$items()
    data = item[[1]][[2]]
    total_cols = total_cols + data$shape[[1]]
    total_rows = data$shape[[2]]
  
    if(i == 0){
      #points to the first file in the directory
      maxshape = c(1306127, 27998)
      cd = output_file$create_dataset("newassay001", c(total_cols,total_rows), data = data, maxshape=maxshape, dtype="i4", compression="gzip")
      where_to_start_appending = total_cols
    }
    else{
      axis = 0
      cd$resize(total_rows, axis=axis)
      #cd[where_to_start_appending:total_cols, :] = data
      where_to_start_appending = total_cols
    }
    output_file$close()
  }
}
