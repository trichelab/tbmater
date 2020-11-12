#' initial tabixmate file read
#' 
#' @param tbk_fnames  filenames
#' @param idx_addr    index addresses
#' @param all_units   all units? (FALSE) 
#' @param max_addr    maximum address (3000)
#' @param max_source  maximimum source size (1000000)
#' 
#' @export 
tbk_data0 <- function(tbk_fnames, idx_addr, all_units=FALSE, max_addr=3000, max_source=10^6) {

  ## read whole data set only if there are too many addresses 
  ## but too small source data
  if (tbk_hdrs(tbk_fnames[1])[[1]]$num < max_source && 
      length(idx_addr) < max_addr) {
    tbk_data_addr(tbk_fnames, idx_addr, all_units = all_units)
  } else {
    tbk_data_bulk(tbk_fnames, idx_addr, all_units = all_units)
  }

}
