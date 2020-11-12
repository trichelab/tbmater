#' bulk read from tabixmate files
#' 
#' @param tbk_fnames  tabixmate filenames
#' @param idx_addr    tabixmate index addresses
#' @param all_units   all units? (FALSE) 
#' 
#' @export 
tbk_data_bulk <- function(tbk_fnames, idx_addr, all_units = FALSE) {

  idx_addr <- sort(idx_addr)
  data <- lapply(tbk_fnames, function(tbk_fname) {
    in_file <- file(tbk_fname,'rb')
    on.exit(close(in_file))
    hdr <- tbk_hdr(in_file)
    idx_addr <- ifelse(idx_addr < 0, NA, idx_addr)
    tbk_read_chunk(in_file, hdr, idx_addr, all_units = all_units)
  })
  data

}
