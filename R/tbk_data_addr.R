#' tabixmate data address
#' 
#' @param tbk_fnames  filenames
#' @param idx_addr    index addresses
#' @param all_units   all units? (FALSE) 
#' 
#' @export
tbk_data_addr <- function(tbk_fnames, idx_addr, all_units = FALSE) {
  idx_addr <- sort(idx_addr)
  data <- lapply(tbk_fnames, function(tbk_fname) {
    in_file <- file(tbk_fname,'rb')
    on.exit(close(in_file))
    hdr <- tbk_hdr(in_file)
    tbk_read_unit(in_file, idx_addr, hdr, all_units = FALSE)
  })
}
