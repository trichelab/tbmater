#' read a tbk file header
#'
#' @param tbk_file  the filename
#' 
#' @return a 'tbk' S3 object with fields tbk_version, dtype, smax, num, and msg
#' 
#' @export 
tbk_hdr <- function(tbk_file) {

  stopifnot(tbk_check(tbk_file)) 

  tbk_version <- readBin(tbk_file, integer(), 1, 4)
  dtype <- readBin(tbk_file, integer(), 1, 8)
  num <- readBin(tbk_file, integer(), 1, 8)
  msg <- rawToChar(readBin(tbk_file, raw(), HDR_EXTRA, 1))
  out <- list(
    tbk_version = tbk_version,
    dtype = names(dtypes)[dtypes==bitwAnd(dtype, 0xff)],
    smax = bitwShiftR(dtype,8),
    num = num,
    msg = msg
  )
  class(out) <- "tbk"
  return(out) 

}
