#' retrieve tbk headers
#'
#' @param tbk_fnames the tbk file names
#' 
#' @return an S3 object of class tbk
#' 
#' @export
tbk_hdrs <- function(tbk_fnames) {

  lapply(tbk_fnames, 
         function(tbk_fname) {
           tbk_file <- file(tbk_fname, "rb")
           on.exit(close(tbk_file))
           hdr <- tbk_hdr(tbk_file)
           hdr
         })

}

