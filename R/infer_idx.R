#' attempt to guess indices
#' 
#' @param tbk_fname   the tabixmate file
#' 
#' @export
infer_idx <- function(tbk_fname) {

  if (file.exists(file.path(dirname(tbk_fname), 'idx.gz')) &&
      file.exists(file.path(dirname(tbk_fname), 'idx.gz.tbi'))) {
    idx_fname <- file.path(dirname(tbk_fname), 'idx.gz')
  } else {
    hdr <- tbk_hdrs(tbk_fname)[[1]]
    if (!is.null(hdr$msg) && file.exists(hdr$msg)) idx_fname <- hdr$msg
    stop("Cannot locate index file. Provide through idx_fname.\n")
  }

  return(idx_fname)

}

