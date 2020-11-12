#' like it says: write a tabixmate header
#' 
#' @param out         filename
#' @param data        the data
#' @param dtype       data type ("FLOAT") 
#' @param msg         the message ("") 
#' @param tbk_version tbmate file format version (1) 
#' 
#' @return the result of writeBin
#' 
#' @export 
tbk_hdr_write <- function(out, data, dtype="FLOAT", msg="", tbk_version = 1) {

  writeBin(charToRaw('tbk'), out, 1)
  writeBin(as.integer(tbk_version), out, 4)
  dt <- as.integer(dtypes[dtype])
  if (dtype == "STRINGF") {
    dt <- bitwOr(dt, bitwShiftL(max(sapply(data, nchar)), 8))
  }
  writeBin(dt, out, 8);
  writeBin(as.integer(length(data)), out, 8)
  msgRaw <- charToRaw(msg)
  if (length(msgRaw) > HDR_EXTRA) {
      msgRaw <- msgRaw[1:HDR_EXTRA]
      warning(sprintf("msg longer than %d, truncated.", HDR_EXTRA))
  }
  if (length(msgRaw) < HDR_EXTRA) {
      msgRaw <- c(msgRaw, rep(as.raw('0'), HDR_EXTRA - length(msgRaw)))
  }
  writeBin(msgRaw, out, 1)

}
