#' read a chunk of a tabixmate file
#' 
#' @param in_file   the file
#' @param hdr       the header
#' @param idx_addr  the index address
#' @param all_units read all units? (FALSE) 
#' 
#' @export 
tbk_read_chunk <- function(in_file, hdr, idx_addr, all_units = FALSE) {

  read_units <- .read_units(in_file, hdr, idx_addr, all_units=all_units)
  data <- read_units[[hdr$dtype]]()
  data <- data[idx_addr+1,,drop=FALSE]
  rownames(data) <- names(idx_addr)
  return(data)

}


# helper fn
.read_units <- function(in_file, hdr, idx_addr, all_units = FALSE) {

  read_unit_default <- function() {
    cbind(sig=readBin(in_file, dtype_type[[hdr$dtype]], 
                      hdr$num, dtype_size[hdr$dtype]))
  }

  res <- list('INT1' = read_unit_default, # FIXME
              'INT2' = read_unit_default, # FIXME
              'ONES' = read_unit_default, # FIXME
              
              'FLOAT_FLOAT' = function() {
                d0 <- readBin(in_file, "raw", hdr$num*8, 1)
                d1 <- readBin(d0[bitwAnd(bitwShiftR(seq_along(d0)+3,2),1) == 1],
                              'numeric', hdr$num, 4)
                d2 <- readBin(d0[bitwAnd(bitwShiftR(seq_along(d0)+3,2),1) == 0],
                              'numeric', hdr$num, 4)
                if (all_units) cbind(sig=d1, sig2=d2)
                else cbind(sig=d1)
              },
          
              'FLOAT_INT' = function() {
                d0 <- readBin(in_file, "raw", hdr$num*8, 1)
                d1 <- readBin(d0[bitwAnd(bitwShiftR(seq_along(d0)+3,2),1) == 1],
                              'numeric', hdr$num, 4)
                d2 <- readBin(d0[bitwAnd(bitwShiftR(seq_along(d0)+3,2),1) == 0],
                              'integer', hdr$num, 4)
                if (all_units) cbind(sig=d1, sig2=d2)
                else cbind(sig=d1)
              },

              'INT32' = read_unit_default, 
              'FLOAT' = read_unit_default, 
              'DOUBLE' = read_unit_default)

  return(res) 

}
