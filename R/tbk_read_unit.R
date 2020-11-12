#' read tabixmate units
#' 
#' @param in_file   filename
#' @param idx_addr  index addresses
#' @param hdr       tabixmate header
#' @param all_units all units? (FALSE) 
#' 
#' @export
tbk_read_unit <- function(in_file, idx_addr, hdr, all_units = FALSE) {
  
  read_unit1 <- list(
      'INT1' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + n
              readBin(in_file, "integer", 1, 1) # FIXME
          })
      },
      'INT2' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              readBin(in_file, "integer", 1, 1) # FIXME
          })
      },
      'FLOAT' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              readBin(in_file, "numeric", 1, 4)
          }, numeric(1))
      },
      'DOUBLE' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              readBin(in_file, "numeric", 1, 4)
          })
      },
      'INT32' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              readBin(in_file, "integer", 1, 4)
          })
      },
      'ONES' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              (as.numeric(readBin(in_file, 'integer', 1, 2, signed=FALSE)) - MAX_DOUBLE16) / MAX_DOUBLE16
          })
      },
      'FLOAT_INT' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              curr_offset <<- curr_offset + 1
              d1 = readBin(in_file, "numeric", 1, 4)
              d2 = readBin(in_file, "integer", 1, 4)
              if (all_units) {
                  c(d1, d2)
              } else {
                  d1
              }
          })
      },
      'FLOAT_FLOAT' = function(idx_addr) {
          vapply(idx_addr, function(offset) {
              if (offset < 0) return(NA)
              if (offset != curr_offset) {
                  seek(in_file, where=offset*dtype_size[hdr$dtype]+HDR_TOTALBYTES, origin='start')
                  curr_offset <- offset
              }
              # yuck
              curr_offset <<- curr_offset + 1
              d1 = readBin(in_file, "numeric", 1, 4)
              d2 = readBin(in_file, "numeric", 1, 4)
              if (all_units) {
                  c(d1, d2)
              } else {
                  d1
              }
          })
      }
  )

  curr_offset <- 0
  read_unit1[[hdr$dtype]](idx_addr)        
}

