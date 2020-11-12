#' write tabixmate chunks
#' 
#' @param out_file  where to write it 
#' @param d1        data 1
#' @param d2        data 2 
#' @param dtype     data type (see `dtype`)
#'
#' @export
tbk_write_unit <- function(out_file, d1, d2, dtype) {

    if (dtype == "INT1") {
        d0 <- 0
        tmp <- lapply(seq_along(d1), function(i) {
            d0 <<- bitwOr(bitwShiftL(d0, i%%8), bitwAnd(d1, 0x1))
            if (i%%8 == 0) {
                writeBin(as.raw(d0), out_file, 1)
                d0 <<- 0
            }
        })
    } else if (dtype == "INT2") {
        d0 <- 0
        tmp <- lapply(seq_along(d1), function(i) {
            d0 <<- bitwOr(bitwShiftL(d0, 2*(i%%4)), bitwAnd(d1, 0x3))
            if (i%%4 == 0) {
                writeBin(as.raw(d0), out_file, 1)
                d0 <<- 0
            }
        })
    } else if (dtype == "INT32") {
        writeBin(as.integer(d1), out_file, 4)
    } else if (dtype == "FLOAT") {
        writeBin(d1, out_file, 4)
    } else if (dtype == 'DOUBLE') {
        writeBin(d1, out_file, 8)
    } else if (dtype == "STRINGF") {
        stop("String packing not supported in R yet. Please use the command line.")
    } else if (dtype == "STRINGD") {
        stop("String packing not supported in R yet. Please use the command line.")
    }  else if (dtype == "ONES") {
        writeBin(as.integer(round((d1 + 1.0) * MAX_DOUBLE16)), out_file, 2);
    }  else if (dtype == "FLOAT_FLOAT") {
        stopifnot(!is.null(d2))
        writeBin(do.call('c', lapply(seq_along(d1), function(i) {
            c(writeBin(d1[i], raw(), 4), writeBin(d2[i], raw(), 4))
        })), out_file, 1)
    }  else if (dtype == "FLOAT_INT") {
        stopifnot(!is.null(d2))
        writeBin(do.call('c', lapply(seq_along(d1), function(i) {
            c(writeBin(d1[i], raw(), 4), writeBin(as.integer(d2[i]), raw(), 4))
        })), out_file, 1)
    } else {
        stop("Unrecognized data type.\n")
    }
}
