#' constants for tbmate:
#' 
#' MAX_DOUBLE16   (32766)
#' HDR_ID         (3)
#' HDR_VERSION    (4)
#' HDR_DATA_TYPE  (8)
#' HDR_MAX_OFFSET (8)
#' HDR_EXTRA      (8169)
#' HDR_TOTALBYTES (8192) 
#' 
#' @name tbk_constants
#' @aliases tbmate_constants
NULL 

MAX_DOUBLE16   <- bitwShiftL(1,15)-2 # 32766
HDR_ID         <- 3
HDR_VERSION    <- 4
HDR_DATA_TYPE  <- 8
HDR_MAX_OFFSET <- 8
HDR_EXTRA      <- 8169
HDR_TOTALBYTES <- 8192

# might eventually replace with data(tbk_dtype)
dtypes <- c('INT1'=1, 'INT2'=2, 'INT32'=3, 
            'FLOAT'=4, 'DOUBLE'=5, 
            'STRINGD'=6, 'STRINGF'=7,
            'ONES'=30, 
            'FLOAT_INT'=31,
            'FLOAT_FLOAT'=32)

dtype_size <- c('INT1'=1, 'INT2'=1, 'INT32'=4, 
                'FLOAT'=4, 'DOUBLE'=8,
                'STRINGD'=8, 'STRINGF'=8, 
                'ONES'=4, 
                'FLOAT_INT'=8,
                'FLOAT_FLOAT'=8)

dtype_type <- list('INT1'='integer', 'INT2'='integer', 'INT32'='integer', 
                   'FLOAT'='numeric', 'DOUBLE'='numeric', 
                   'STRINGD'='character', 'STRINGF'='character',
                   'ONES'='numeric', 
                   'FLOAT_INT'=c('numeric','integer'), 
                   'FLOAT_FLOAT'=c('numeric','numeric'))

# check 
stopifnot(identical(names(dtypes), names(dtype_size)))
stopifnot(identical(names(dtype_size), names(dtype_type)))

# stored version
data(tbk_dtype) 

# migration: can rely totally upon
# data(tbk_dtype, package="tbmater") 
if (FALSE) { # how to generate this
  tbk_dtype <- data.frame(id=dtypes,
                          name=names(dtypes),
                          size=dtype_size,
                          type=sapply(dtype_type, paste, collapse=","))
}
