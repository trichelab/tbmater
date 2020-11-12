#' Read a tabix-indexed bed file into a list
#' 
#' @param paths         paths to the bed files
#' @param chrm          chromosome name
#' @param beg           start coordinate of CpG
#' @param end           end coordinate of CpG 2^29 is the max taken
#' @param sample_names  sample names, just use paths if not specified
#' @param BPPARAM       how to parallelize
#'
#' @return a list object with DNA methylation level and depth
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges       IRanges
#' 
#' @import BiocParallel
#' @import tibble
#' @import dplyr
#'
#' @export
tabixRetrieve <- function(paths, chrm, beg = 1, end = 2^28,
                          min_depth = 0, sample_names = NULL,
                          BPPARAM = SerialParam()) {

  input_range <- GRanges(chrm, IRanges(beg, end))
  df_list <- bplapply(paths, 
                      function(path) {
                        df <- as_tibble(t(simplify2array(strsplit(scanTabix(path, param=input_range)[[1]], '\t'))), stringsAsFactors = FALSE)
                        colnames(df) <- c('chrm','beg','end','beta','depth')
                        df$beg <- as.numeric(df$beg)
                        df$beg <- df$beg + 1
                        df$end <- as.numeric(df$end)
                        ## in case the tabix is mal-formed
                        df$beta[df$beta == '.' | df$beta == 'NA'] <- NA
                        df$beta <- as.numeric(df$beta)
                        df$depth <- as.integer(df$depth)
                        df$depth[is.na(df$beta)] <- 0
                        df
                      })
  
  ## make sure the coordinates are the same
  same_coordinates <- sapply(seq_len(length(df_list)-1),
      function(i) identical(df_list[[i]][,1:3], df_list[[i+1]][,1:3]))
  stopifnot(all(same_coordinates))

  ## set sample names
  if (is.null(sample_names)) {
      sample_names <- paths
  }
  stopifnot(length(sample_names) == length(paths))
  names(df_list) <- sample_names

  df_list
}
