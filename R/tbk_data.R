#' Get data from tbk
#'
#' Assumptions:
#' 1) All the tbks have the same index.
#' 2) If idx_fname not given, idx.gz is in the same folder as the first sample.
#' 
#' @param fnames        tbk_fnames
#' @param idx_fname     index file. If not given, use the idx.gz in first path 
#' @param probes        probe names
#' @param simplify      reduce matrix to vector if only one sample is queried
#' @param all_units     retrieve all units for float.float and float.int
#' @param max_pval      maximum sig2 for float.float
#' @param min_coverage  minimum sig2 for float.int
#' @param name.use.base use basename for sample name
#' @param max_addr      random addressing if under max_addr
#' @param min_source    random addressing if source size is under min_source
#'
#' @return              a numeric matrix
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges       IRanges
#' 
#' @import Rsamtools
#' @import tools
#' 
#' @export
tbk_data <- function(tbk_fnames, idx_fname = NULL, probes = NULL, show.unaddressed = FALSE, chrm = NULL, beg = NULL, end = NULL, as.matrix = FALSE, simplify = FALSE, name.use.base=TRUE, max_addr = 3000, max_source = 10^6, max_pval = 0.05, min_coverage = 5, all_units = FALSE) {

  ## given a folder
  if (length(tbk_fnames) == 1 && dir.exists(tbk_fnames[1])) {
    tbk_fnames <- list.files(tbk_fnames, '.tbk$', full.names=TRUE)
  }

  ## infer idx
  if (is.null(idx_fname)) idx_fname <- infer_idx(tbk_fnames[1])

  ## subset probes
  if (is.null(chrm)) { # subset with probes

    idx_addr <- with(read.table(gzfile(idx_fname), 
                                header=F, stringsAsFactors=FALSE), 
                     setNames(V4, V1))
    if (length(probes) > 0) idx_addr <- idx_addr[probes]

  } else { # subset with genomic coordinates

    if (is.null(beg)) beg <- 1
    if (is.null(end)) end <- 2^28
    input_range <- GRanges(chrm, IRanges(beg, end))
    df <- as.data.frame(t(simplify2array(strsplit(scanTabix(
        idx_fname, param=input_range)[[1]], '\t'))), stringsAsFactors = FALSE)
    colnames(df)[1:4] <- c('seqnames','beg','end','offset')
    df$offset <- as.integer(df$offset)
    if (!show.unaddressed) df <- df[df$offset >= 0,]
    if (ncol(df) >= 5 && sum(df$beg==1) > 10) { # probe names from 5th column
      idx_addr <- setNames(df$offset, df[,5])
    } else if (length(unique(df$seqnames[1:min(nrow(df),100)])) < 30) { #chr:beg
      idx_addr <- setNames(df$offset, paste0(df$seqnames,":",df$beg))
    } else { # use probe names from rownames
      idx_addr <- setNames(df$offset, rownames(df))
    }
  }

  data <- tbk_data0(tbk_fnames, idx_addr, max_addr=max_addr, 
                    max_source=max_source, all_units=all_units)

  ## add column names
  if (name.use.base) {
    names(data) <- tools::file_path_sans_ext(basename(tbk_fnames))
  } else {
    names(data) <- tbk_fnames
  }

  ## add row names
  if (!all_units) {
    data <- do.call(cbind, data)
  }

  return(data)
}
