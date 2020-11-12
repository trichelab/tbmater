#' Pack data to tbk
#'
#' @param data      the data
#' @param data2     2nd data if float.float or float.int
#' @param out_dir   output dir (out_fname is set to out_dir/colnames(data).tbk)
#' @param out_fname output tbk file name
#' @param dtype     data type: INT1,INT2,INT32,FLOAT,DOUBLE,STRINGD, or STRINGF
#' @param idx_fname index file name
#' @param link_idx  whether or not a link is generated to the index
#' 
#' @export
tbk_pack <- function(data, data2 = NULL, out_dir = NULL, out_fname = NULL, dtype="FLOAT", idx_fname = NULL, tbk_version = 1, msg="", na.token = -1.0, link_idx = FALSE) {

    ## output dir
    if (is.null(out_dir)) {
        if (is.null(out_fname)) {
            stop("Please provide out_fname.\n")
        } else {
            out_dir = dirname(out_fname);
        }
    }
    if (!dir.exists(out_dir)) dir.create(out_dir);

    ## infer index
    if (is.null(idx_fname)) {
        if (file.exists(file.path(out_dir, 'idx.gz'))) {
            idx_fname = file.path(out_dir, 'idx.gz')
            message(paste0("Using index: ", idx_fname))
        } else {
            stop("Please provide idx_fname.\n")
        }
    } else if (link_idx && !file.exists(file.path(out_dir, 'idx.gz'))) { # set up idx.gz if nonexistent
        file.symlink(
            tools::file_path_as_absolute(idx_fname),
            file.path(out_dir, 'idx.gz'))
        file.symlink(
            paste0(tools::file_path_as_absolute(idx_fname), '.tbi'),
            file.path(out_dir, 'idx.gz.tbi'))
    }

    ## load index
    idx_addr <- sort(with(read.table(gzfile(idx_fname), header=F,
        stringsAsFactors=FALSE), setNames(V4,V1)))
    
    ## if data is a single numeric vector
    if (is.null(dim(data))) {
        data <- as.matrix(data)
        colnames(data) <- out_fname
        if (is.null(out_fname)) stop("Please provide out_fname.\n");
    }
    if (!is.null(data2) && is.null(dim(data2))) {
        data2 <- as.matrix(data2)
        colnames(data2) <- out_fname
        if (is.null(out_fname)) stop("Please provide out_fname.\n");
    }

    tmp <- lapply(seq_len(ncol(data)), function(k) {
        fname <- colnames(data)[k]
        idx <- match(names(idx_addr), rownames(data))
        d1 <- ifelse(is.na(idx), na.token, data[idx,k])
        d1[is.na(d1)] <- na.token
        if (is.null(data2)) {
            d2 <- NULL
        } else {
            d2 <- ifelse(is.na(idx), na.token, data2[idx,k])
            d2[is.na(d2)] <- na.token
        }

        out_fname1 <- file.path(out_dir, fname)
        if (!grepl('\\.tbk$', out_fname1)) out_fname1 <- paste0(out_fname1, '.tbk')
        out_file <- file(out_fname1, 'wb')
        on.exit(close(out_file))

        ## store idx_fname if msg is not given
        if (length(msg) == 0) {
            msg = idx_fname
        }
        if (nchar(msg) == 0) msg <- idx_fname
        tbk_hdr_write(out_file, d1, dtype = dtype, msg = msg, tbk_version = tbk_version)
        tbk_write_unit(out_file, d1, d2, dtype)
    })
}

