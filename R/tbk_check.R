#' check that a file is actually a tabixmate file
#' 
#' @param tbk_file  character string, the file
#' 
#' @return          logical, is it or isn't it 
#' 
#' @export
tbk_check <- function(tbk_file) {

  return(rawToChar(readBin(tbk_file, raw(), 3, 1)) == "tbk")

}
  
