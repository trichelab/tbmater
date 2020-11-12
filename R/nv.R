#' convenience function: make a 'named vector'
#' 
#' This trivial function is mostly for convenience in iterating over vectors.
#' As a side effect, it exposes any duplicates in supposedly unique indices. 
#' 
#' @param   x   a character vector, or something that can become one 
#' @return      the same vector, but with its own values as names 
#' 
#' @examples
#'   vec <- letters[1:5]
#'   vec
#'   nv(vec) 
#' 
#' @export 
nv <- function(x) { 

  names(x) <- as.character(x)
  return(x) 

}
