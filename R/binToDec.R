#' Convert binary to decimal
#'
#' Create an auxiliary function for decoding a number in binary system into a number in decimal system
#'
#' @param x binary sequence
#'
#' @return value decoded into decimal system
#' @export
binToDec <- function(x){
  return(sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1)))
}
