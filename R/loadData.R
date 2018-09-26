#' Load data
#'
#' Function loads the required stock monthly data for tick *x* from https://stooq.pl.
#'
#' @param x tick of required stock (ex. "PKO")
#' @param start start date (format = yyyymmdd)
#' @param end end date (format = yyyymmdd)
#'
#' @return data.frame with share prices
#' @export
loadData <- function(x, start, end){

  URL <- paste("https://stooq.pl/q/d/l/?s=", x, "&d1=", start, "&d2=", end, "&i=m", sep="")
  x <- RCurl::getURL(URL)
  out <- read.csv(textConnection(x))
  return(out)
}
