#' Show portfolio structure
#'
#' Function decodes sequence of bits in *x* and returns the portfolio structure.
#'
#' @param x single portfolio (sequence of bits)
#' @param num_genes number of bits encoding single stock
#' @param num_stocks. number of stocks in analysis
#'
#' @return Vector with portfolio structure.
#' @export
showPortfolio <- function(x, num_genes, num_stocks.=num_stocks){

  y <- paste(unlist(x), collapse="")
  y <- sapply(1:num_stocks., FUN = function(z) substring(y,(z-1)*num_genes+1, z*num_genes))
  portfolioShares <- sapply(y,binToDec)
  portfolioShares <- portfolioShares / sum(portfolioShares)
  names(portfolioShares) <- names(expectedReturn)
  return(portfolioShares)
}



#' Show portfolio return
#'
#' Function decodes sequence of bits in *x* and returns expected return for a given portfolio.
#'
#' @param x single portfolio (sequence of bits)
#' @param num_genes number of bits encoding single stock
#' @param num_stocks. number of stocks in analysis
#'
#' @return Value for expected return
#' @export
portfolioReturn <- function(x, num_genes, expectedReturn.=expectedReturn, num_stocks.=num_stocks){

  e <- sum(showPortfolio(unlist(x), num_genes, num_stocks.) * expectedReturn.)
  return(e)
}



#' Show portfolio risk
#'
#' Function decodes sequence of bits in *x* and returns value of risk for a given portfolio.
#'
#' @param x single portfolio (sequence of bits)
#' @param num_genes number of bits encoding single stock
#' @param num_stocks. number of stocks in analysis
#'
#' @return Value of risk
#' @export
portfolioRisk <- function(x, num_genes, num_stocks.=num_stocks){

  share <- showPortfolio(unlist(x), num_genes)
  portfolioRisk <- 0
  for(i in 1:num_stocks.){
    for(j in 1:num_stocks.){
      portfolioRisk <- portfolioRisk + share[i] * share[j] * cov[i,j]
    }
  }
  return(sqrt(portfolioRisk))
}
