#' Count speciman adaptation
#'
#' Create an auxiliary function for counting a single speciman adaptation (single portfolio quality)
#' Function takes *x* which is a single specimen (sequence of bits) and returns a value of its adaptation. The adaptation value is the
#' expected portfolio return / portfolio risk.
#'
#' @param x speciman, which is a sequence of bits
#' @param num_genes number of bits encoding single stock
#' @param expectedReturn. a vector with expected return rates for analysed stocks
#' @param num_stocks. number of stocks in analysis
#'
#' @return Value of adaptation
#' @export
singleAdapt <- function(x, num_genes, expectedReturn.=expectedReturn, num_stocks.=num_stocks){

  y <- paste(unlist(x), collapse="")
  y <- sapply(1:num_stocks., FUN = function(z) substring(y,(z-1)*num_genes+1, z*num_genes))
  portfolioShares <- sapply(y,binToDec)
  portfolioShares <- portfolioShares / sum(portfolioShares)
  expectedPortfolioReturn <- sum(portfolioShares * expectedReturn.)

  portfolioRisk <- 0
  for(i in 1:num_stocks.){
    for(j in 1:num_stocks.){
      portfolioRisk <- portfolioRisk + portfolioShares[i] * portfolioShares[j] * cov[i,j]
    }
  }

  specimenAdaptation <- expectedPortfolioReturn / sqrt(portfolioRisk)
  if(is.nan(specimenAdaptation)){
    return(0)
  } else{
    return(specimenAdaptation)
  }
}
