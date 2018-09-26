#' Prepare data
#'
#' Function prepares all the necessary data for further analysis. Requires the dataset loaded with @loadData function. Function assigns the Values
#' into .GlobalEnv.
#' @importFrom magrittr %>%
#' @param data raw dataset loaded with @loadData function
#'
#' @return data - modified dataset
#' @return chng - matrix with percentage changes in stock prices
#' @return cor, cov, expectedReturn, risk - measures of stocks under analysis
#' @return num_stocks - number of stocks in analysis
#' @export
prepareData <- function(data){

  # Count percentage differences of stock prices (based on the closing price)
  data <- lapply(data, FUN = function(x){ x %>%
      dplyr::mutate(Zmiana = (Zamkniecie-lag(Zamkniecie))/lag(Zamkniecie))})

  # number of stocks
  num_stocks <- length(data)

  # Create matrix with stock prices monthly percentage changes
  chng <- sapply(1:num_stocks, FUN = function(x) rbind(data[[x]][,7]))[-1,]
  colnames(chng) <- tickers

  # Correlation, covariance, expected rate of return and risk (measured with standard deviation)
  cor <- cor(chng)
  cov <- cov(chng)
  expectedReturn <- apply(chng, 2, mean)
  risk <- apply(chng, 2,sd)

  # Assign all created variables into Globalenv
  assign("data", data, .GlobalEnv)
  assign("chng", chng, .GlobalEnv)
  assign("cor", cor, .GlobalEnv)
  assign("cov", cov, .GlobalEnv)
  assign("expectedReturn", expectedReturn, .GlobalEnv)
  assign("risk", risk, .GlobalEnv)
  assign("num_stocks", num_stocks, .GlobalEnv)
}
