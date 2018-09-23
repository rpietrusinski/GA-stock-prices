

loadData <- function(x, start, end){
  # Function loads the required stock monthly data for tick @x from https://stooq.pl domain.
  # 
  # 
  # Parameters:
  # -----------
  # x - tick of required stock (ex. "PKO")  
  # start - start date (format = yyyymmdd)
  # end - end date (format = yyyymmdd)
  # 
  #   
  # Returns:
  # --------
  # data.frame with the following columns:
  #   Data - date
  #   Otwarcie - opening price
  #   Najwyzszy - highest price
  #   Najnizszy - lowest price
  #   Zamkniecie - closing price
  #   Wolumen - volume
  
    
  
  require(RCurl)
  URL <- paste("https://stooq.pl/q/d/l/?s=", x, "&d1=", start, "&d2=", end, "&i=m", sep="")
  x <- getURL(URL)
  out <- read.csv(textConnection(x))
  return(out)
}







# EXAMPLE

# 1. Create a vector of ticks of all WIG20 companies
# tickers <- c('ALR','ACP','BZW','CCC','CPS',
#              'ENG','EUR','JSW','KGH','LTS',
#              'LPP','MBK','OPL','PEO','PGE',
#              'PGN','PKN','PKO','PZU','TPE')

# 2. Create a list of data.frames with data for each stock
# data <- lapply(tickers, loadData, start=20150701, end=20170801)
