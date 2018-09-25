# Load required packages
rm(list=ls())

source("scripts/scraper.R")
source("scripts/GA_implementation.R")


# Load data for 5 biggest polish companies
tickers <- c("kgh","pko","peo","pkn", "ccc", "jsw", "tpe", "lpp", "eur", "cdr")
start=20160101
end=20180925
data <- lapply(tickers, loadData, start=start, end=end)

# Prepare data
prepareData(data)
  
# RUN EXPERIMENT - run with default hiperparameter values
num_iter = 200
num_spec = 400
num_genes = 10 
p_cross = 0.9
p_mutant = 0.01

system.time({
  result <- genalg(num_iter = num_iter,
                   num_spec = num_spec,
                   num_genes = num_genes,
                   p_cross = p_cross,
                   p_mutant = p_mutant)
})



# ANALYSIS
performancePlot()
violinPlot()
riskVsReturnPlot()
optimumPortfolio()
comparePopulPlot()
corrplot(cor,  order='hclust')




