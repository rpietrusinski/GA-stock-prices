## Stock prices prediction with Genetic Algorithm
Genetic algorithm implementation for investment portfolio optimization. Optimization criterion: sharpe = expected return / risk. The effect of risk free rate has been neglected.


### EXAMPLE
##### Clean workspace and load 'genalg.
```r
rm(list=ls())
library(genalg)
```

##### Load data for given tickers, starting/ending point and data frequency ('d' -> daily).
```r
tickers <- c("kgh","pko","peo","pkn", "ccc", "jsw", "tpe", "lpp", "eur", "cdr")
start <- 20180626
end <- 20180926
freq <- 'd'
data <- lapply(tickers, loadData, start=start, end=end, freq=freq)
```

##### Run prepareData function - it prepares data and global variables setup.
```r
prepareData(data)
```

##### Run experiment.
Hyperparameters:  
- num_iter -> Number of iterations
- num_spec -> Number of specimen
- num_genes -> Number of bits encoding a single stock
- p_cross -> Cross-over probability
- p_mutant -> Mutation probability
```r
num_iter = 200
num_spec = 400
num_genes = 10 
p_cross = 0.9
p_mutant = 0.01


result <- genalg(num_iter = num_iter,
                 num_spec = num_spec,
                 num_genes = num_genes,
                 p_cross = p_cross,
                 p_mutant = p_mutant)
```


##### Plots
```r
performancePlot()
violinPlot()
riskVsReturnPlot()
optimumPortfolio()
comparePopulPlot()
corrplot(cor,  order='hclust')
```


