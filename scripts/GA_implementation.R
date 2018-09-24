# AUXILIARY FUNCTIONS
require(dplyr)
require(ggplot2)
require(tidyr)
require(RColorBrewer)
require(corrplot)
require(scales)
require(beepr)
require(doParallel)
require(foreach)


prepareData <- function(data){
  # Function prepares all the necessary data for further analysis. Requires the dataset loaded with @loadData function from scraper.R.
  # 
  # Parameters:
  # -----------
  # data - raw dataset loaded with @loadData function   
  #   
  # Returns:
  # --------
  # data - modified dataset
  # chng - matrix with percentage changes in stock prices
  # cor, cov, expectedReturn, risk - measures of stocks under analysis
  
  
  # Count percentage differences of stock prices (based on the closing price)
  data <- lapply(data, FUN = function(x){ x %>% 
      mutate(Zmiana = (Zamkniecie-lag(Zamkniecie))/lag(Zamkniecie))})
  
  # Create matrix with stock prices monthly percentage changes
  chng <- sapply(1:5, FUN = function(x) rbind(data[[x]][,7]))[-1,]
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
}





# Create an auxiliary function for decoding a number in binary system into a number in decimal system
binToDec <- function(x){
  return(sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1)))
}
  

# Create an auxiliary function for counting a single speciman adaptation (single portfolio quality)
singleAdapt <- function(x, num_genes, expectedReturn){
  # Function takes @x which is a single specimen (sequence of bits) and returns a value of its adaptation. The adaptation value is the
  # expected portfolio return / portfolio risk.
  # 
  # Parameters:
  # -----------
  # x - specimen, which is a sequence of bits
  # num_genes - number of bits encoding single stock
  # expectedReturn - A vector with expected return rates for analysed stocks
  #   
  # Returns:
  # --------
  # Value of adaptation
  

  num_genes=num_genes
  expectedReturn=expectedReturn
  
  y <- paste(unlist(x), collapse="")
  y <- sapply(1:5, FUN = function(z) substring(y,(z-1)*num_genes+1, z*num_genes))
  portfolioShares <- sapply(y,binToDec)
  portfolioShares <- portfolioShares / sum(portfolioShares)
  expectedPortfolioReturn <- sum(portfolioShares * expectedReturn)
  
  portfolioRisk <- 0
  for(i in 1:5){
    for(j in 1:5){
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



showPortfolio <- function(x, num_genes){
  # Function decodes sequence of bits in @x and returns the portfolio structure.
  # 
  # Parameters:
  # -----------
  # x - single portfolio (sequence of bits)
  # num_genes - number of bits encoding single stock
  #   
  # Returns:
  # --------
  # Vector with portfolio structure.
  
  
  y <- paste(unlist(x), collapse="")
  y <- sapply(1:5, FUN = function(z) substring(y,(z-1)*num_genes+1, z*num_genes))
  portfolioShares <- sapply(y,binToDec)
  portfolioShares <- portfolioShares / sum(portfolioShares)
  names(portfolioShares) <- names(expectedReturn)
  return(portfolioShares)
}


portfolioReturn <- function(x, num_genes){
  # Function decodes sequence of bits in @x and returns expected return for a given portfolio.
  # 
  # Parameters:
  # -----------
  # x - single portfolio (sequence of bits)  
  # num_genes - number of bits encoding single stock
  #   
  # Returns:
  # --------
  # Value for expected return
  

  e <- sum(showPortfolio(unlist(x), num_genes) * expectedReturn)
  return(e)
}


portfolioRisk <- function(x, num_genes){
  # Function decodes sequence of bits in @x and returns value of risk for a given portfolio.
  # 
  # Parameters:
  # -----------
  # x - single portfolio (sequence of bits)  
  # num_genes - number of bits encoding single stock
  #   
  # Returns:
  # --------
  # Value of risk
  

  share <- showPortfolio(unlist(x), num_genes)
  portfolioRisk <- 0
  for(i in 1:5){
    for(j in 1:5){
      portfolioRisk <- portfolioRisk + share[i] * share[j] * cov[i,j]
    }
  }
  return(sqrt(portfolioRisk))
}



# 2. IMPLEMENTATION OF GENETIC ALGORITHM
genalg <- function(num_iter = 200,
                   num_spec = 400,
                   num_genes = 10, 
                   p_cross = 0.9,
                   p_mutant = 0.01){ 
  
  # Function implements genetic algorithm.
  # A single speciman consists of 5 stocks, each encoded by @num_genes bits. Therefore a speciman is encoded with @num_genes * 5 bits. 
  # During each iteration only a single stock (bits encoding one stock) is taken into accunt for crossover.
  # 
  # 
  # Parameters:
  # -----------
  # num_iter - Number of iterations
  # num_spec - Number of specimen (must be even)
  # num_genes - Number of number of bits encoding a single stock
  # p_cross - Crossing-over probability
  # p_mutant - Mutation probability
  #   
  # Returns:
  # --------
  # List with the following components:
  #   $value - maximum adaptaion for each generation
  #   $avg - average adaptation for each generation
  #   $std - standard deviation of adaptation for each generation
  #   $specimen - Sequence of bits encoding the best speciman in each generation
  #   $popul - the whole population for the first and the last generation
  
  
  
  totalBest <- list()
  chrom_len <- num_genes*5
  
  # Initiate random population
  popul1 <- matrix(data = rbinom(num_spec*chrom_len, 1, .5), nrow = num_spec, ncol = chrom_len)
  popul2 <- matrix(nrow = num_spec, ncol = chrom_len)
  
  k=1
  while(k<=num_iter){
    
    # Add adaptation value for each speciman
    popul1 <- cbind(popul1, apply(popul1, 1, singleAdapt, num_genes=num_genes, expectedReturn=expectedReturn)) 
    
    # Save best result / average result, standard deviation and portfolio structure
    maxGeneration <- max(popul1[,ncol(popul1)])
    avgGeneration <- mean(popul1[,ncol(popul1)])
    stdGeneration <- sd(popul1[,ncol(popul1)])
    totalBest$value[k] <- maxGeneration
    totalBest$avg[k] <- avgGeneration
    totalBest$std[k] <- stdGeneration
    totalBest$specimen[k] <- data.frame(popul1[which.max(popul1[,ncol(popul1)]), 1:chrom_len])
    # Save the whole population from the first and the last generation
    if(k==1 | k==num_iter){
      totalBest$popul[[k]] <- data.frame(popul1[,1:chrom_len])
    }
    
    
    # SELECTION - ROULETTE METHOD
    popul1 <- cbind(popul1, cumsum(popul1[,ncol(popul1)]) / sum(popul1[,ncol(popul1)]), 0, 0)
    popul1[1,ncol(popul1)] <- popul1[1,ncol(popul1)-2]
    for (i in 2:num_spec) { 
      popul1[i,ncol(popul1)-1]=popul1[i-1,ncol(popul1)]
      popul1[i,ncol(popul1)]=popul1[i,ncol(popul1)-2]
    }
    
    
    for (i in 1:num_spec) {
      los <- runif(1,0,1)
      for (j in 1:num_spec) { 
        if(los >= popul1[j,ncol(popul1)-1] & los < popul1[j,ncol(popul1)]){
          popul2[i,] <- popul1[j,1:chrom_len]
        }
      }
    }
    
    # CROSS-OVER
    # cross a single stock (component of speciment) and not the whole speciman
    specimenVec <- sample.int(num_spec)
    for (i in 1:(num_spec/2)) { 
      s1 <- specimenVec[1]
      s2 <- specimenVec[2]
      los <- runif(1,0,1) 
      if (los <= p_cross) { 
        pos <- sample(1:5, 1)
        cut <- round(runif(1, 1, num_genes-1))
        
        p1 <- popul2[s1, ((pos-1)*num_genes+1):(pos*num_genes)]
        p2 <- popul2[s2, ((pos-1)*num_genes+1):(pos*num_genes)]
        
        c1 <- p1[(cut+1):num_genes]
        c2 <- p2[(cut+1):num_genes]
        
        p1[(cut+1):num_genes] <- c2
        p2[(cut+1):num_genes] <- c1
        
        popul2[s1, ((pos-1)*num_genes+1):(pos*num_genes)] <- p1
        popul2[s2, ((pos-1)*num_genes+1):(pos*num_genes)] <- p2
        
        specimenVec <- specimenVec[-(1:2)]
      } else {
        specimenVec <- specimenVec[-(1:2)]
      }
    }
    
    # MUTATION
    for (i in 1:num_spec) { 
      for (j in 1:chrom_len) {
        los=runif(1,0,1)
        if (los <= p_mutant) { 
          if (popul2[i,j]==1) {
            popul2[i,j]=0
          }  else {
            popul2[i,j]=1 
          }
        }
      }
    }
    
    # GENERATION EXCHANGE
    popul1 <- popul2
    print(k)
    k <- k + 1
  }
  return(totalBest)
}