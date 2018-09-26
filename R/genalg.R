#' Function implements genetic algorithm.
#'
#' A single speciman consists of *num_stock* stocks, each encoded by *num_genes* bits. Therefore a speciman is encoded with num_genes *
#' num_stocks bits.
#' During each iteration only a single stock (bits encoding one stock) is taken into accunt for crossover.
#'
#' @param num_iter Number of iterations
#' @param num_spec Number of specimen (must be even)
#' @param num_genes Number of number of bits encoding a single stock
#' @param p_cross Crossing-over probability
#' @param p_mutant Mutation probability
#' @param num_stocks. Number of stocks in analysis
#' @param expectedReturn. Vector of expected returns (returned by prepareData)
#'
#' @return List with the following components:
#' @return $value - maximum adaptaion for each generation
#' @return $avg - average adaptation for each generation
#' @return $std - standard deviation of adaptation for each generation
#' @return $specimen - Sequence of bits encoding the best speciman in each generation
#' @return $popul - the whole population for the first and the last generation
#' @export
genalg <- function(num_iter = 200,
                   num_spec = 400,
                   num_genes = 10,
                   p_cross = 0.9,
                   p_mutant = 0.01,
                   num_stocks. = num_stocks,
                   expectedReturn. = expectedReturn){



  totalBest <- list()
  chrom_len <- num_genes*num_stocks.

  # Initiate random population
  popul1 <- matrix(data = rbinom(num_spec*chrom_len, 1, .5), nrow = num_spec, ncol = chrom_len)
  popul2 <- matrix(nrow = num_spec, ncol = chrom_len)

  k=1
  while(k<=num_iter){

    # Add adaptation value for each speciman
    popul1 <- cbind(popul1, apply(popul1, 1, singleAdapt, num_genes, expectedReturn., num_stocks.))

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
        pos <- sample(1:num_stocks., 1)
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
