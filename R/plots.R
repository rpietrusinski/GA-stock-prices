#' Performance plots
#'
#' Generates plots with best speciman, average adaptation, standard deviation
#'
#' @return ggplot
#' @export
performancePlot <- function(){

  best <- data.frame(type = "best", value = result$value)
  mean <- data.frame(type = "mean", value = result$avg)
  std  <- data.frame(type = "std dev", value = result$std)
  m <- rbind(best, mean, std)
  m$iter <- seq(1:num_iter)

  return(
    ggplot2::ggplot(m, ggplot2::aes(x=iter, y=value, col=type)) +
      ggplot2::geom_line(lwd=1) +
      ggplot2::facet_wrap(~type, scales = "free", nrow = 3) +
      ggplot2::scale_color_discrete("Legend")
  )
}


#' Violin plots
#'
#' Generates violin plots - distributions of portfolio structure
#'
#' @return ggplot
#' @export
violinPlot <- function(){

  a <- as.data.frame(t(mapply(showPortfolio, result$specimen, num_genes, num_stocks)))
  a <- tidyr::gather(a, stock, value)

  return(
    ggplot2::ggplot(a, ggplot2::aes(x=stock, y=value, fill=stock)) +
      ggplot2::geom_violin() +
      ggplot2::stat_summary(fun.y=median, geom="point", size=2, color="red") +
      ggplot2::labs(title = "Distribution of portfolio shares",
           subtitle = "Based on the best portfolio in each generation",
           x="stock",
           y = "Share in portfolio")
  )
}


#' Risk vs return
#'
#' Generates risk vs return analysis
#'
#' @return ggplot
#' @export
riskVsReturnPlot <- function(){

  chrom_len <- num_stocks*num_genes
  a <- t(mapply(rbind, result$specimen))
  a <- as.data.frame(cbind(a, apply(a,1,portfolioReturn, num_genes=num_genes), apply(a,1,portfolioRisk,num_genes=num_genes)))
  a <- a[,c(chrom_len+1,chrom_len+2)]
  a[,3] <- a[,1]/a[,2]
  colnames(a) <- c("return", "risk","sharpe")


  ret <- data.frame(return=expectedReturn, risk=risk, share=names(expectedReturn))
  ret <- ret %>% dplyr::mutate(sharpe=return/risk)

  best.num <- which.max(result$value)
  best <- a[best.num,]

  return(
    ggplot2::ggplot(a, ggplot2::aes(x=risk, y=return, col=sharpe)) +
      ggplot2::geom_point(alpha=.3) +
      ggplot2::geom_point(data=best, ggplot2::aes(x=risk,y=return), col="red") +
      ggplot2::geom_text(data = ret, ggplot2::aes(x=risk,y=return,label=share),size=5, col="black") +
      ggplot2::labs(title="Risk vs Return of the best portfolios in each generation")
  )
}


#' Optimum portfolio
#'
#' Generates Optimum portfolio structure
#'
#' @return ggplot
#' @export
optimumPortfolio <- function(){

  best.num <- which.max(result$value)
  best <- result$specimen[best.num] %>%
    showPortfolio(num_genes=num_genes)
  best <- data.frame(names(best), best)
  colnames(best) <- c("stock", "share")
  best$label <- percent(best$share)
  best <- dplyr::arrange(best, stock)

  return(
    ggplot2::ggplot(best, ggplot2::aes(x=stock, y=share, label=label)) +
      ggplot2::geom_col(fill="orange") +
      ggplot2::labs(title="Optimum portfolio",
           y="Portfolio share") +
      ggplot2::geom_label(ggplot2::aes(x=1:num_stocks,y=0), inherit.aes = T)
  )

}



#' Compare first and last iter
#'
#' Generates comparison of populations in the first and the last generation
#'
#' @return ggplot
#' @export
comparePopulPlot <- function(){

  pierwsza <- data.frame(return = apply(result$popul[[1]], 1, portfolioReturn, num_genes=num_genes),
                         risk = apply(result$popul[[1]], 1, portfolioRisk, num_genes=num_genes))
  ostatnia <- data.frame(return = apply(result$popul[[num_iter]], 1, portfolioReturn, num_genes=num_genes),
                         risk = apply(result$popul[[num_iter]], 1, portfolioRisk, num_genes=num_genes))
  pierwsza <- pierwsza %>% dplyr::mutate(sharpe=return/risk, iter=1)
  ostatnia <- ostatnia %>% dplyr::mutate(sharpe=return/risk, iter=num_iter)
  d <- rbind(pierwsza, ostatnia)

  return(
    ggplot2::ggplot(d, ggplot2::aes(x=risk, y=return, col=factor(iter))) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(.~iter) +
      ggplot2::labs(x="Risk", y="Return") +
      ggplot2::scale_color_discrete("Generation")
  )

}

