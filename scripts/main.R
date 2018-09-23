# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(corrplot)
library(scales)
library(beepr)
rm(list=ls())

source("scripts/scraper.R")
source("scripts/GA.R")


# Load data for 5 biggest polish companies
tickers <- c("kgh","pko","peo","pkn","pzu")
data <- lapply(tickers, loadData, start=20150701, end=20170831)

# Prepare data
prepareData(data)
  
# RUN EXPERIMENT
l_iter = 200
l_osob = 400
l_alle = 10
p_cross = 0.9
p_mutant = 0.01

system.time({
  result <- genalg(l_iter = l_iter, 
                   l_osob = l_osob, 
                   l_alle = l_alle, 
                   p_cross = p_cross,
                   p_mutant = p_mutant)
})



# ANALYSIS

# 1. best speciman, average adaptation, standard deviation
best <- data.frame(type = "best", value = result$value)
mean <- data.frame(type = "mean", value = result$avg)
std  <- data.frame(type = "std dev", value = result$std)
m <- rbind(best, mean, std)
m$iter <- seq(1:200)

ggplot(m, aes(x=iter, y=value, col=type)) +
  geom_line(lwd=1) +
  facet_wrap(~type, scales = "free", nrow = 3) +
  scale_color_discrete("Legend")


#2 violin plots - distributions of portfolio structure
a <- as.data.frame(t(mapply(showPortfolio, result$specimen, l_alle=l_alle)))
head(a)
summary(a)
a <- gather(a, stock, value, names(a))

ggplot(a, aes(x=stock, y=value, fill=stock)) + 
  geom_violin() + 
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  labs(title = "Rozkład udziałów akcji w optymalnym portfelu",
       subtitle = "Wyniki na podstawie analizy najlepszych osobników w 200 pokoleniach",
       x="stock", 
       y = "Udział w portfelu")


#3 risk vs return analysis
a <- t(mapply(rbind, result$specimen))
a <- as.data.frame(cbind(a, apply(a,1,portfolioReturn, l_alle=l_alle), apply(a,1,portfolioRisk,l_alle=l_alle)))
a <- a[,51:52]
a[,3] <- a[,1]/a[,2]
colnames(a) <- c("return", "risk","sharpe")


ret <- data.frame(return=expectedReturn, risk=risk, share=names(expectedReturn))
ret <- ret %>% mutate(sharpe=return/risk)

best.num <- which.max(result$value)
best <- a[best.num,]
ggplot(a, aes(x=risk, y=return, col=sharpe)) +
  geom_point(alpha=.3) +
  geom_point(data=best, aes(x=risk,y=return), col="red") +
  # guides(size=F, col = F) + 
  geom_text(data = ret, aes(x=risk,y=return,label=share),size=5, col="black") + 
  geom_text(aes(x=0.062, y = 0.02, label="best result"), col="red") + 
  labs(title="Risk vs Return of the best portfolios in each generation")


#4 Optimum portfolio structure
best.num <- which.max(result$value)
best <- result$specimen[best.num] %>%
  showPortfolio(l_alle=l_alle)
best <- data.frame(names(best), best)
colnames(best) <- c("stock", "share")
labels <- percent(best$share)

pal <- brewer.pal(5, "Dark2")
ggplot(best, aes(x=stock, y=share)) + 
  geom_col(fill="orange") + 
  labs(title="Optymalny portfel",
       y="Udzial w portfelu") +
  geom_label(aes(x=1:5,y=0, label=labels))


#5 Corrplot 
corrplot(cor, order="hclust")



#6 kolorowy sharpe
x=seq(0.05,0.15,length.out = 100)
y=seq(-0.005,0.025,length.out = 100)
m <- expand.grid(x,y)
m[,3] <- m[,2]/m[,1]
m <- as.data.frame(m)
colnames(m) <- c("x","y","sharpe")

a <- t(mapply(rbind, result$specimen))
a <- as.data.frame(cbind(a, apply(a,1,portfolioReturn, l_alle=l_alle), apply(a,1,portfolioRisk, l_alle=l_alle)))
a <- a[,51:52]
a[,3] <- a[,1]/a[,2]
colnames(a) <- c("return", "risk","sharpe")

ret <- data.frame(return=expectedReturn, risk=risk, share=names(expectedReturn))
ret <- ret %>% mutate(sharpe=return/risk)

best.num <- which.max(result$value)
best <- a[best.num,]
ggplot(a, aes(x=risk, y=return)) +
  geom_point(alpha=.3) +
  geom_point(data=best, aes(x=risk,y=return), col="red") +
  guides(col = F) + 
  geom_text(data = ret, aes(x=risk,y=return,label=share),size=5, col="black") + 
  geom_text(aes(x=0.062, y = 0.02, label="best result"), col="red") + 
  labs(title="Risk vs Return of the best portfolios in each generation") + 
  geom_tile(data=m, aes(x=x,y=y, fill=sharpe), alpha=.4, inherit.aes = F) +
  scale_fill_gradientn(colours = c("red","green")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())





#7 Compare populations in the first and the last generation
pierwsza <- data.frame(return = apply(result$popul[[1]], 1, portfolioReturn, l_alle=l_alle), 
                       risk = apply(result$popul[[1]], 1, portfolioRisk, l_alle=l_alle))
ostatnia <- data.frame(return = apply(result$popul[[200]], 1, portfolioReturn, l_alle=l_alle), 
                       risk = apply(result$popul[[200]], 1, portfolioRisk, l_alle=l_alle))
pierwsza <- pierwsza %>% mutate(sharpe=return/risk, iter=1)
ostatnia <- ostatnia %>% mutate(sharpe=return/risk, iter=200)
d <- rbind(pierwsza, ostatnia)

x=seq(0.04,0.1,length.out = 100)
y=seq(0,0.022,length.out = 100)
m <- expand.grid(x,y)
m[,3] <- m[,2]/m[,1]
m <- as.data.frame(m)
colnames(m) <- c("x","y","sharpe")
 
ggplot(m, aes(x=x,y=y,z=sharpe)) +
  stat_contour(col = "black", lty = 2) +
  geom_point(data=d, aes(x=risk, y=return, col=factor(iter)), inherit.aes = F) +
  facet_grid(.~iter) +
  labs(x="Risk",
       y="Return") +
  scale_color_discrete("Generation")










