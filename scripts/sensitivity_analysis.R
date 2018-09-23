# SENSITIVITY ANALYSIS
library(ggplot2)
library(doParallel)
library(foreach)


# Establish cluster for parallel computations
cl <- makeCluster(3)
registerDoParallel(cl)

# Sensitivity analysis is being conducted for the following parameters:
  # l_osob - Number of species (should be even)
  # l_alle - Number of number of bits encoding a single stock
  # p_cross - Crossing-over probability
  # p_mutant - Mutation probability


#l_osob
z <- c(10,20,50,100,200,400,600)
result.l_osob <- foreach(i = z) %dopar% {genalg(l_osob = i)}

#l_alle
z <- c(3,5,10,15,20)
result.l_alle <- foreach(i = z) %dopar% {genalg(l_alle = i)}

#p_cross
z <- c(0.2, 0.5, 0.8, 0.9, 1)
result.p_cross <- foreach(i = z) %dopar% {genalg(p_cross = i)}

#p_mutant
z <- c(0, 0.01, 0.03, 0.05, 0.1, 0.2)
result.p_mutant <- foreach(i = z) %dopar% {genalg(p_mutant = i)}




# ANALYSIS

#l_osob
l_osob <- data.frame(param = "l_osob",
                     value = c(10,20,50,100,200,400,600),
                     max = sapply(1:7, FUN = function(x) max(result.l_osob[[x]]$value)),
                     avg = sapply(1:7, FUN = function(x) mean(result.l_osob[[x]]$avg)))

#l_alle
l_alle <- data.frame(param = "l_alle",
                     value = c(3,5,10,15,20),
                     max = sapply(1:5, FUN = function(x) max(result.l_alle[[x]]$value)),
                     avg = sapply(1:5, FUN = function(x) mean(result.l_alle[[x]]$avg)))

#p_cross
p_cross <- data.frame(param = "p_cross",
                 value = c(0.2, 0.5, 0.8, 0.9, 1),
                 max = sapply(1:5, FUN = function(x) max(result.p_cross[[x]]$value)),
                 avg = sapply(1:5, FUN = function(x) mean(result.p_cross[[x]]$avg)))

#p_mutant
p_mutant <- data.frame(param = "p_mutant",
                 value = c(0, 0.01, 0.03, 0.05, 0.1, 0.2),
                 max = sapply(1:6, FUN = function(x) max(result.p_mutant[[x]]$value)),
                 avg = sapply(1:6, FUN = function(x) mean(result.p_mutant[[x]]$avg)))



df <- rbind(l_osob, l_alle, p_cross, p_mutant)

ggplot(df, aes(x=max, y=avg, col=param)) +
  geom_point(size = 2) +
  facet_wrap(~param)

ggplot(l_osob, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="l_osob",
       subtitle="Liczba osobnkow w pokoleniu")

ggplot(l_alle, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="l_alle",
       subtitle="Liczba alleli kodujacych pojedynczy papier wartosciowy") +
  scale_x_continuous(limits=c(0.275, 0.285))

ggplot(p_cross, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="p_cross",
       subtitle="Prawdopodobienstwo krzyzowania")

ggplot(p_mutant, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="p_mutant",
       subtitle="Prawdopodobienstwo mutacji")








