# SENSITIVITY ANALYSIS
rm(list=ls())
source("./scripts/GA_helpers.R")
source("./scripts/scraper.R")


# Load data for 5 biggest polish companies
tickers <- c("kgh","pko","peo","pkn","pzu")
data <- lapply(tickers, loadData, start=20150701, end=20170831)

# Prepare data
prepareData(data)

# Establish cluster for parallel computations
cl <- makeCluster(3)
registerDoParallel(cl)

# Sensitivity analysis is being conducted for the following parameters:
  # num_spec - Number of species (should be even)
  # num_genes - Number of bits encoding a single stock
  # p_cross - Crossover probability
  # p_mutant - Mutation probability


#num_spec
z <- c(10,20,50,100,200,400,600)
result.num_spec <- foreach(i = z) %dopar% {genalg(num_spec = i)}

#num_genes
z <- c(3,5,10,15,20)
result.num_genes <- foreach(i = z) %dopar% {genalg(num_genes = i)}

#p_cross
z <- c(0.2, 0.5, 0.8, 0.9, 1)
result.p_cross <- foreach(i = z) %dopar% {genalg(p_cross = i)}

#p_mutant
z <- c(0, 0.01, 0.03, 0.05, 0.1, 0.2)
result.p_mutant <- foreach(i = z) %dopar% {genalg(p_mutant = i)}




# ANALYSIS

#num_spec
num_spec <- data.frame(param = "num_spec",
                     value = c(10,20,50,100,200,400,600),
                     max = sapply(1:7, FUN = function(x) max(result.num_spec[[x]]$value)),
                     avg = sapply(1:7, FUN = function(x) mean(result.num_spec[[x]]$avg)))

#num_genes
num_genes <- data.frame(param = "num_genes",
                     value = c(3,5,10,15,20),
                     max = sapply(1:5, FUN = function(x) max(result.num_genes[[x]]$value)),
                     avg = sapply(1:5, FUN = function(x) mean(result.num_genes[[x]]$avg)))

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



df <- rbind(num_spec, num_genes, p_cross, p_mutant)

ggplot(df, aes(x=max, y=avg, col=param)) +
  geom_point(size = 2) +
  facet_wrap(~param)

ggplot(num_spec, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="num_spec",
       subtitle="Number of species")

ggplot(num_genes, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="num_genes",
       subtitle="Number of bits encoding a single stock") +
  scale_x_continuous(limits=c(0.275, 0.285))

ggplot(p_cross, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="p_cross",
       subtitle="Crossover probability")

ggplot(p_mutant, aes(x=max, y=avg, label=value)) +
  geom_text(size=5) +
  labs(title="p_mutant",
       subtitle="Mutation probability")








