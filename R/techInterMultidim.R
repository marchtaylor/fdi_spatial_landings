library(vegan)
# library(sinkr)
library(pals)

load("shiny/Data/fdi_ecoregion/Greater North Sea_data.Rdata")
head(data)


agg <- aggregate(
  totwghtlandg ~ icesname + gear_type + year + quarter + species, 
  data = data, FUN = sum, na.rm = T)

dim(agg)
head(agg)
names(agg)[6] <- "landings"


dat <- reshape2::dcast(data = agg, 
  formula = icesname + gear_type + year + quarter ~ species, 
  value.var = "landings", fill = 0)
dim(dat)


M <- as.matrix(dat[,-c(1:4)])
dim(M)


# Distance matrix
D <- cor(log(M+1)) # Pearson correlation of log landings
# D <- cor(M) # Pearson correlation of log landings

# Plot
heatmap(D, col = pals::ocean.balance(21), zlim = c(-1,1), symm =T, 
  margins = c(3,2))





