library(pals)
library(reshape2)


# required data -----------------------------------------------------------

load("shiny/Data/fdi_ecoregion/Greater North Sea_data.Rdata")

# aggregate data and cast species dimension as columns -----------------------------------------------------

agg <- aggregate(
  totwghtlandg ~ icesname + gear_type + year + quarter + species, 
  data = data, FUN = sum, na.rm = T)
names(agg)[6] <- "landings"

dat <- reshape2::dcast(data = agg, 
  formula = icesname + gear_type + year + quarter ~ species, 
  value.var = "landings", fill = 0)

# calculate distance matrix among species-----------------------------------------------

M <- as.matrix(dat[,-c(1:4)])
D <- cor(log(M+1)) # Pearson correlation of log landings
# D <- cor(M) # Pearson correlation of log landings

# plot distance matrix as heatmap ------------------------------------------------------------

COL <- c("#181C43", "#232D6B", "#273D98", "#1554BA", "#166FBB", "#3787BA", 
  "#599BBA", "#81AFC0", "#A8C1CB", "#CDD6D9", "#F0ECEB", "#E5CEC7", 
  "#DBB0A3", "#D2937E", "#C8765A", "#BF583B", "#B23727", "#9C1826", 
  "#7E0E28", "#5C0D20", "#3C0912")

heatmap(D, col = COL, zlim = c(-1,1), symm =T, 
  margins = c(3,2))





