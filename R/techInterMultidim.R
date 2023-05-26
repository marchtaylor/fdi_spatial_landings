
# required libraries ------------------------------------------------------

library(pals)
library(data.table)
library(vegan)

# required data -----------------------------------------------------------

load("shiny/Data/fdi_ecoregion/Greater North Sea_data.Rdata")

# aggregate data using data.table functions ----------------------------------
data <- as.data.table(data)

agg <- data[, .(landings = sum(totwghtlandg, na.rm = TRUE)), 
            by = .(icesname, gear_type, year, quarter, species)]
agg[, landingsSum := sum(landings), by = .(year, species)]
agg[, landingsPerc := landings/landingsSum]

# reshape data using dcast from data.table ----------------------------------
dat <- dcast(agg, icesname + gear_type + year + quarter ~ species, 
             value.var = "landingsPerc", fill = 0)

# calculate distance matrix among species using base R dist ---------------

M <- as.matrix(dat[, -c(1:4)])
# D <- cor(log(M+1)) # Pearson correlation of log landings
# D <- cor(M) # Pearson correlation of log landings
D <- vegdist(t(M), method = "bray")

# plot distance matrix as heatmap -------------------------------------------

COL <- pals::brewer.spectral(100)

png("output/techInterMultidim_ex.png", width = 5, height = 5, units = "in", res = 400)
heatmap(as.matrix(D), col = COL, zlim = c(0, 1), symm = TRUE, margins = c(3, 4))
dev.off()

