
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

COL <- pals::brewer.spectral(21)
# COL <- grey.colors(21, gamma = 0.2, start = 0.1, end = 0.9)

png("output/techInterMultidim_ex.png", width = 5, height = 5, units = "in", res = 400)
heatmap(as.matrix(D), col = COL, zlim = c(0, 1), symm = TRUE, margins = c(3, 4))
dev.off()





# CCA ---------------------------------------------------------------------

comm <- dat[, -c(1:4)]
env <- dat[, c(1:4)]
tmp <- mapplots::ices.rect(env$icesname)
head(tmp)
env <- cbind(env, tmp)
keep <- which(rowSums(comm)>0)
comm <- comm[keep,]
env <- env[keep,]
env$yeardec <- env$year + env$quarter/4
str(env)

head(comm)
head(env)

fit <- cca(comm ~ gear_type + lon*lat, data = env)
summary(fit)

(An <- anova.cca(fit,  permutations = 9))

(An <- anova.cca(fit, by = "margin", permutations = 9))

plot(fit, display = c("sp", "cn"))
