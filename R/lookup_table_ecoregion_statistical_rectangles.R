library(icesVMS)


tmp <- get_csquare()
dim(tmp)

lutEcoRegion <- unique(tmp[,c("stat_rec", "ecoregion")])
rownames(lutEcoRegion) <- seq(nrow(lutEcoRegion))
lutEcoRegion

lutEcoRegion <- subset(lutEcoRegion, subset = !is.na(ecoregion))
save(lutEcoRegion, file = "shiny/lutEcoRegion.Rdata")

