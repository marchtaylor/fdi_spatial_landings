library(icesVMS)


tmp <- get_csquare()
dim(tmp)
names(tmp)

lutEcoRegion <- unique(tmp[,c("stat_rec", "ices_area", "ecoregion")])
lutEcoRegion <- lutEcoRegion[order(lutEcoRegion$ecoregion, lutEcoRegion$stat_rec),]
rownames(lutEcoRegion) <- seq(nrow(lutEcoRegion))
lutEcoRegion

lutEcoRegion <- subset(lutEcoRegion, subset = !is.na(ecoregion))
save(lutEcoRegion, file = "shiny/lutEcoRegion.Rdata")

