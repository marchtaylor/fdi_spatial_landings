library(icesVMS)

ecoRegions <- c("Azores", "Baltic Sea", "Barents Sea", 
  "Bay of Biscay and Iberian Coast", "Celtic Seas", "Faroes", 
  "Greater North Sea", "Greenland Sea", "Icelandic Waters", 
  "Norwegian Sea", "Oceanic Northeast Atlantic")

RES <- vector("list", length = length(ecoRegions))
names(RES) <- ecoRegions
pb <- txtProgressBar(max = length(RES), style = 3)
for(i in seq(RES)){
  tmp <- get_csquare(ecoregion = "Greater North Sea")
  
  RES[[i]] <- data.frame(stat_rec = unique(tmp$stat_rec), ecoregion = ecoRegions[i])
  setTxtProgressBar(pb, i)
  
}
close(pb)

lutEcoRegion <- do.call("rbind", RES)
rownames(lutEcoRegion) <- seq(nrow(lutEcoRegion))
lutEcoRegion

save(lutEcoRegion, file = "shiny/lutEcoRegion.Rdata")
