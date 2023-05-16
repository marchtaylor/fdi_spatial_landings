# data link: 
# Gibin, Maurizio; Maksims Kovšars; Maciej Adamowicz; Zanzi, Antonella; 
# Hekim, Zeynep (2022): Fisheries landings & effort: data by c-square. 
# European Commission, Joint Research Centre (JRC) [Dataset] 
# PID: http://data.europa.eu/89h/00ae6659-ddde-4314-a9da-717bb2e82582

load("shiny/lutEcoRegion.Rdata")

fnames <- list.files("data/FDI_spatial_data_EU28/")
fnames <- fnames[grep(pattern = "spatial_landings", x = fnames)]
fnames

ecoRegions <- sort(unique(lutEcoRegion$ecoregion))


for(i in seq(ecoRegions)[-c(1:9)]){ # for each ecoregion
  
  res <- vector("list", length(fnames))
  for(j in seq(res)){
    # i = 9; j = 1
    # read data
    fdi.j <- read.csv(file.path("data/FDI_spatial_data_EU28", fnames[j]))
    # subset ecoregion and add to res
    mat <- match(fdi.j$icesname, lutEcoRegion$stat_rec)
    fdi.j$ecoregion <- lutEcoRegion$ecoregion[mat]
    res[[j]] <- subset(fdi.j, ecoregion == ecoRegions[i])
    print(paste("ecoregion =", ecoRegions[i], "| fname =", fnames[j]))
  }
  
  
  # subset species
  res2 <- do.call("rbind", res)
  
  res2 <- unique(res2[,c("ecoregion", "year", "quarter", "vessel_length", "fishing_tech", "gear_type", "mesh_size_range", 
      "target_assemblage", "metier", "icesname", "species", "totwghtlandg")])
  
  if(nrow(res2)>1000){
    agg <- aggregate(totwghtlandg ~ species, data = res2, FUN = sum, na.rm = TRUE)
    agg <- agg[order(agg$totwghtlandg, decreasing = TRUE),]
    agg$rank <- seq(nrow(agg))
    data <- subset(res2, subset = species %in% c(agg$species[seq(20)]))
    
    # save ecoregion output
    save(data, file = file.path("shiny/Data/fdi_ecoregion/", paste0(ecoRegions[i], "_data.Rdata")))
  }
  rm(res); rm(res2)
}

fnames <- list.files("shiny/Data/fdi_ecoregion/")
fnames <- fnames[grep(pattern = "_data.Rdata", x = fnames)]
fnames

ecoRegions <- unlist(lapply(strsplit(fnames, split = "_"), FUN = function(x){x[1]}))
save(ecoRegions, file = "shiny/Data/fdi_ecoregion/ecoRegions.csv")
