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


for(i in seq(ecoRegions)){ # for each ecoregion
  
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
    
    if(i == 9){
      sppSub <- c("BLL", "COD", "DAB", "GUR", "HAD", "HER", "HKE", "HOM", "LEM", "NEP", "PLE", "POK", 
        "SAN", "SPR", "SOL", "TUR", "MAC", "WHG", "WIT", "USK")
    }else{
      sppSub <- c(agg$species[seq(20)])
    }
    
    data <- subset(res2, subset = species %in% sppSub)
    
    mesh_size_split <- strsplit(data$mesh_size_range, "D")
    mesh_sizes <- suppressWarnings(as.numeric(do.call("c", mesh_size_split)))
    mesh_size_range <- range(unique(mesh_sizes), na.rm = TRUE)
    mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(length(x)<2){c(mesh_size_range)}else{x}})
    mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(x[2]=="XX"){c(x[1], mesh_size_range[2])}else{x}})
    data$mesh_size_min <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[1]})))
    data$mesh_size_max <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[2]})))
    data <- as.data.table(data)
    
    # save ecoregion output
    save(data, file = file.path("shiny/Data/fdi_ecoregion/", paste0(ecoRegions[i], "_data.Rdata")))
  }
  rm(res); rm(res2)
}

fnames <- list.files("shiny/Data/fdi_ecoregion/")
fnames <- fnames[grep(pattern = "_data.Rdata", x = fnames)]
fnames

ecoRegions <- unlist(lapply(strsplit(fnames, split = "_"), FUN = function(x){x[1]}))
tmp <- data.frame("ecoRegion" = ecoRegions)
save(tmp, file = "shiny/Data/fdi_ecoregion/ecoRegions.csv")
