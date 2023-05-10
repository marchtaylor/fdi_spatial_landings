# data link: 
# Gibin, Maurizio; Maksims Kovšars; Maciej Adamowicz; Zanzi, Antonella; 
# Hekim, Zeynep (2022): Fisheries landings & effort: data by c-square. 
# European Commission, Joint Research Centre (JRC) [Dataset] 
# PID: http://data.europa.eu/89h/00ae6659-ddde-4314-a9da-717bb2e82582
fnames <- list.files("data/FDI_spatial_data_EU28/")
fnames <- fnames[grep(pattern = "spatial_landings", x = fnames)]
fnames

res <- vector("list", length(fnames))
pb = txtProgressBar(min = 0, max = length(res), style = 3) 
for(i in seq(res)){
  tmp <- read.csv(file.path("data/FDI_spatial_data_EU28", fnames[i]))
  tmp <- subset(tmp, sub_region %in% c("27.4.A", "27.4.B", "27.4.C") & 
    species %in% c("BLL", "COD", "DAB", "GUR", "HAD", "NEP", "PLE", "POK", "SOL", "TUR", "WHG", "WIT"))
  tmp <- unique(tmp[,c("year", "quarter", "vessel_length", "fishing_tech", "gear_type", "mesh_size_range", 
      "target_assemblage", "metier", "icesname", "species", "totwghtlandg")])
  res[[i]] <- tmp
  setTxtProgressBar(pb, i)
}
close(pb)

data <- do.call("rbind", res)
dim(data)

save(data, file = "shiny/data.Rdata")
# load(file = "shiny/data.Rdata")


