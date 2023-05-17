
load("shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.Rdata")


saveRDS(data, file = "Azores_data", ascii = FALSE, version = NULL,
        compress = FALSE, refhook = NULL)
readRDS(file, refhook = NULL)
library(data.table)
fwrite(data, file = "shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.csv")
fread(file = "shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.csv")

