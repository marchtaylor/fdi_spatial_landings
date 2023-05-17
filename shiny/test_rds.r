
load("shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.Rdata")


saveRDS(data, file = "Azores_data", ascii = FALSE, version = NULL,
        compress = FALSE, refhook = NULL)
readRDS(file, refhook = NULL)
library(data.table)
fwrite(data, file = "shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.csv")
fread(file = "shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.csv")




all.files<- c("shiny/Data/fdi_ecoregion/Azores_data.Rdata",
"shiny/Data/fdi_ecoregion/Baltic Sea_data.Rdata",
"shiny/Data/fdi_ecoregion/Barents Sea_data.Rdata",
"shiny/Data/fdi_ecoregion/Bay of Biscay and the Iberian Coast_data.Rdata", 
"shiny/Data/fdi_ecoregion/Celtic Seas_data.Rdata",
"shiny/Data/fdi_ecoregion/Faroes_data.Rdata" ,
"shiny/Data/fdi_ecoregion/Greater North Sea_data.Rdata",
"shiny/Data/fdi_ecoregion/Greenland Sea_data.Rdata",
"shiny/Data/fdi_ecoregion/Norwegian Sea_data.Rdata",
"shiny/Data/fdi_ecoregion/Oceanic Northeast Atlantic_data.Rdata", 
"shiny/Data/fdi_ecoregion/Western Mediterranean Sea_data.Rdata")

names_eco<- c("Azores","Baltic Sea", "Barents Sea","Bay of Biscay and the Iberian Coast", "Celtic Seas", "Faroes", "Greater North Sea", "Greenland Sea","Norwegian Sea", "Oceanic Northeast Atlantic" ,"Western Mediterranean Sea")

mylist<- lapply(all.files, function(x) {
  load(file = x)
  data$ecoregion
})

# Create an empty list to store the data frames
data_list <- list()

# Set the directory path where the RData files are located
directory_path <- "D:/Profile/Documents/GitHub/fdi_spatial_landings/shiny/Data/fdi_ecoregion"

# Get a list of all RData files in the directory
file_list <- list.files(directory_path, include.dirs = TRUE)
setwd("D:/Profile/Documents/GitHub/fdi_spatial_landings/shiny/Data/fdi_ecoregion")
# Iterate through each RData file
for (i in all.files) {
  # Load the data from the RData file
  load(all.files[i])
  data$ecoregion <- names_eco[i]
  # Extract the name of the file without the extension
#   file_name <- basename(file)
#   file_name <- sub(".RData", "", file_name)
  
#   # Add the file name as a new column in the data frame
#   data <- cbind(get(ls()), file_name)
  
#   # Append the data frame to the list
  data_list[[length(data_list) + 1]] <- data
}

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, data_list)

# Print the combined data frame
print(combined_data)



# Set the directory where your .Rdata files are located
setwd("shiny/Data/fdi_ecoregion")

# List all .Rdata files in the directory
file_list <- list.files(pattern = "\\.Rdata$")

# Create an empty dataframe to store the combined data
combined_df <- data.frame()

# Loop through each file and load the data
for (file in file_list) {
  load(file)
  
  # Assuming the objects in the .Rdata files are data frames,
  # you can append them to the combined_df using rbind
  combined_df <- rbind(combined_df, data)
}

# Reset the row names of the combined dataframe
row.names(combined_df) <- NULL
dim(combined_df)
# Now, the combined_df contains the collated data from all the .Rdata files
setwd("D:/Profile/Documents/GitHub/fdi_spatial_landings")
file_path <- "shiny/Data/fdi_ecoregion/all_ecoregions_data.rds"
saveRDS(combined_df, file = file_path)

combined_df <- readRDS("shiny/Data/fdi_ecoregion/all_ecoregions_data.rds")
