#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(mapplots)
library(maps)
library(mapdata)
library(pals)
library(leaflet)
library(sf)
library(shinyjs)
library(reshape2)
library(data.table)

source("imageDimnames.r")
source("plotCor.r")
source("utilities_load_ecoregion_shp.r")
source("utilities_ecoregion_mapping.r")


# load function -----------------------------------------------------------
source("imageDimnames.r")


# Load data
# load("data.Rdata")
load("Data/fdi_ecoregion/Greater North Sea_data.Rdata")

# extract mesh size ranges
mesh_size_split <- strsplit(data$mesh_size_range, "D")
mesh_sizes <- suppressWarnings(as.numeric(do.call("c", mesh_size_split)))
mesh_size_range <- range(unique(mesh_sizes), na.rm = TRUE)
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(length(x)<2){c(mesh_size_range)}else{x}})
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(x[2]=="XX"){c(x[1], mesh_size_range[2])}else{x}})
data$mesh_size_min <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[1]})))
data$mesh_size_max <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[2]})))

# data <- as.data.table(data)  

# Define UI
ui <- fluidPage(
  # Sidebar with filters
  sidebarLayout(
    sidebarPanel(width = 3,
      leafletOutput("map_ecoregion"),

      selectizeInput(
        inputId = "selected_locations",
        label = "ICES Ecoregions",
        choices = sort(shape_eco$Ecoregion),
        selected = "Greater North Sea",
        multiple = FALSE,
        width = "100%",
        options = list(
          placeholder = "Select Ecoregion(s)"
        )
      ),

      sliderInput("year_range", "Year range:", step = 1, sep = "",
        min = min(data$year), max = max(data$year),
        value = c(max(data$year)-3, max(data$year))),
      
      
      selectInput(inputId = "vessel_length", label = "Vessel Length:",
                  selected = c("VL1218", "VL1824"),
                  choices = list("0-12m" = "VL0010",
                                 "10-12m" = "VL1012",
                                 "12-18m" = "VL1218",
                                 "18-24m" = "VL1824",
                                 "24-40m" = "VL2440",
                                 "40m+" = "VL40XX",
                                 "Not known" = "NK"
                                 ),
                  
                  multiple = TRUE),
      
      
      selectInput(inputId = "gear_type", label = "Gear type:",
        selected = c("OTB", "OTM"),
        choices = sort(unique(data$gear_type)),
        multiple = TRUE),
      
      sliderInput("mesh_range", "Mesh range [mm]:", step = 10, sep = "",
        min = 0, max = 250,
        value = c(0, 250)),
      
      selectInput(inputId = "species", label = "Species:", 
        selected = c("COD", "HAD", "POK", "WHG"),
        choices = sort(unique(data$species)),
        multiple = TRUE),
      
      
      selectInput(inputId = "pal", label = "Palette:", 
        selected = "spectral",
        choices = c("spectral", "paired", "cols25", 
          "set1", "set2", "set3", 
          "alphabet", "alphabet2", "polychrome", "glasbey"),
        multiple = FALSE),
      
      checkboxInput(inputId = "sc", label = "Scaled landings", value = TRUE),
      
      # downloadLink("downloadMap", "Download Map")
      downloadButton("downloadMap", "Download Map")
      
    ),

    mainPanel(width = 9,
      fluidRow(
        column(5, plotOutput("map_species", width = "100%", height = 600)),
        column(5, plotOutput("map_gear_type", width = "100%", height = 600))
      ),
      fluidRow(
        column(5, plotOutput("corr_species", width = "100%", height = 400)),
        column(5, plotOutput("corr_gear_type", width = "100%", height = 400))
      )
    )
        
    
    
    # # Main panel with map
    # mainPanel(
    #   plotOutput("map_species", height = 800, width = 600),
    #   plotOutput("corr_species", height = 400, width = 600)
    # )
    


  )
)

# Define server logic
server <- function(input, output, session) {
  map_panel_server(input, output, session)

  ## DATA PREPARATION ----  
  # color palette look-up ----
  lutPal <- rbind(
    data.frame(pal = "spectral", fun = "brewer.spectral"),
    data.frame(pal = "paired", fun = "brewer.paired"),
    data.frame(pal = "cols25", fun = "cols25"),
    data.frame(pal = "set1", fun = "brewer.set1"),
    data.frame(pal = "set2", fun = "brewer.set2"),
    data.frame(pal = "set3", fun = "brewer.set3"),
    data.frame(pal = "alphabet", fun = "alphabet"),
    data.frame(pal = "alphabet2", fun = "alphabet2"),
    data.frame(pal = "polychrome", fun = "polychrome"),
    data.frame(pal = "glasbey", fun = "glasbey")
  )
  
  # filter data ----
  filtered_data <- reactive({
    dfsub <- subset(data, species %in% input$species & 
      gear_type %in% input$gear_type &
      vessel_length %in% input$vessel_length &  
      mesh_size_min >= input$mesh_range[1] &
      mesh_size_max <= input$mesh_range[2] &
      year >= input$year_range[1] &
      year <= input$year_range[2])
    
    dfsub <- as.data.table(dfsub)
    dfsub <- dfsub[, .(landings = sum(totwghtlandg, na.rm = TRUE)), 
        by = .(icesname, gear_type, year, quarter, species)]
    dfsub[, landingsSum := sum(landings), by = list(year, species)]
    dfsub[, landingsPerc := landings/landingsSum]
    dfsub
  })
  
  # species spatial aggregation ----
  filtered_data_species <- reactive({
    
    dfsub <- filtered_data()
    
    lutCol <- data.frame(species = sort(unique(data$species)))
    lutCol$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)],
      args = list(n = length(lutCol$species)))
    
    # aggregate
    agg <- dfsub[, .(landings = sum(landings, na.rm = TRUE)), 
      by = .(icesname, species)]
    agg[, landingsSum := sum(landings), by = .(species)]
    agg[, landingsPerc := landings/landingsSum]

    
    # agg1 <- aggregate(totwghtlandg ~ icesname + species, data = dfsub, FUN = sum, na.rm = T)
    # names(agg1)[3] <- "landings"
    # agg1 <- subset(agg1, landings > 0) # remove zero landings
    # agg2 <- aggregate(landings ~ icesname, data = agg1, FUN = sum, na.rm = T)
    # names(agg2)[2] <- "sumLandings"
    # 
    # agg <- merge(agg1, agg2, all.x = T)
    # agg$percLandings <- agg$landings / agg$sumLandings
    
    
    agg$sc <- sqrt(agg$landingsSum/max(agg$landingsSum, na.rm = T))
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutCol$col[match(agg$species, lutCol$species)]

    agg
  })
  
  # gear type spatial aggregation ----
  filtered_data_gear_type <- reactive({
    
    dfsub <- filtered_data()
    
    lutCol <- data.frame(gear_type = sort(unique(data$gear_type)))
    lutCol$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)],
      args = list(n = length(lutCol$gear_type)))
    
    # aggregate
    agg <- dfsub[, .(landings = sum(landings, na.rm = TRUE)), 
      by = .(icesname, gear_type)]
    agg[, landingsSum := sum(landings), by = .(gear_type)]
    agg[, landingsPerc := landings/landingsSum]
    
    # agg1 <- aggregate(totwghtlandg ~ icesname + gear_type, data = dfsub, FUN = sum, na.rm = T)
    # names(agg1)[3] <- "landings"
    # agg1 <- subset(agg1, landings > 0) # remove zero landings
    # agg2 <- aggregate(landings ~ icesname, data = agg1, FUN = sum, na.rm = T)
    # names(agg2)[2] <- "sumLandings"
    # 
    # agg <- merge(agg1, agg2, all.x = T)
    # agg$percLandings <- agg$landings / agg$sumLandings
    
    agg$sc <- sqrt(agg$landingsSum/max(agg$landingsSum, na.rm = T))
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutCol$col[match(agg$gear_type, lutCol$gear_type)]

    agg
  })
  
  ## PLOTS ----  
  # map species ----
  plot_map_species <- reactive({

    # op <- par(cex = 1.5, mar = c(2,2,1,1))

    data2 <- filtered_data_species()
    
    op <- par(cex = 1.5)
    plot(1, xlim = c(-6,15), ylim = c(51,62), 
      t = "n", asp = 2, xlab = "", ylab = "")
    # map("world", xlim = range(agg$lon), ylim = range(agg$lat))
    urect <- unique(data2$icesname)
    for(i in seq(urect)){
      aggsub <- subset(data2, icesname == urect[i])
      sc <- ifelse(input$sc, 1, aggsub$sc[1])

      excl <- which(aggsub$landingsPerc==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$landingsPerc==0)]
      if(nrow(aggsub)>0){
        barplot2D(z = aggsub$landingsPerc, x = aggsub$lon[1], y = aggsub$lat[1],
          width = 1*sc, height = 0.5*sc,
          colour = aggsub$col, border = NA,
          lwd.frame = 0.25, col.frame = "black")
      }
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uSppCol <- unique(data2[,c("species", "col")])
    uSppCol <- uSppCol[order(uSppCol$species),]
    legend("topright", legend = uSppCol$species, col = uSppCol$col, fill = uSppCol$col)
    par(op)

    recordPlot()
    
  })
  
  # map gear type ----
  plot_map_gear_type <- reactive({
    

    data2 <- filtered_data_gear_type()
    
    op <- par(cex = 1.5)
    plot(1, xlim = c(-6,15), ylim = c(51,62), 
      t = "n", asp = 2, xlab = "", ylab = "")
    # map("world", xlim = range(agg$lon), ylim = range(agg$lat))
    urect <- unique(data2$icesname)
    for(i in seq(urect)){
      aggsub <- subset(data2, icesname == urect[i])
      sc <- ifelse(input$sc, 1, aggsub$sc[1])

      excl <- which(aggsub$landingsPerc==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$landingsPerc==0)]
      if(nrow(aggsub)>0){
        barplot2D(z = aggsub$landingsPerc, x = aggsub$lon[1], y = aggsub$lat[1],
          width = 1*sc, height = 0.5*sc,
          colour = aggsub$col, border = NA,
          lwd.frame = 0.25, col.frame = "black")
      }
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uGearCol <- unique(data2[,c("gear_type", "col")])
    uGearCol <- uGearCol[order(uGearCol$gear_type),]
    legend("topright", legend = uGearCol$gear_type, col = uSppCol$col, fill = uGearCol$col)
    par(op)

    recordPlot()
    
  })
  
  # correlation species ----
  plot_corr_species <- reactive({
   
    # data2 <- filtered_data_species()
    # data3 <- dcast(data = data2, formula = icesname ~ species, 
    #   value.var = "landings", fun.aggregate = sum, na.rm = TRUE)
    # rownames(data3) <- data3$icesname
    # data3 <- data3[,-1]
    # # corrTab <- cor(as.matrix(data3[,-1]))
    # 
    # op <- par(cex = 1)
    # # imageDimnames(round(corrTab,2), col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1))
    # # plotCor(data3, log = FALSE)
    # # mat <- round(cor(data3, use = "pairwise.complete.obs", method = "pearson"), 2)
    # # imageCor(mat)
    # mat <- cor(log(data3+1), use = "pairwise.complete.obs", method = "pearson")
    # heatmap(mat, col = pals::ocean.balance(21), zlim = c(-1,1), symm = T, margins = c(5,5))
    # 
    # par(op)
    
    dfsub <- filtered_data()
    
    # reshape data using dcast from data.table
    dat <- dcast(dfsub, icesname + gear_type + year + quarter ~ species, 
             value.var = "landingsPerc", fill = 0)

    # calculate distance matrix among species using base R dist
    M <- as.matrix(dat[, -c(1:4)])
    D <- vegan::vegdist(t(M), method = "bray")

    # plot distance matrix as heatmap
    # COL <- pals::brewer.spectral(21)
    COL <- grey.colors(21, gamma = 0.2, start = 0.1, end = 0.9)

    heatmap(as.matrix(D), col = COL, zlim = c(0, 1), symm = TRUE, margins = c(3, 4))
    
    recordPlot()
    
  })
  
  # correlation gear type ----
  plot_corr_gear_type <- reactive({
   
    # data2 <- filtered_data_gear_type()
    # data3 <- dcast(data = data2, formula = icesname ~ gear_type, 
    #   value.var = "landings", fun.aggregate = sum, na.rm = TRUE)
    # rownames(data3) <- data3$icesname
    # data3 <- data3[,-1]
    # # corrTab <- cor(as.matrix(data3[,-1]))
    # 
    # op <- par(cex = 1)
    # # imageDimnames(round(corrTab,2), col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1))
    # # plotCor(data3, log = FALSE)
    # # mat <- round(cor(data3, use = "pairwise.complete.obs", method = "pearson"), 2)
    # # imageCor(mat)
    # mat <- cor(log(data3+1), use = "pairwise.complete.obs", method = "pearson")
    # heatmap(mat, col = pals::ocean.balance(21), zlim = c(-1,1), symm = T, margins = c(5,5))
    # 
    # par(op)
    dfsub <- filtered_data()
    
    # reshape data using dcast from data.table
    dat <- dcast(dfsub, icesname + species + year + quarter ~ gear_type, 
             value.var = "landingsPerc", fill = 0)

    # calculate distance matrix among species using base R dist
    M <- as.matrix(dat[, -c(1:4)])
    D <- vegan::vegdist(t(M), method = "bray")

    # plot distance matrix as heatmap
    # COL <- pals::brewer.spectral(21)
    COL <- grey.colors(21, gamma = 0.2, start = 0.1, end = 0.9)

    heatmap(as.matrix(D), col = COL, zlim = c(0, 1), symm = TRUE, margins = c(3, 4))
    
    recordPlot()
    
  })

  
  
  # Render map(s)
  output$map_species <- renderPlot({
    replayPlot(req(plot_map_species()))
    # plot_map()
  })
  
  output$map_gear_type <- renderPlot({
    replayPlot(req(plot_map_gear_type()))
    # plot_map()
  })
  
  # Render corr plot
  output$corr_species <- renderPlot({
    # replayPlot(req(plot_map()))
    replayPlot(req(plot_corr_species()))
  })
  
  output$corr_gear_type <- renderPlot({
    # replayPlot(req(plot_map()))
    replayPlot(req(plot_corr_gear_type()))
  })
  
  
  
  # output$corr <- renderPlot({
  #   replayPlot(req(plot_corr()))
  # }, height = 400, width = 400)
  
  output$downloadMap <- downloadHandler(
    filename = function(){"output.png"},
    content = function(file){
      png(file, height = 800, width = 650)
        replayPlot(plot_map_species())
      dev.off()
    }
  )

}

# Run the app
shinyApp(ui, server)
