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


# figure attributes -------------------------------------------------------
figattr <- list()
figattr$png.width = 8
figattr$png.height = 9
figattr$png.unit = "in"
figattr$png.res = 400
figattr$ps = 12
figattr$lo <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
figattr$widths = c(3.5,3.5)
figattr$heights = c(4.5,3.5)
figattr$width = 800
figattr$height = (figattr$png.height * figattr$png.res) / (figattr$png.width * figattr$png.res / figattr$width)


# load function -----------------------------------------------------------
source("imageDistClust.R")
source("stockInteraction.R")
source("embedPlot.R")
source("imageScale.R")
source("val2col.R")
source("matrixPoly.R")


# Load data
# load("shiny/Data/fdi_ecoregion/Greater North Sea_data.Rdata")
load("Data/fdi_ecoregion/Greater North Sea_data.Rdata")


# input <- list(); input$ecoregion = "Greater North Sea"; input$year_range <- c(max(data$year)-3, max(data$year)); input$vessel_length <- c("VL1218", "VL1824"); input$gear_type <- c("OTB", "OTM", "SSC", "TBB"); input$species <- c("COD", "HAD", "POK", "WHG"); input$mesh_range <- c(0, 250); input$pal <- "glasbey"; input$sc <- 1

# 1. Define UI -----
ui <- fluidPage(
  # 1.1. Sidebar -----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # selectInput(inputId = "ecoregion", label = "Select Ecoregion:",
      #   selected = c("Greater North Sea"),
      #   choices = list("Azores", "Baltic Sea", "Barents Sea",
      #     "Bay of Biscay and the Iberian Coast",
      #     "Celtic Seas", "Faroes", "Greater North Sea",
      #     "Greenland Sea", "Norwegian Sea", "Oceanic Northeast Atlantic",
      #     "Western Mediterranean Sea"),
      #   multiple = FALSE),
      # actionButton("buttonEcoregion", "Update Ecoregion", icon("refresh"),
      #   class = "btn btn-primary"),
      # hr(),

      sliderInput("year_range", "Year range:", step = 1, sep = "",
        min = min(data$year), max = max(data$year),
        value = c(max(data$year)-3, max(data$year))),
      
      
      selectInput(inputId = "vessel_length", label = "Vessel Length:",
        selected = c("VL0010", "VL1218", "VL1824", "VL2440", "VL40XX"),
        choices = list(
          "0-12m" = "VL0010",
          "10-12m" = "VL1012",
          "12-18m" = "VL1218",
          "18-24m" = "VL1824",
          "24-40m" = "VL2440",
          "40m+" = "VL40XX",
          "Not known" = "NK"
        ),
        multiple = TRUE),
      
      
      selectInput(inputId = "gear_type", label = "Gear type:",
        selected = c("OTB", "OTM", "OTT", "SSC", "TBB", "GNS"),
        choices = sort(unique(data$gear_type)),
        multiple = TRUE),
      
      sliderInput(inputId = "mesh_range", label = "Mesh range [mm]:", 
        step = 10, sep = "",
        min = 0, max = 250,
        value = c(0, 250)),
      
      selectInput(inputId = "species", label = "Species:", 
        selected = c("COD", "HAD", "POK", "WHG", "PLE", "SOL", "BLL", "DAB", "LEM"),
        choices = sort(unique(data$species)),
        multiple = TRUE),
      
      selectInput(inputId = "pal", label = "Palette:", 
        selected = "glasbey",
        choices = c("spectral", "paired", "cols25", 
          "set1", "set2", "set3", 
          "alphabet", "alphabet2", "polychrome", "glasbey"),
        multiple = FALSE),
      
      checkboxInput(inputId = "sc", label = "Scaled landings", value = TRUE),
      
      actionButton("buttonPlots", "Update plots", icon("refresh"),
        class = "btn btn-primary"),
      hr(),
      downloadButton("download", "Download Image")
      
    ),
  
    # 1.2. Main panel -----
    mainPanel(
      width = 9,
      tabsetPanel(type = "tabs",
        tabPanel("Plot", 
          imageOutput("myImage")
        ),
        tabPanel("Notes",
          br(),
          p(em("The visualization uses FDI landings by various categories: 
            year, quarter, species, vessel length, gear type, mesh size, ICES 
            rectangle (among others)")),
          br(),
          p(em("Maps aggregate filtered data per ICES rectange, either by 
            species (left top panel) or gear type (right top panel). 
            When 'Scaled landings' is selected, 2D barplots fill the entire 
            spatial rectangle (i.e. emphasise on composition). When unselected
            the barplots area is relative to the rectangle 
            with the maximum aggregated landings 
            (emphasis on composition and magnitude).")),
          br(),
          p(em("Lower panels show the degree of dissimilarity (Bray-Curtis) in 
            landings patterns among species (left lower panel) and gear types 
            (right lower panel). Before computing dissimilarities, landings are
            scaled in the following way:"),
            br(),
            tags$ol(
              tags$li(em("Species comparison - 
                landings are divided by the sum of landings by year and species, 
                which accounts for differences in biomass among species.")), 
              tags$li(em("Gear type comparison - 
                landings are divided by the sum of landings by year, species and 
                gear, which accounts for differences in biomass among species 
                and fishing effort / catchability among gears."))
            ),
            br(),
            em("Hierarchical clustering dendrograms illustrate patterns of 
              dissimilarity among species / gears 
              (Ward's minimum variance method is used).")
          ),
          br(),
          style='width: 500px; height: 1000px'
        )
      )
    )


  )
)

# 2. Define server logic -----
server <- function(input, output, session) {

  # 2.1. Data preparation -----  
  # raw_data <- eventReactive(eventExpr = input$buttonEcoregion, {
  #   fname <- paste0("Data/fdi_ecoregion/", input$ecoregion, "_data.Rdata", sep = "")
  #   load(fname)
  #   data
  # })
  
  
  
  # 2.1.1. color palette look-up table -----
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
  
  # 2.1.2. filter data ----
  filtered_data <- eventReactive(eventExpr = input$buttonPlots, {
    # data <- raw_data()
    dfsub <- subset(data, species %in% input$species & 
      gear_type %in% input$gear_type &
      vessel_length %in% input$vessel_length &  
      mesh_size_min >= input$mesh_range[1] &
      mesh_size_max <= input$mesh_range[2] &
      year >= input$year_range[1] &
      year <= input$year_range[2])
    
    dfsub <- as.data.table(dfsub)
    
    # eventually allow definition of aggregation ***
    dfsub <- dfsub[, .(landings = sum(totwghtlandg, na.rm = TRUE)), 
      by = .(icesname, gear_type, year, species)]
    

    dfsub
  })
  
  # 1.3. color palette look-up tables
  make_lutCol <- eventReactive(eventExpr = input$buttonPlots, {
    # data <- raw_data()
    lutColSpp <- data.frame(species = sort(unique(data$species)))
    lutColSpp$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)],
      args = list(n = length(lutColSpp$species)))
    
    lutColGear <- data.frame(gear_type = sort(unique(data$gear_type)))
    lutColGear$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)],
      args = list(n = length(lutColGear$gear_type)))
    
    lut <- list(lutColSpp = lutColSpp, lutColGear = lutColGear)
    lut
  })
  

  
  # 2.1.3. interaction data -----
  interaction_data <- eventReactive(eventExpr = input$buttonPlots, {

    dfsub <- filtered_data()
    
    # species comparisons scale yearly totals by species to account for biomass differences
    # gear comparisons scale yearly totals by species and gear type to account for biomass and effort differences
    # dfsub[, landingsSum := sum(landings, na.rm = TRUE), by = .(year, species)]
    # dfsub[, landingsSc := landings/landingsSum]
    dfsub[, landingsScSpp := landings / sum(landings, na.rm = TRUE), by = .(year, species)]
    dfsub[, landingsScGear := landings / sum(landings, na.rm = TRUE), by = .(year, species, gear_type)]
    dfsub[is.na(landingsScSpp), landingsScSpp := 0]
    dfsub[is.na(landingsScGear), landingsScGear := 0]
    
    # species
    cats_incl <- c("icesname", "gear_type", "year")
    compareVar <- "species"
    fmla <- formula(paste(paste(cats_incl, collapse = " + "),"~",  compareVar))
    tmp <- reshape2::dcast(data = as.data.frame(dfsub), formula = fmla, 
      value.var = "landingsScSpp", fun.aggregate = sum, fill = 0)
    
    intSpp <- vegan::vegdist(x = t(tmp[,-seq(cats_incl)]), method = "bray")

    # gear
    cats_incl <- c("icesname", "species", "year")
    compareVar <- "gear_type"
    fmla <- formula(paste(paste(cats_incl, collapse = " + "),"~",  compareVar))
    tmp <- reshape2::dcast(data = as.data.frame(dfsub), formula = fmla, 
      value.var = "landingsScGear", fun.aggregate = sum, fill = 0)
    intGear <- vegan::vegdist(x = t(tmp[,-seq(cats_incl)]), method = "bray")
    
    intData <- list(species = intSpp, gear_type = intGear)

  })
  
  # 2.1.4. map data -----
  map_data <- eventReactive(eventExpr = input$buttonPlots, {

    dfsub <- filtered_data()
    lut <- make_lutCol()
    
    # aggregate by species & ICES rectangle
    lutAgg <- lut$lutColSpp
    agg <- dfsub[, .(landings = sum(landings, na.rm = TRUE)), 
      by = .(icesname, species)]
    agg[, landingsSum := sum(landings), by = .(icesname)]
    agg[, landingsFrac := landings/landingsSum]
    agg[is.na(landingsFrac), landingsFrac := 0]
    agg[, sc := sqrt(landingsSum/max(landingsSum, na.rm = T))]
    agg[is.na(sc), sc := 0]
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutAgg$col[match(agg$species, lutAgg$species)]
    aggSpp <- agg

    # aggregate by species & ICES rectangle
    lutAgg <- lut$lutColGear
    agg <- dfsub[, .(landings = sum(landings, na.rm = TRUE)), 
      by = .(icesname, gear_type)]
    agg[, landingsSum := sum(landings), by = .(icesname)]
    agg[, landingsFrac := landings/landingsSum]
    agg[is.na(landingsFrac), landingsFrac := 0]
    agg[, sc := sqrt(landingsSum/max(landingsSum, na.rm = T))]
    agg[is.na(sc), sc := 0]
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutAgg$col[match(agg$gear_type, lutAgg$gear_type)]
    aggGear <- agg
    
    mapData <- list(species = aggSpp, gear_type = aggGear)
  })
  
 
  # 2.2. Image --------------------------------------------------------------------
  image <- eventReactive(eventExpr = input$buttonPlots, {
    outfile <- tempfile(fileext = ".png")

    png(outfile, width = figattr$png.width, height = figattr$png.height, units = figattr$png.unit, res = figattr$png.res)
    op <- par(no.readonly = TRUE)
    lo <- layout(figattr$lo, 
      widths = figattr$widths, heights = figattr$heights, respect = T)
    par(cex = 1, mar = c(3,3,1,1), ps = figattr$ps)
    
    mapData <- map_data()
    
    # 2.2.1. map of species -----
    plot(1, xlim = range(mapData$species$lon, na.rm = TRUE), ylim = range(mapData$species$lat, na.rm = TRUE), 
      t = "n", asp = 2, xlab = "", ylab = "")
    urect <- unique(mapData$species$icesname)
    for(i in seq(urect)){
      aggsub <- subset(mapData$species, icesname == urect[i])
      sc <- ifelse(input$sc, 1, aggsub$sc[1])
  
      excl <- which(aggsub$landingsFrac==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$landingsFrac==0)]
      if(nrow(aggsub)>0){
        barplot2D(z = aggsub$landingsFrac, x = aggsub$lon[1], y = aggsub$lat[1],
          width = 1*sc, height = 0.5*sc,
          colour = aggsub$col, border = NA,
          lwd.frame = 0.25, col.frame = "black")
      }
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uSppCol <- unique(mapData$species[,c("species", "col")])
    uSppCol <- uSppCol[order(uSppCol$species),]
    legend("topright", legend = uSppCol$species, col = uSppCol$col, fill = uSppCol$col)
  
    # 2.2.2. map of gears -----
    plot(1, xlim = range(mapData$gear_type$lon, na.rm = TRUE), ylim = range(mapData$gear_type$lat, na.rm = TRUE), 
      t = "n", asp = 2, xlab = "", ylab = "")
    urect <- unique(mapData$gear_type$icesname)
    for(i in seq(urect)){
      aggsub <- subset(mapData$gear_type, icesname == urect[i])
      sc <- ifelse(input$sc, 1, aggsub$sc[1])
  
      excl <- which(aggsub$landingsFrac==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$landingsFrac==0)]
      if(nrow(aggsub)>0){
        barplot2D(z = aggsub$landingsFrac, x = aggsub$lon[1], y = aggsub$lat[1],
          width = 1*sc, height = 0.5*sc,
          colour = aggsub$col, border = NA,
          lwd.frame = 0.25, col.frame = "black")
      }
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uGearCol <- unique(mapData$gear_type[,c("gear_type", "col")])
    uGearCol <- uGearCol[order(uGearCol$gear_type),]
    legend("topright", legend = uGearCol$gear_type, col = uSppCol$col, fill = uGearCol$col)
    
    # 2.2.3. diss. of species  -----
    intData <- interaction_data()
    par(mar = c(5,3,1,1))
    
    imageDistClust(D = intData$species, hclustMethod = "ward.D", zlim = c(0,1), 
      col = rev(pals::brewer.greys(10)), atScale = c(0,0.5,0.95,1),
      cex.diag = NULL, cellBorder = 1, cellLwd = 1, lwdDendro = 1)

    # 2.2.4. diss. of gears  -----
    imageDistClust(D = intData$gear_type, hclustMethod = "ward.D", zlim = c(0,1), 
      col = rev(pals::brewer.greys(10)), atScale = c(0,0.5,0.95,1),
      cex.diag = NULL, cellBorder = 1, cellLwd = 1, lwdDendro = 1)
    
  
  par(op)
  dev.off()

  list(
    src = outfile,
    contentType = "image/png",
    width = figattr$width, height = figattr$height, 
    alt = "This is alternate text"
  )
  })
  
  output$myImage <- renderImage(
    {
      image()
    },
    deleteFile = FALSE
  )

  output$download <- downloadHandler(
    filename = function() {
      "generated_plot.png"
    },
    content = function(file) {
      img <- image()$src
      file.copy(img, file)
    }
  )
}

# Run the app
shinyApp(ui, server)
