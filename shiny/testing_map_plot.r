library(mapplots)

data(landings) 
data(coast) 
xlim<-c(-15,0) 
ylim<-c(50,56) 
xyz <-make.xyz(landings$Lon,landings$Lat,landings$LiveWeight,landings$Species) 
col<-rainbow(5) 

basemap(xlim,ylim,main="Speciescompositionofgadoidlandings") 
draw.shape(coast,col="cornsilk") 
draw.barplot2D(xyz$x,xyz$y,xyz$z,width=0.8,height=0.4,col=col) 
legend("topright",legend=colnames(xyz$z),fill=col,bg="lightblue",inset=0.02) 


basemap(xlim,ylim,main="Speciescompositionofgadoidlandings") 
draw.shape(coast,col="cornsilk") 
draw.barplot2D(xyz$x,xyz$y,xyz$z,width=1,height=0.5,scale=TRUE,col=col) 
legend("topright",legend=colnames(xyz$z),fill=col,bg="lightblue",inset=0.02)







#####################################
# Title: R Leaflet custom summing marker demo


# Load packages ##################
install.packages("pacman")
require(pacman)

pacman::p_load(pacman, dplyr, leaflet, summarytools)


# Creates data ##################
data("breweries91",package="leaflet")
cat('\014') # ctrl+L
#head(breweries91, 2L)
breweries91$goodbeer<-sample(as.factor(c("terrific","marvelous","culparterretaping")),nrow(breweries91),replace=T)
names(breweries91)

# Colors
joliepalette<-c("darkviolet","orangered","lime")[1:nlevels(breweries91$goodbeer)]
getColor <- function(breweries91) {joliepalette[breweries91$goodbeer]}

# Score
jolieValue<-c(1L,2L,3L)[1:nlevels(breweries91$goodbeer)]
getScore <- function(breweries91) {jolieValue[breweries91$goodbeer]}


# iconCreateFunction Javascript
jsscript3<-paste0(
  "function(cluster) {
   const groups= [",paste("'",levels(breweries91$goodbeer),"'",sep="",collapse=","),"];
   const colors= {
     groups: [",paste("'",joliepalette,"'",sep="",collapse=","),"],
     center:'#ddd',
     text:'black'
    };
   const markers= cluster.getAllChildMarkers();
   let grandTotal = markers.reduce((a,b)=> +a + +b.options.score, 0);

   const proportions= groups.map(group => markers
                        .filter(marker => marker.options.group === group)
                         .reduce((a,b)=> +a + +b.options.score, 0) / grandTotal);

   function sum(arr, first= 0, last) {
    return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
  }
  const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
  cumulativeProportions.unshift(0);

  let width = 4 + 2*Math.sqrt(grandTotal/1.5);
  width = width > 16? 16: width;
  let radius= 10 + (width/2);
  radius += (grandTotal < 40)? grandTotal/10 : 4;

  const arcs= cumulativeProportions.map((prop, i) => { return {
    x   :  radius*Math.sin(2*Math.PI*prop),
    y   : -radius*Math.cos(2*Math.PI*prop),
    long: proportions[i-1] >.5 ? 1 : 0
   }});
 const paths= proportions.map((prop, i) => {
   if (prop === 0) return '';
   else if (prop === 1) return `<circle cx='0' cy='0' r='${radius-2}' fill-opacity='0.3' stroke-opacity fill='${colors.groups[i]}' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
   else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i+1].long} 1 ${arcs[i+1].x} ${arcs[i+1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
  });

  return new L.DivIcon({
   html: `
    <svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
      <circle cx='0' cy='0' r='15' stroke='none' fill='${colors.center}' />
      ${paths.join('')}
      <text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='16'>${grandTotal}</text>
    </svg>
    `,
   className: 'marker-cluster'
   });
}")

# gather stats for legend
myStat <- freq(breweries91$goodbeer, report.nas = FALSE, cumul = FALSE)
s1 <- paste("3 - terrific(",myStat[3,1],")")
s2 <- paste("2 - marvelous(",myStat[2,1],")")
s3 <- paste("1 - culparterretaping(", myStat[1,1],")")


# generates the map.
leaflet() %>%
  addTiles() %>%
  addLegend("topright", 
            colors = c("lime", "orangered", "darkviolet"),
            labels = c(s1, s2,s3),
            title = "Beer goodness:",
            opacity = 1) %>%
  addMarkers(data = breweries91,
             group = ~goodbeer,
             options = markerOptions(score = getScore(breweries91)),
             clusterOptions = markerClusterOptions(
               singleMarkerMode = TRUE,
               iconCreateFunction = JS(jsscript3)
             )
  )


# print stats
freq(breweries91$goodbeer, report.nas = FALSE, cumul = FALSE)
print(paste("Grand Score: ", myStat[1,1]*1 + myStat[2,1]*2 + myStat[3,1]*3))


##################################################################
library(leaflet.minicharts)
data("eco2mix")
head(eco2mix)
library(dplyr)

prod2016 <- eco2mix %>%
  mutate(
    renewable = bioenergy + solar + wind + hydraulic,
    non_renewable = total - bioenergy - solar - wind - hydraulic
  ) %>%
  filter(grepl("2016", month) & area != "France") %>%
  select(-month) %>%
  group_by(area, lat, lng) %>%
  summarise_all(sum) %>%
  ungroup()

head(prod2016)

library(leaflet)

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(width = "100%", height = "800px") %>%
  addTiles(tilesURL)

colors <- c("#4fc13c", "#c13cba", "#c1b83c", "#c18c3c","#c14c3c")

basemap %>%
  addMinicharts(
    xyz$x, xyz$y,
    type = "pie",
    chartdata = xyz$z, 
    colorPalette = colors, 
    width = 15
  )



barplot2D(z = aggsub$percLandings, x = aggsub$lon[1], y = aggsub$lat[1],
        width = 1*sc, height = 0.5*sc,
        colour = aggsub$col, border = NA,
        lwd.frame = 0.25, col.frame = "black")
    }

basemap %>%
  addMinicharts(
    aggsub$lon[1], aggsub$lat[1],
    type = "pie",
    chartdata = aggsub$percLandings, 
    colorPalette = aggsub$col, 
    width = 1*sc
  )


  #######
source("shiny/imageDimnames.R")
source("shiny/utilities_load_ecoregion_shp.r")
source("shiny/utilities_ecoregion_mapping.r")
source("shiny/interactive_map_landings.r")


# Load data
load("shiny/data.Rdata")
head(data)
mesh_size_split <- strsplit(data$mesh_size_range, "D")
mesh_sizes <- suppressWarnings(as.numeric(do.call("c", mesh_size_split)))
mesh_size_range <- range(unique(mesh_sizes), na.rm = TRUE)
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(length(x)<2){c(mesh_size_range)}else{x}})
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(x[2]=="XX"){c(x[1], mesh_size_range[2])}else{x}})
data$mesh_size_min <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[1]})))
data$mesh_size_max <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[2]})))

agg1 <- aggregate(totwghtlandg ~ icesname + species, data = dfsub, FUN = sum, na.rm = T)

test_data <- read.table("shiny/test_data.csv")

interactive_map_landings <- function(aggsub, sc) {
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

    basemap <- leaflet(width = "100%", height = "800px") %>%
        addTiles(tilesURL)
    basemap %>%
        addMinicharts(
            aggsub$lon, aggsub$lat,
            type = "pie",
            chartdata = aggsub$percLandings,
            colorPalette = aggsub$col,
            width = 15
        )
    basemap
}
interactive_map_landings(test_data, test_data$sc)
