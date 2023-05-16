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
            width = 1 * sc
        )
    basemap
}