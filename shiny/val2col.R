val2col <- function (z, zlim, col = hcl.colors(12, "YlOrRd", rev = TRUE), 
  breaks){
  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than color")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 
      1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(z, na.rm = TRUE)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 
      1))
  }
  CUT <- cut(z, breaks = breaks, include.lowest = TRUE)
  colorlevels <- col[match(CUT, levels(CUT))]
  return(colorlevels)
}