matrixPoly <- function (x, y, z, n = NULL){
  if (missing(z)) 
    stop("Must define matrix 'z'")
  if (missing(n)) 
    n = seq(z)
  if (missing(x)) 
    x <- seq(0, 1, length.out = dim(z)[1])
  if (missing(y)) 
    y <- seq(0, 1, length.out = dim(z)[2])
  poly <- vector(mode = "list", length(n))
  for (i in seq(length(n))) {
    ROW <- row(z)[n[i]]
    COL <- col(z)[n[i]]
    dist.left <- (x[ROW] - x[ROW - 1])/2
    dist.right <- (x[ROW + 1] - x[ROW])/2
    if (ROW == 1) 
      dist.left <- dist.right
    if (ROW == dim(z)[1]) 
      dist.right <- dist.left
    dist.down <- (y[COL] - y[COL - 1])/2
    dist.up <- (y[COL + 1] - y[COL])/2
    if (COL == 1) 
      dist.down <- dist.up
    if (COL == dim(z)[2]) 
      dist.up <- dist.down
    xs <- c(x[ROW] - dist.left, x[ROW] - dist.left, x[ROW] + 
      dist.right, x[ROW] + dist.right, x[ROW] - dist.left)
    ys <- c(y[COL] - dist.down, y[COL] + dist.up, y[COL] + 
      dist.up, y[COL] - dist.down, y[COL] - dist.down)
    poly[[i]] <- data.frame(x = xs, y = ys)
  }
  return(poly)
}
