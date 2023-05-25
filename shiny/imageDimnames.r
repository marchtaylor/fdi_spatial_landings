
ac <- function(x, ...){
  as.character(x, ...)
}

matrixIndex <- function(idx = NULL, coord = NULL, dim.mat = NULL){
	if(is.null(idx) & is.null(coord) | is.null(dim.mat)){
		stop("must supply either 'idx' or 'coord', and 'dim.mat'")
	}
	if(is.null(idx) & !is.null(coord) & !is.null(dim.mat)){
		if(!is.matrix(coord)) coord <- matrix(coord, ncol = 2, byrow = TRUE)
		idx <- ((coord[,2]-1)*dim.mat[1])+coord[,1] # position
		return(idx)
	}
	if(!is.null(idx) & is.null(coord) & !is.null(dim.mat)){
    coord <- matrix(NA, nrow = length(idx), ncol=2)
    colnames(coord) <- c("row", "col")
		coord[,1] <- ((idx-1) %% dim.mat[1]) +1
		coord[,2] <- ((idx-1) %/% dim.mat[1]) +1
		return(coord)
	}
}


val2col<-function(z, zlim, col = hcl.colors(12, "YlOrRd", rev = TRUE), breaks){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than color")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 CUT <- cut(z, breaks=breaks, include.lowest = TRUE)
 colorlevels <- col[match(CUT, levels(CUT))] # assign colors to heights for each point
 return(colorlevels)
}

matrixPoly <- function(x, y, z, n=NULL){
	if(missing(z)) stop("Must define matrix 'z'")
	if(missing(n)) n=seq(z)
	if(missing(x)) x <- seq(0, 1, length.out = dim(z)[1])
	if(missing(y)) y <- seq(0, 1, length.out = dim(z)[2])
	poly <- vector(mode="list", length(n))
	for(i in seq(length(n))){
		ROW <- row(z)[n[i]] # ((n[i]-1) %% dim(z)[1]) +1
		COL <- col(z)[n[i]] # ((n[i]-1) %/% dim(z)[1]) +1

		dist.left <- (x[ROW]-x[ROW-1])/2
		dist.right <- (x[ROW+1]-x[ROW])/2
		if(ROW==1) dist.left <- dist.right
		if(ROW==dim(z)[1]) dist.right <- dist.left

		dist.down <- (y[COL]-y[COL-1])/2
		dist.up <- (y[COL+1]-y[COL])/2
		if(COL==1) dist.down <- dist.up
		if(COL==dim(z)[2]) dist.up <- dist.down
		
		xs <- c(x[ROW]-dist.left, x[ROW]-dist.left, x[ROW]+dist.right, x[ROW]+dist.right, x[ROW]-dist.left)
		ys <- c(y[COL]-dist.down, y[COL]+dist.up, y[COL]+dist.up, y[COL]-dist.down, y[COL]-dist.down)
		poly[[i]] <- data.frame(x=xs, y=ys)
	}
	return(poly)
}

  
imageCor <- function(mat, 
  xlab = NULL, ylab = NULL, xaxisSide = 1, yaxisSide = 2,
  axisLas = 1, log = FALSE,
  drawBorders = TRUE, borderCol = 1, borderLty = 1, borderLwd = 1,
  addLabels = TRUE, labels = ac(c(mat)), labelCol = "black", labelFont = 1,
  col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1),
  ...){
  
  
  if(is.null(dimnames(mat))){
    dimnames(mat) <- list(seq(nrow(mat)), seq(ncol(mat)))
  }
  if(is.null(dimnames(mat)[[1]])){
    dimnames(mat)[[1]] <- seq(nrow(mat))
  }
  if(is.null(dimnames(mat)[[2]])){
    dimnames(mat)[[2]] <- seq(ncol(mat))
  }
  
  # mat[!lower.tri(mat)] <- NaN
  x <- seq(dimnames(mat)[[1]])
  y <- seq(dimnames(mat)[[2]])
  z <- mat
  if(is.null(xlab)) xlab = names(dimnames(mat))[1]
  if(is.null(ylab)) ylab = names(dimnames(mat))[2]
  if(is.null(xlab)) xlab = ""
  if(is.null(ylab)) ylab = ""
  
  idx <- which(lower.tri(mat))
  mat[!lower.tri(mat)] <- NaN
  z <- mat
  image(x = x, y = y, z = z, axes = FALSE, xlab = xlab, ylab = ylab, col = col, zlim = zlim)#, ...)
  
  polys <- matrixPoly(x, y, z=z, n=idx)
  COL <- val2col(z = z[idx], col = col, zlim = zlim)
  for(i in seq(polys)){
    polygon(polys[[i]], col=COL[i], border=1)
  }
  
  # matrix value labels
  txt <- as.data.frame(matrixIndex(idx = idx, dim.mat = dim(mat)))
  txt$val <- c(z[idx])
  txt$x <- x[txt$row]
  txt$y <- y[txt$col]
  txt$labels <- labels[idx]
  text(x = txt$x, y = txt$y, labels = txt$labels, 
    col = labelCol, font = labelFont)
  
  # dimension labels
  txt <- data.frame(x = x, y = y-0.5, labels = dimnames(mat)[[1]])
  text(x = txt$x, y = txt$y, labels = txt$labels, 
    col = 1, font = labelFont, pos = 3)
  
  
  # axis(side = xaxisSide, at = x, labels = dimnames(mat)[[1]], las = axisLas)
  # axis(side = yaxisSide, at = y, labels = dimnames(mat)[[2]], las = axisLas)
  
  # if(addLabels){
  #   txt <- expand.grid(x = x, y = y)
  #   txt$val <- c(z)
  #   text(x = txt$x, y = txt$y, labels = labels, 
  #     col = labelCol, font = labelFont)
  # }
}



imageDimnames <- function(mat, 
  xlab = NULL, ylab = NULL, xaxisSide = 1, yaxisSide = 2,
  axisLas = 1,
  drawBorders = TRUE, borderCol = 1, borderLty = 1, borderLwd = 1,
  addLabels = TRUE, labels = sinkr::ac(c(mat)), labelCol = "black", labelFont = 1,
  ...){
  if(is.null(dimnames(mat))){
    dimnames(mat) <- list(seq(nrow(mat)), seq(ncol(mat)))
  }
  if(is.null(dimnames(mat)[[1]])){
    dimnames(mat)[[1]] <- seq(nrow(mat))
  }
  if(is.null(dimnames(mat)[[2]])){
    dimnames(mat)[[2]] <- seq(ncol(mat))
  }
  
  x <- seq(dimnames(mat)[[1]])
  y <- seq(dimnames(mat)[[2]])
  z <- mat
  if(is.null(xlab)) xlab = names(dimnames(mat))[1]
  if(is.null(ylab)) ylab = names(dimnames(mat))[2]
  if(is.null(xlab)) xlab = ""
  if(is.null(ylab)) ylab = ""
  
  image(x = x, y = y, z = z, axes = FALSE, xlab = xlab, ylab = ylab, ...)
  if(drawBorders){
    abline(h = seq(length(y))-0.5, 
      col = borderCol, lty = borderLty, lwd = borderLwd)
    abline(v = seq(length(x))-0.5, 
      col = borderCol, lty = borderLty, lwd = borderLwd)
    box(col = borderCol, lty = borderLty, lwd = borderLwd)
  }
  
  axis(side = xaxisSide, at = x, labels = dimnames(mat)[[1]], las = axisLas)
  axis(side = yaxisSide, at = y, labels = dimnames(mat)[[2]], las = axisLas)
  
  if(addLabels){
    txt <- expand.grid(x = x, y = y)
    txt$val <- c(z)
    text(x = txt$x, y = txt$y, labels = labels, 
      col = labelCol, font = labelFont)
  }
}
