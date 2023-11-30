
imageDistClust <- function(D, 
  col = pals::magma(21), zlim = range(D), cex.diag = NULL,
  addDendro = TRUE, hclustMethod = "ward.D2",
  addScale = TRUE, labScale = "", atScale = c(0, 0.5, 0.95, 1),
  cellBorder = 1, cellLty = 1, cellLwd = 1,
  lwdDendro = 1,
  ...){
  
  z <- as.matrix(D)
  
  if(is.null(dimnames(z))){
    dimnames(z) <- list(seq(nrow(z)), seq(ncol(z)))
  }
  if(is.null(dimnames(z)[[1]])){
    dimnames(z)[[1]] <- seq(nrow(z))
  }
  if(is.null(dimnames(z)[[2]])){
    dimnames(z)[[2]] <- seq(ncol(z))
  }
  
  x <- seq(dimnames(z)[[1]])
  y <- seq(dimnames(z)[[2]])

  # clustering
  hc <- hclust(D, method = hclustMethod)
  hcd <- as.dendrogram(hc, check = F)
  
  # re-arrange data
  z <- z[hc$order, hc$order]
  labs <- hc$labels[hc$order]
  # txt <- data.frame(x = x, y = y, labels = labs)
  idx <- which(lower.tri(z))
  ztri <- z
  ztri[!lower.tri(z)] <- NaN
  if(is.null(zlim)) zlim <- range(ztri, na.rm = TRUE)

  # image
  image(x = x, y = y, z = ztri, zlim = zlim, col = col,
    axes = FALSE, xlab = "", ylab = "")
  

  polys <- sinkr::matrixPoly(x, y, z = ztri, n = idx)
  COL <- sinkr::val2col(z = ztri[idx], col = col, zlim = zlim)
  for(i in seq(polys)){
    polygon(polys[[i]], col = COL[i], border = cellBorder, lty = cellLty, lwd = cellLwd)
  }
  
  # diagonal labels
  # diagonal text size adjustment
  PAR <- par()
  cxy <- c(strwidth("O"), strheight("O"))
  diagDist <- sqrt(diff(PAR$usr[1:2])^2 + diff(PAR$usr[3:4])^2)
  cxy.wanted <- diagDist / dim(z)[2] * 0.5
  if(is.null(cex.diag)){cex.diag <- min(PAR$cex, cxy.wanted/cxy[2])}
  text(x = x, y = y, labels = labs, 
    col = 1, font = 1, xpd = TRUE, 
    srt = -45, adj = c(1,0.5), cex = cex.diag)
  par(cex = PAR$cex)
  
  # dendrogram
  if(addDendro){
    embedPlot(expr = {
      plot(hcd, type = "rectangle", 
        xlim = PAR$usr[1:2], xaxs = "i", leaflab = "none", axes = F, 
        ylim = c(max(hc$height), 0), edgePar = list(lwd = lwdDendro))
    }, at = c(0, 1, -PAR$mai[1]*0.95/PAR$pin[2], 0))
  }
  
  # scale
  if(addScale){
    embedPlot(expr = {
      imageScale(z = ztri, 
        zlim = zlim, col = col, add.axis = FALSE)
      axis(side = 1)
      mtext(labScale, side = 1, line = PAR$mgp[1], cex = PAR$cex.lab)
    }, at = atScale)    
  }

}

