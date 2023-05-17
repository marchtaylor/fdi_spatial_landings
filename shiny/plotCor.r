

plotCor <- function(dat, log = TRUE, method = "spearman", ...){
  dat <- as.matrix(dat)
  if(log){ 
    dat <- log(dat)
    dat[is.infinite(dat)] <- NaN
  }

  # psych::pairs.panels(dat,
  #   scale = FALSE,
  #   density = TRUE, 
  #   ellipses = F,    
  #   method = method,
  #   pch = 21,           
  #   lm = FALSE, 
  #   cor = TRUE,        
  #   hist.col = "grey90",      
  #   stars = TRUE, 
  #   ci = FALSE, 
  #   ...)
  
  COR <- cor(dat, use = "pairwise.complete.obs", method = "pearson")
  # heatmap(COR, col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1))
  imageDimnames(round(COR,2), col = colorRampPalette(c(2,"white", 4))(21), zlim = c(-1,1))
  
}
