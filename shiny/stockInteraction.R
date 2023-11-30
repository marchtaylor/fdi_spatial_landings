
stockInteraction <- function(data, 
  valueVar = "totwghtlandg", 
  compareVar = "species",
  aggVars = c("species", "year", "vessel_length", "gear_type", "mesh_size_range", "icesname"), 
  scaleVars = c("species", "year"), 
  distMethod = "bray"
  ){
  
  if(!compareVar %in% aggVars){stop("'scaleVars' should be included in 'aggVars'")}
  if(!all(scaleVars %in% aggVars)){stop("'scaleVars' should be included in 'aggVars'")}
  # convert to data.table
  dt <- as.data.table(data)
  
  # aggregate data
  dt <- dt[, .(value = sum(get(valueVar), na.rm = TRUE)), 
    by = aggVars]
  
  # scale data
  dt <- dt[, valueFrac := value/sum(value), by = scaleVars]
  
  # reshape to matrix
  cats_incl <- aggVars[-which(aggVars == compareVar)]
  fmla <- formula(paste(paste(cats_incl, collapse = " + "),"~",  compareVar))
  df <- dcast(data = as.data.frame(dt), formula = fmla, 
    value.var = "valueFrac", fun.aggregate = sum, fill = 0)

  Y <- df[, -c(seq(cats_incl))]
  X <- df[, c(seq(cats_incl))]
  D <- vegan::vegdist(t(Y), method = distMethod)
  
  res <- list(D = D, X = X, Y = Y)
  return(res)
  
}


