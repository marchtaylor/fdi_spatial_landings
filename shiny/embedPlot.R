embedPlot <- function(expr = expression({
  plot(1, t = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, font = 3, labels = "embedPlot error:\nexpression\nnot defined")
}), at = c(0.5, 0.95, 0.6, 0.95)){
  space_convert <- function(vec1, vec2) {
    vec1[1:2] <- vec1[1:2] * diff(vec2)[1] + vec2[1]
    vec1[3:4] <- vec1[3:4] * diff(vec2)[3] + vec2[3]
    vec1
  }
  plt <- par("plt")
  plt_space <- space_convert(at, plt)
  par(plt = plt_space, new = TRUE)
  eval(expr = expr)
  par(plt = plt)
}