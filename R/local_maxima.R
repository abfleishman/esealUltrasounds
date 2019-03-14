#' Find the Idecies of Local Maximum
#'
#' Find the local maxima (peaks) in a time series
#'
#' @param x a numeric vector (timeseries)
#' @return a vector of indecies of the local maxima
#' @export

local_maxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
