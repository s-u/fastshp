read.shp <- function(where, format = c("list","pairlist","polygon","table"), close=TRUE) {
  args <- c("pairlist","list","polygon","table")
  if (!is.numeric(format)) format <- match(match.arg(format), args) - 1L
  .Call("read_shp", where, format, close)
}
