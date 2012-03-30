read.shp <- function(fn, format = c("list","pairlist","polygon","table")) {
  args <- c("pairlist","list","polygon","table")
  if (!is.numeric(format)) format <- match(match.arg(format), args) - 1L
  .Call("read_shp", fn, format)
}
