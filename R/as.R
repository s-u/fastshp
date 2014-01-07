as.shp <- function(x)
  if (inherits(x, "shp")) x else UseMethod("as.shp")

as.shp.list <- function(x) {
  if (length(x) < 1L) return(structure(list(), bbox=rep(NA, 4), class="shp"))
  l <- lapply(x, function(o) {
    rx <- range(o$x, na.rm=TRUE)
    ry <- range(o$y, na.rm=TRUE)
    list(id = 0L, type = 5L, box = c(rx[1L], ry[1L], rx[2L], ry[2L]),
         parts = 0L, x = o$x, y = o$y)
  })
  for (i in seq.int(l)) l[[i]]$id <- i
  rx <- range(vapply(l, function(o) o$box[c(1L, 3L)], c(1,1)), na.rm=TRUE)
  ry <- range(vapply(l, function(o) o$box[c(2L, 4L)], c(1,1)), na.rm=TRUE)
  attr(l, "bbox") <- c(rx[1L], ry[1L], rx[2L], ry[2L])
  class(l) <- "shp"
  l
}
