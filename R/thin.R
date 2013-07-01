thin <- function(x, y, tolerance = 1e-4, lock = NULL, method = 2L, id = NULL) .Call(C_thin, x, y, tolerance, lock, method, id)

thin.shp <- function(shp, tolerance = 1e-3, max.width = 5L, all = !is.data.frame(shp)) {
  if (is.character(shp) || is.raw(shp) || inherits(shp, "connection"))
    shp <- read.shp(shp, "table")
  # FIXME: we need a better class structure to identify shp formats
  if (!is.data.frame(shp) || !identical(names(shp), c("id","type","part","x","y"))) {
    if (inherits(shp, "shp")) shp <- list2table.shp(shp)
    else stop("`shp' must be a shape list as returned by read.shp(.., format='table')")
  }
  o <- order(shp$x, shp$y)
  xp <- max(shp$part) + 2L ## we'll generate synthetic IDs that consist of both part and id
  t <- .Call(C_atag, shp$x, shp$y, shp$id * xp + shp$part, o, max.width) 
  h <- thin(shp$x, shp$y, tolerance, t$fix, id = shp$id)
  if (all) {
    shp$thin <- h[t$ref]
    shp
  } else h[t$ref]
}
