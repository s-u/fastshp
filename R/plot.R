## R's polypath is horribly inefficient because the R-level API requires breaks and the internal removes them
## so there are two unnecessary copies for each coordinate ... while we can jsut pass them directly
.onLoad <- function(...) {
  gre <- asNamespace("graphics")
  if (exists("C_path",gre))
    environment(.onLoad)$C_path <- get("C_path", gre)
  ## FIXME: polypath won't work if C_path is not found, but at least
  ## the package will load ...
}

polypath.shp <- function(so, col="#e0e0e0", border="#808080", lty=1L, ...) .External.graphics(C_path, so$x, so$y, diff(c(so$parts, length(so$x))), 1L, col, border, lty, ...)

plot.shp <- function(x, xlim, ylim, asp=1/cos(sum(ylim)/360*pi), add=FALSE, axes=FALSE, full=TRUE, hold=FALSE, col="#e0e0e0", border="#808080", ...) {
  if (is.data.frame(x)) {
    if (missing(xlim)) xlim <- range(x$x, na.rm=TRUE)
    if (missing(ylim)) ylim <- range(x$y, na.rm=TRUE)
    if (hold) { dev.hold(); on.exit(dev.flush()) }
    if (!add) {
      if (full) par(mar=rep(0,4L))
      plot(xlim, ylim, ty='n', axes=FALSE, asp=asp)
    }
    s <- split(x, x$id);
    col <- rep(col, length.out=length(s))
    border <- rep(border, length.out=length(s))
    for (i in seq.int(length(s))) {
      l <- unclass(s[[i]])
      p <- l$part; np <- length(p)
      l$parts <- if (p[1L] == p[np]) 0L else c(0L, which(p[-1L] != p[-np]))
      polypath.shp(l, col=col[i], border=border[i], ...)
    }
  } else {
    if (missing(xlim)) xlim <- range(sapply(x, function(o) c(o$box[1L], o$box[3L])))
    if (missing(ylim)) ylim <- range(sapply(x, function(o) c(o$box[2L], o$box[4L])))
    if (hold) { dev.hold(); on.exit(dev.flush()) }
    col <- rep(col, length.out=length(x))
    border <- rep(border, length.out=length(x))
    if (!add) {
      if (full) par(mar=rep(0,4L))
      plot(xlim, ylim, ty='n', axes=FALSE, asp=asp)
    }
    for (i in seq.int(length(x)))
      polypath.shp(x[[i]], col=col[i], border=border[i], ...)
  }
  invisible(NULL)
}
