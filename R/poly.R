.extract.polygons <- function(shp) {
  if (is.matrix(shp)) {
    if (dim(shp)[2L] != 2L) stop("invalid matrix - must have two columns")
    shp <- list(x=shp[,1], y=shp[,2])
  }
  l <- list(x=shp$x, y=shp$y, part=shp[["part"]])
  if (is.null(l$x) && length(shp) == 2L) { l$x <- shp[[1L]]; l$y <- shp[[2L]] }
  if (is.null(l$x) || is.null(l$y)) stop("missing coordinate vectors - the list must have elements x and y")
  if (is.null(l$part) && length(shp$parts) > 1L) {
    ## NA polygon encoding doesn't need part, but the gapless does
    if (!is.na(l$x[shp$parts[2L] + 1L]))
      l$part <- rep(seq.int(length(shp$parts)), diff(c(shp$parts, length(l$x))))
  }
  l
}

polygons.op <- function(subject, clip, op=c("intersection", "union", "difference", "xor")) {
  op <- match(match.arg(op), c("intersection", "union", "difference", "xor")) - 1L
  p1 <- .extract.polygons(subject)
  p2 <- .extract.polygons(clip)
  .Call(C_poly_op, p1$part, p1$x, p1$y, p2$part, p2$x, p2$y)
}

clip.shp <- function(shp, xlim, ylim, drop=FALSE) {
  xlim <- range(xlim)
  ylim <- range(ylim)
  ## FIXME: this is inconsistent with all other results
  if (diff(xlim) == 0 || diff(ylim) == 0) return(NULL)
  if (is.data.frame(shp)) shp <- table2list.shp(shp, "list")
  clip.x <- xlim[c(1L,2L,2L,1L)]
  clip.y <- ylim[c(1L,1L,2L,2L)]
  s <- lapply(shp, function(o) {
    if (length(o$box) && # pre-check using the bounding box if present
        (o$box[1L] > xlim[2L] || o$box[3L] < xlim[1L] || o$box[2L] > ylim[2L] || o$box[4L] < ylim[1L])) { 
      l <- list()
    } else {
      l <- .extract.polygons(o)
      l <- .Call(C_poly_op, l$part, l$x, l$y, NULL, clip.x, clip.y)
    }
    if (!length(l$x)) {
      if (drop) NULL else list(id=o$id, type=o$type, box=numeric(0), parts=integer(0), x=numeric(0), y=numeric(0))
    } else list(id=o$id, type=o$type, box=c(range(l$x),range(l$y))[c(1L,3L,2L,4L)],
                parts=c(0L, which(l$part[-1L] != l$part[-length(l$part)])), x=l$x, y=l$y)
  })
  if (!is.null(names(shp))) names(s) <- names(shp)
  if (drop) s <- s[!unlist(lapply(s, is.null))]
  class(s) <- "shp"
  s
}

bbox.filter <- function(shp, xlim, ylim, drop=FALSE) {
  xlim <- range(xlim)
  ylim <- range(ylim)
  if (is.data.frame(shp)) {
    b <- .Call(C_bboxes, shp$id, shp$x, shp$y)
    outside <- sapply(seq.int(length(b$ids)),
           function(i) {
             box <- b$boxes[,i]
             (box[1L] > xlim[2L] || box[3L] < xlim[1L] || box[2L] > ylim[2L] || box[4L] < ylim[1L])
           })
    return(shp[shp$id %in% b$ids[!outside],])
  }
  if (!inherits(shp, "shp")) stop("shp must be of the class \"shp\" or a data frame with x, y, id");
  s <- lapply(shp, function(o) {
    if (o$box[1L] > xlim[2L] || o$box[3L] < xlim[1L] || o$box[2L] > ylim[2L] || o$box[4L] < ylim[1L]) {
      if (drop) NULL else list(id=o$id, type=o$type, box=numeric(0), parts=integer(0), x=numeric(0), y=numeric(0))
    } else o 
  })
  if (!is.null(names(shp))) names(s) <- names(shp)
  if (drop) s <- s[!unlist(lapply(s, is.null))]
  if (class(shp) != class(s)) class(s) <- class(shp)
  s
}
