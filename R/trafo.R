.expand <- function(x, part) .Call(C_expand_vec, x, part)

table2list.shp <- function(shp, type=c("list", "polygons")) {
  if (any(is.na(type <- match(match.arg(type), c("list", "polygons"))))) stop("invalid type")
  expand <- type == 2L
  s <- split(shp, shp$id)
  s <- lapply(s, function(o)
              list(id=o$id[1L], type=o$type[1L], box=c(range(o$x), range(o$y))[c(1L,3L,2L,4L)],
                   parts=c(0L, which(o$part[-1L] != o$part[-length(o$part)])),
                   x=if (expand) .expand(o$x, o$part) else o$x,
                   y=if (expand) .expand(o$y, o$part) else o$y))
}

list2table.shp <- function(shp) {
  id <- sapply(shp, function(o) o$id)
  ty <- sapply(shp, function(o) o$type)
  n  <- sapply(shp, function(o) sum(!is.na(o$x)))
  df <- list(id = rep(id, n), type = rep(ty, n),
             part = unlist(lapply(shp, function(o) { p <- diff(c(o$parts, sum(!is.na(o$x)))); rep(seq.int(length(p)), p) })),
             x = unlist(lapply(shp, function(o) o$x[!is.na(o$x)])), y = unlist(lapply(shp, function(o) o$y[!is.na(o$x)])))
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  class(df) <- "data.frame"
  df
}
