merge.tiles <- function(x, y, id = rep(1L, length(x))) .Call(merge_pts, x, y, id)
