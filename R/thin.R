thin <- function(x, y, tolerance = 1e-4, lock = NULL) .Call("thin", x, y, tolerance, lock, 1L)
