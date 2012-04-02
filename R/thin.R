thin <- function(x, y, tolerance = 1e-4, lock = NULL, method = 1L) .Call("thin", x, y, tolerance, lock, method)
