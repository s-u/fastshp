match.sat <- function(x, table) .Call(sat_match, x, table)

overlap.sat <- function(x1, y1, x2, y2) .Call(sat, x1, y1, x2, y2)
