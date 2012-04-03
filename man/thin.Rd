\name{thin}
\alias{thin}
\title{
  Thin out polyline/polygon by removing unneeded points.
}
\description{
  \code{thin} thins out a polyline/polygon by removing points that
  are deemed to have no visual effect under the given tolerance.
}
\usage{
thin(x, y, tolerance = 1e-4, lock = NULL, method = 2L)
}
\arguments{
  \item{x}{x coordinates of the points}
  \item{y}{y coordinates of the points}
  \item{tolerance}{maximum allowable distance for a point to be
    removed}
  \item{lock}{defines points that cannot be removed. Can be
  \code{NULL} (any point can be removed), a logical vector of the same
  length as the number of points or a numeric vector specifying the
  indices of points that will cannot be removed.}
  \item{method}{Must be one of \code{1L} - fast, linear method, but
    guarantees only \code{n * tolerance} accuracy where \code{n} is the
    number of subsequently removed points. \code{2L} - slower
    (\code{O(n^2)}),more conservative method that guarantees
    \code{tolerance} accuracy even with increasing \code{n}.}
}
\details{
  The default algorithm used here is very simple and fast: it performs a
  linear scan through all points and for each convex point it measures
  the distance of the point from a line connecting last unthinned point
  and the subsequent point. If this distance is below tolerance it is
  removed. Note that the x, y space must be Euclidean so coordinates
  may need to be transformed accordingly (i.e. typically you don't want
  to use uncorrected lat/lon!). This fast algorithm guarantees only
  \code{n * tolerance} accuracy with \code{n} being the number of
  subsequently removed points. The extra error will be more noticeable
  for subsequent slowly drifting points.

  The alternative algorithm (\code{method = 2L}) additionally checks
  whether any of the previously removed points would be out of tolerance
  as well - this adds complexity (it is quardatic in the number of
  removed points), but guarantees that the result is never further than
  \code{tolerance} away from the original shape.

  The input \code{x}, \code{y} can contain multiple segments separated
  by \code{NA} (R polygon format). Segments are always assumed to be a
  loop (you can still use \code{keep} to force both ends to be
  non-removable).
}
\value{
  Logical vector of the same length as the number of points with
  \code{TRUE} for points that are kept and \code{FALSE} for removed
  points.
}
%\references{
%}
\author{
Simon Urbanek
}
\examples{
  # load 2010 Census TIGER/Line(TM) state data (if included)
  shp <- system.file("shp","tl_2010_us_state10.shp.xz",package="fastshp")
  if (nzchar(shp)) {
    s <- read.shp(xzfile(shp, "rb"), "pol")
    # thin on a cylindrical projection (around ca. 37 deg lat)
    t <- lapply(s, function(o) thin(o$x / 1.25, o$y, 1e-3))
    par(mar = rep(0, 4))
    plot(c(-125,-67), c(25, 49.4), asp=1.25, ty='n', axes=FALSE)
    for (i in seq.int(s))
       polygon(s[[i]]$x[t[[i]]], s[[i]]$y[t[[i]]], col="#eeeeee")
    cat(" reduction: ", 100 - sum(sapply(t, sum)) / sum(sapply(t, length)) * 100, "\%\n", sep='')
    # use the more conservative algorithm
    t <- lapply(s, function(o) thin(o$x / 1.25, o$y, 1e-3, method = 2L))
    cat(" reduction: ", 100 - sum(sapply(t, sum)) / sum(sapply(t, length)) * 100, "\%\n", sep='')
  }
}
\keyword{manip}
