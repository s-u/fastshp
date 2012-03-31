\name{thin}
\alias{thin}
\title{
  Thin out polyline/polygon by removing unneeded points.
}
\description{
  \code{thin} thins out a polyline/polygon by removing points that
  are deepemed to have no visual effect under the given tolerance. The
  algorithm used here is very simple and fast: it performs a liner
  scan through all points and for each convex point it measures the
  distance of the point from a line connecting last unthinned point
  and the subsequent point. If this distance is below tolerance it is
  removed. Note that the x, y space must be Euclidean so coordinates
  may need to be transformed accordingly (i.e. don't use uncorrected
  lat/lon!)
}
\usage{
thin(x, y, tolerance = 1e-04, lock = NULL)
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
}
%\details{
%}
\value{
  Logical vector of the same length as the number of points with
  \code{TRUE} for points that wre kept and \code{FALSE} for removed
  points.
}
%\references{
%}
\author{
Simon Urbanek
}
%\examples{
%}
\keyword{manip}
