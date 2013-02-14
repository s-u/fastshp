\name{merge.tiles}
\alias{merge.tiles}
\title{
  Merges adjacent tiles (polygons) where possible
}
\description{
  \code{merges.tiles} merges adjacent polygons
}
\usage{
merge.tiles(x, y, id = rep(1L, length(x)))
}
\arguments{
  \item{x}{x coordinates}
  \item{y}{y coordinates}
  \item{id}{identifiers defining tiles - all contiguous coordinates with
    the same \code{id} define one polygon}
}
\details{
  Each tile is a polygon defined by points with the \code{id} of the
  tile. The points for the same \code{id} must be contiguous.
  No tiles are allowed to overlap. Shared edges must be defined by
  points common to both tiles. The orientation must be consistent since
  only edges with opposite orientation will be merged.

  The algorithm used to merge finds all common points in the data, for
  each common point checks whether edges from that point are shared
  and any such edges are marked for deletion.

  The resulting set of \code{id}s will be a subset of the initial
  \code{id}s. Merged tiles recevied the \code{id} that comes earlier in
  the input \code{id} vector.
}
\value{
  List:
  \item{x}{x coordinates}
  \item{y}{y coordinates}
  \item{id}{identifiers defining the resulting tiles}
}
\author{
  Simon Urbanek
}
%\seealso{
%}
%\examples{
%}
\keyword{manip}
