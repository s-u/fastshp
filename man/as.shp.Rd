\name{as.shp}
\alias{as.shp}
\title{
  Coerce objects to shapefiles
}
\description{
  \code{as.shp} coerces lists of polygons to adhere to the
  \code{shp} representation.
}
\usage{
as.shp(x)
}
\arguments{
  \item{x}{object to coerce}
}
\details{
  Objects of the class \code{"shp"} are lists of shapes. Each shape
  comprises of polygons and has an identifier and a bounding box.

  The \code{as.shp} method coerces various representations of shape sets
  into \code{"shp"} objects.

  Currently the only supported representation to convert from is a list
  of \code{"x"}, \code{"y"} pairs, e.g.: \code{list(list(x=..., y=...),
  list(x=..., y=...), ...)} where each element of the list defines a
  shape. The shapes will be assigned sequential identifiers starting at
  1.
}
\value{
  Object of the class \code{"shp"}.
}
%\references{
%}
\author{
  Simon Urbanek
}
%\note{
%}
%\seealso{
%}
%\examples{
%}
\keyword{manip}
