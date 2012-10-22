\name{inside}
\alias{inside}
\title{
  Finds shapes that contain given points
}
\description{
  \code{inside} returns indices of shapes that contain given points
}
\usage{
inside(shp, x, y, clockwise = TRUE)
}
\arguments{
  \item{shp}{shape object (as returned by \code{read.shp(...,
      format="polygon")}) to match against}
  \item{x}{x coordinates of the points to match}
  \item{y}{y coordinates of the points to match}
  \item{clockwise}{logical: if \code{TRUE} then polygons are oriented
    clockwise, otherwise counter-clockwise}
}
\details{
  The matching uses bounding box as a first approximation and then
  winding rule (due east) to determine whether the point is inside. If
  more than one shape matches, the index of the first matching shape is
  returned. Points exactly on the boundary (as far as possible by
  floating point arithmetics) are not considered inside. Note that the
  shape cooridnates must be in R polygon format (\code{format="polygon"}
  in \code{read.shp}) or have just one part, otherwise parts will not be
  treated properly.
}
\value{
  Integer vector of the same length as the number of points, each value
  is either an index of the matching shape or \code{NA} if the point is
  not inside of any shapes.
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
