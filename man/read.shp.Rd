\name{read.shp}
\alias{read.shp}
\title{
  Read ESRI shapefile
}
\description{
  \code{read.shp} reads ESRI shapefile format (\code{.shp}). Currtenly
  only polygon and polylines are supported.
}
\usage{
read.shp(fn, format = c("list", "pairlist", "polygon", "table"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{filename to read the data from or a raw vector with the data content}
  \item{format}{output format (see below for details), defaults to "list".}
}
%\details{
%}
\value{
  The result depends on the value of the \code{format} argument:

  \item{"list"}{list (generic vector) of shapes exactly as represented
    in the \code{.shp} file format: each shape is represented by a list
    with the following elements:
    \itemize{
    \item{id}{shape identifier}
    \item{type}{shape type}
    \item{box}{bounding box - a vector of \code{xmin, ymin, xmax, ymax}}
    \item{parts}{0-based index of the beginning of each part}
    \item{x}{x coordinates (typically longitude)}
    \item{y}{y coordinates (typically latitude)}
    }
  }
  \item{"pairlist"}{same as "list" except that the list of shapes is
    stored in a pairlist and not a list. This is the most
    memory-efficient way of reading a shapefile, because and all other
    formats are derived from first reading this format. Pairlists are
    good for linear scans but inefficient for indexing.}
  \item{"polygon"}{same as "list" except that coordinates are
    represented in R polygon format (parts are separated by \code{NA}s
    in the coordinates) instead of part indexing. This is typically the
    preferred format for plotting.}
  \item{"table"}{The result is a data frame with the following columns:
    \code{id}, \code{type}, \code{part}, \code{x},
    \code{y}. Coordinates for each part are therefore identified by
    common \code{id}, \code{part} tuple. This is typically the preferred
    format for performing computations on the coordinates.}
}
%\references{
%}
\author{
  Simon Urbanek
}
\note{
  Although other packages exist for reading shapefiles, the focus of
  this implementation is speed, so it works on very large compilations
  of shapes, such as the Tiger database which is impossible to load
  using naive R implementations.
}
%\seealso{
%}
%\examples{
%}
\keyword{IO}
