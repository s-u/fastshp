\name{plot.shp}
\alias{plot.shp}
\title{
  Plot shape files
}
\description{
  \code{plot.shp} plots a list of shapes
}
\usage{
plot.shp(x, xlim, ylim, asp = 1/cos(sum(ylim)/360 * pi), add = FALSE,
         axes = FALSE, full = TRUE, hold = FALSE,
         col = "#e0e0e0", border = "#808080", ...)
}
\arguments{
  \item{x}{\code{"shp"} object as returned from \code{\link{read.shp}}}
  \item{xlim}{range to plot in \code{x} direction; defaults to the range
    of all bounding boxes}
  \item{ylim}{range to plot in \code{x} direction; defaults to the range
    of all bounding boxes}
  \item{asp}{aspect ratio; the default ensures least distortion in the
    center of the image}
  \item{add}{logical; if \code{TRUE} polygons are added to the existing
    plot and \code{xlim}, \code{ylim}, \code{asp}, \code{axes},
    \code{full} are ignored. Otherwise a new plot is created.}
  \item{axes}{logical; passed to \code{plot}}.
  \item{full}{logical; if \code{TRUE} then margins are removed for
    full-size plot, otherwise margins are not touched.}
  \item{hold}{logical; if \code{TRUE} then the drawing code is wrapped
    in \code{dev.hold()}/\code{dev.flush()}}
  \item{col}{colors for the shapes; each shape (possibly consisting of
    multiple polygons) consumes one element}
  \item{border}{borders for the shapes; consumer and recycled jsut like
    \code{col}}
  \item{\dots}{additional arguments}
}
%\details{
%}
\value{
\code{NULL} invisibly
}
%\references{
%}
\author{
Simon Urbanek
}
\note{
  It is most efficient to plot the result of \code{read.shp(...,
    format="list")}. All other types are converted into that type before
  plotting. It is most inefficient to use \code{format="table"}.
}
\seealso{
  \code{\link{read.shp}}
}
\examples{
  # Census 2010 TIGER/Line(TM) state shapefile
  fn <- system.file("shp", "tl_2010_us_state10.shp.xz", package="fastshp")
  # contrary to the advice above we use the table format, because
  # it is a huge file with many points, so we use constrained thinning
  # which works on tables
  s <- read.shp(xzfile(fn, "rb"), "table")
  # substantially reduce the number of points
  s <- s[thin.shp(s, 0.01),]
  # focus on continental US only
  q <- list(x=c(-127.35, -65), y = c(51.5, 22.23))
  plot.shp(s, q$x, q$y)
}
\keyword{plot}
