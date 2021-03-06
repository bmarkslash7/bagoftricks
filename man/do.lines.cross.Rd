% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dolinescross.R
\name{do.lines.cross}
\alias{do.lines.cross}
\title{Given two line segments, returns TRUE if they cross, FALSE if they don't}
\usage{
do.lines.cross(start.line.one, end.line.one, start.line.two, end.line.two)
}
\arguments{
\item{start.line.one}{list of coordinates c(x,y) for first point on first line}

\item{end.line.one}{list of coordinates c(x,y) for second point on first line}

\item{start.line.two}{list of coordinates c(x,y) for first point on second line}

\item{end.line.two}{list of coordinates c(x,y) for second point on second line}
}
\value{
TRUE if lines cross, FALSE if lines do not
}
\description{
from algorithm described at: http://stackoverflow.com/questions/7069420/check-if-two-line-segments-are-colliding-only-check-if-they-are-intersecting-n
}
\examples{
do.lines.cross(start.line.one = c(0,0), end.line.one = c(1,1),
		start.line.two = c(1,0), end.line.two = c(0,1))
}

