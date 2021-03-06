% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_overlap.R
\name{get_overlap}
\alias{get_overlap}
\title{get_overlap.}
\usage{
get_overlap(
  x = NULL,
  y = NULL,
  type = c("sum", "rel", "masses", "intweight"),
  dmz = 2,
  ppm = 2,
  digits = 2
)
}
\arguments{
\item{x}{Vector of masses or of class MALDIquant "MassSpectrum".}

\item{y}{Vector of masses or of class MALDIquant "MassSpectrum".}

\item{type}{Comparison type.}

\item{dmz}{Delta mz in milli Dalton.}

\item{ppm}{Parts per million parameter.}

\item{digits}{Rounding precision.}
}
\value{
A factor vector that can be used to split x into groups.
}
\description{
\code{get_overlap} will group a numeric vector according to a specified gap.
}
\details{
tbd.
}
\keyword{internal}
