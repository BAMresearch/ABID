#'@title get_overlap.
#'
#'@description
#'\code{get_overlap} will group a numeric vector according to a specified gap.
#'
#'@param x Vector of masses or of class MALDIquant "MassSpectrum".
#'@param y Vector of masses or of class MALDIquant "MassSpectrum".
#'@param type Comparison type.
#'@param dmz Delta mz in milli Dalton.
#'@param ppm Parts per million parameter. 
#'@param digits Rounding precision.
#'
#'@details
#'tbd.
#'
#'@return
#'A factor vector that can be used to split x into groups.
#'
#'@importFrom MALDIquant mass intensity
#'
#'@keywords internal
get_overlap <- function(x=NULL, y=NULL, type=c("sum","rel","masses","intweight"), dmz=2, ppm=2, digits=2) {
  ppm <- ppm/10^6
  if (class(x) %in% c("MassSpectrum","MassPeaks")) {
    xm <- mass(x)
    xi <- intensity(x)
  } else {
    xm <- x[,1]
    xi <- x[,2]
  }
  if (class(y) %in% c("MassSpectrum","MassPeaks")) {
    ym <- mass(y)
    yi <- intensity(y)
  } else {
    ym <- y[,1]
    yi <- y[,2]
  }
  if (length(xm)==0 | length(ym)==0) {
    # avoid errors for comparison with an empty dataset (no peak obtained)
    check_presence <- NA
  } else {
    check_presence <- sapply(xm, function(z) {ifelse(any(abs(ym-z)<max(dmz, z*ppm)),which.min(abs(ym-z)),NA)})
  }
  #if (is.list(check_presence)) browser()
  out <- switch(type,
                "sum" = sum(is.finite(check_presence)),
                "rel" = sum(is.finite(check_presence))/length(xm),
                "masses" = xm[is.finite(check_presence)],
                "intweight" = sum(xi[is.finite(check_presence)])/sum(xi),
                "all" = data.frame(
                  "matching.peptides"=sum(is.finite(check_presence)),
                  "rel"=round(sum(is.finite(check_presence))/length(check_presence), digits = digits),
                  "intweight"=round(sum(xi[is.finite(check_presence)])/sum(xi), digits = digits),
                  "masses"=paste(xm[is.finite(check_presence)],sep=" ",collapse=" "),
                  stringsAsFactors = FALSE)
  )
  return(out)
}