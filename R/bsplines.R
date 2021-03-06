##' B-spline bases
##'
##' These functions generate B-spline basis functions.  \code{bspline.basis}
##' gives a basis of spline functions.  \code{periodic.bspline.basis} gives a
##' basis of periodic spline functions.
##'
##' @name bsplines
##' @rdname bsplines
##'
##' @param x Vector at which the spline functions are to be evaluated.
##' @param nbasis The number of basis functions to return.
##' @param degree Degree of requested B-splines.
##' @param period The period of the requested periodic B-splines.
##' @param deriv The order of the derivative required.
##' @param names optional; the names to be given to the basis functions.  These
##' will be the column-names of the matrix returned.  If the names are
##' specified as a format string (e.g., "basis\%d"), \code{\link{sprintf}} will
##' be used to generate the names from the column number.  If a single
##' non-format string is specified, the names will be generated by
##' \code{\link{paste}}-ing \code{name} to the column number.  One can also
##' specify each column name explicitly by giving a length-\code{nbasis} string
##' vector.  By default, no column-names are given.
##'
##' @return
##' \item{bspline.basis}{ Returns a matrix with \code{length(x)} rows
##' and \code{nbasis} columns.  Each column contains the values one of the
##' spline basis functions.}
##' \item{periodic.bspline.basis}{ Returns a matrix with \code{length(x)} rows
##' and \code{nbasis} columns.  The basis functions returned are periodic with
##' period \code{period}.}
##' If \code{deriv>0}, the derivative of that order of each of the corresponding spline basis functions are returned.  
##'
##' @section C API:
##' Access to the underlying C routines is available: see 
##' \href{https://kingaa.github.io/pomp/vignettes/C_API.html}{the \pkg{pomp} C API document}.
##' for definition and documentation of the C API.
##'
##' @author Aaron A. King
##'
##' @keywords smooth
##' @examples
##'
##' x <- seq(0,2,by=0.01)
##' y <- bspline.basis(x,degree=3,nbasis=9,names="basis")
##' matplot(x,y,type='l',ylim=c(0,1.1))
##' lines(x,apply(y,1,sum),lwd=2)
##'
##' x <- seq(-1,2,by=0.01)
##' y <- periodic.bspline.basis(x,nbasis=5,names="spline%d")
##' matplot(x,y,type='l')
##'
NULL

##' @rdname bsplines
##' @export
bspline.basis <- function (x, nbasis, degree = 3, deriv = 0, names = NULL) {
  ep <- "bspline.basis"
  y <- tryCatch(
    .Call(P_bspline_basis,x,nbasis,degree,deriv),
    error = function (e) {
      pStop(ep,conditionMessage(e))
    }
  )
  if (deriv > degree)
    pWarn(ep,"returning 0 since ",sQuote("deriv")," > ",sQuote("degree"))
  if (!is.null(names)) {
    if (length(names)==1) {
      if (!grepl("%",names)) {
        names <- paste0(names,".%d")
      }
      colnames(y) <- sprintf(names,seq_len(nbasis))
    } else if (length(names)==nbasis) {
      colnames(y) <- names
    } else {
      pStop(ep,sQuote("names")," must be of length 1 or ",nbasis)
    }
  }
  y
}

##' @rdname bsplines
##' @export
periodic.bspline.basis <- function (x, nbasis, degree = 3, period = 1,
  deriv = 0, names = NULL) {
  ep <- "periodic.bspline.basis"
  y <- tryCatch(
    .Call(P_periodic_bspline_basis,x,nbasis,degree,period,deriv),
    error = function (e) {
      pStop(ep,conditionMessage(e))
    }
  )
  if (deriv > degree)
    pWarn(ep,"returning 0 since ",sQuote("deriv")," > ",sQuote("degree"))
  if (!is.null(names)) {
    if (length(names)==1) {
      if (!grepl("%",names)) {
        names <- paste0(names,".%d")
      }
      colnames(y) <- sprintf(names,seq_len(nbasis))
    } else if (length(names)==nbasis) {
      colnames(y) <- names
    } else {
      pStop(ep,sQuote("names")," must be of length 1 or ",nbasis)
    }
  }
  y
}
