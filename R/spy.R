##' Spy
##'
##' Peek into the inside of one of \pkg{pomp}'s objects.
##'
##' @name spy
##' @rdname spy
##' @include pomp_class.R
##' @aliases spy,missing-method spy,ANY-method
##' @param object the object whose structure we wish to examine
##' @seealso Csnippet
##' @example examples/spy.R
##' @family extraction methods
##'
NULL

setGeneric(
  "spy",
  function (object, ...)
    standardGeneric("spy")
)

setMethod(
  "spy",
  signature=signature(object="missing"),
  definition=function (...) {
    reqd_arg("spy","object")
  }
)

setMethod(
  "spy",
  signature=signature(object="ANY"),
  definition=function (object, ...) {
    undef_method("spy",object)
  }
)

##' @rdname spy
##' @export
setMethod(
  "spy",
  signature=signature(object="pomp"),
  definition=function (object) {

    nm <- deparse(substitute(object,env=parent.frame()))
    f <- tempfile()
    con <- file(description=f,open="w+")
    sink(file=con)
    on.exit(if (sink.number()) sink())

    cat("==================\npomp object ",sQuote(nm),":\n\n",sep="")

    if (nrow(object@data) > 0L) {
      cat("- data:\n")
      cat("  -",length(object@times),"records of",
        nrow(object@data),
        ngettext(nrow(object@data),"observable,","observables,"),
        "recorded from t =",
        min(object@times),"to",max(object@times),"\n")
      cat("  - summary of data:\n")
      print(summary(as.data.frame(t(obs(object)))))
    } else {
      cat("- data: <none>\n")
    }

    cat("- zero time, t0 = ",object@t0,"\n",sep="")

    cat("- covariates: ")
    show(object@covar)

    cat("- initial state simulator, rinit: ")
    show(object@rinit)

    cat("- initial state density, dinit: ")
    show(object@dinit)

    cat("- process-model simulator, rprocess: ")
    show(object@rprocess)

    cat("- process model density, dprocess: ")
    show(object@dprocess)

    cat("- measurement model simulator, rmeasure: ")
    show(object@rmeasure)

    cat("- measurement model density, dmeasure: ")
    show(object@dmeasure)

    cat("- measurement model expectation, emeasure: ")
    show(object@emeasure)

    cat("- measurement model covariance, vmeasure: ")
    show(object@vmeasure)

    cat("- prior simulator, rprior: ")
    show(object@rprior)

    cat("- prior density, dprior: ")
    show(object@dprior)

    cat("- deterministic skeleton: ")
    show(object@skeleton)

    if (object@partrans@has) {
      cat("- parameter transformations:\n")
      show(object@partrans)
    }

    if (length(coef(object))>0) {
      cat("- parameter vector:\n")
      print(coef(object))
    } else {
      cat ("- parameter vector unspecified\n");
    }

    if (length(object@userdata)>0) {
      unames <- names(object@userdata)
      utypes <- vapply(object@userdata,typeof,character(1))
      cat(
        "- extra user-defined variables:",
        sprintf("  - %s (type %s)",sQuote(unames),sQuote(utypes)),
        sep="\n"
      )
    }

    cat("\n")

    ## now display C snippets
    if (length(object@solibs) > 0) {
      for (i in seq_along(object@solibs)) {
        cat("- C snippet file ",i,":\n\n")
        cat(object@solibs[[i]]$src)
      }
    }

    sink()
    close(con)
    file.show(f,delete.file=TRUE)
    invisible(NULL)
  }
)
