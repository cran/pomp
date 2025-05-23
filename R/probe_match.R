##' Probe matching
##'
##' Estimation of parameters by maximum synthetic likelihood
##'
##' In probe-matching, one attempts to minimize the discrepancy between simulated and actual data, as measured by a set of summary statistics called \emph{probes}.
##' In \pkg{pomp}, this discrepancy is measured using the \dQuote{synthetic likelihood} as defined by Wood (2010).
##'
##' @docType methods
##' @name probe_match
##' @rdname probe_match
##' @aliases probe_objfun probe_objfun,missing-method probe_objfun,ANY-method
##' @include probe.R plot.R
##' @author Aaron A. King
##' @concept probe matching
##' @concept synthetic likelihood
##' @family summary statistic-based methods
##' @family estimation methods
##' @family methods based on maximization
##' @references
##'
##' \Kendall1999
##'
##' \Wood2010
##'
##' @seealso \code{\link[stats]{optim}} \code{\link[subplex]{subplex}} \code{\link[nloptr]{nloptr}}
##' @param est character vector; the names of parameters to be estimated.
##' @param fail.value optional numeric scalar;
##' if non-\code{NA}, this value is substituted for non-finite values of the objective function.
##' It should be a large number (i.e., bigger than any legitimate values the objective function is likely to take).
##' @param seed  integer.
##' When fitting, it is often best to fix the seed of the random-number generator (RNG).
##' This is accomplished by setting \code{seed} to an integer.
##' By default, \code{seed = NULL}, which does not alter the RNG state.
##' @inheritParams probe
##' @inheritParams pomp
##' @return
##' \code{probe_objfun} constructs a stateful objective function for probe matching.
##' Specifically, \code{probe_objfun} returns an object of class \sQuote{probe_match_objfun}, which is a function suitable for use in an \code{\link[stats]{optim}}-like optimizer.
##' In particular, this function takes a single numeric-vector argument that is assumed to contain the parameters named in \code{est}, in that order.
##' When called, it will return the negative synthetic log likelihood for the probes specified.
##' It is a stateful function:
##' Each time it is called, it will remember the values of the parameters and its estimate of the synthetic likelihood.
##' @inheritSection pomp Note for Windows users
##' @inheritSection objfun Important Note
##' @inheritSection objfun Warning! Objective functions based on C snippets
##' @example examples/probe_match.R
##'
NULL

setClass(
  "probe_match_objfun",
  contains="function",
  slots=c(
    env="environment",
    est="character"
  )
)

setGeneric(
  "probe_objfun",
  function (data, ...)
    standardGeneric("probe_objfun")
)

setMethod(
  "probe_objfun",
  signature=signature(data="missing"),
  definition=function (...) {
    reqd_arg("probe_objfun","data")
  }
)

setMethod(
  "probe_objfun",
  signature=signature(data="ANY"),
  definition=function (data, ...) {
    undef_method("probe_objfun",data)
  }
)

##' @rdname probe_match
##' @export
setMethod(
  "probe_objfun",
  signature=signature(data="data.frame"),
  definition=function (
    data,
    ...,
    est = character(0), fail.value = NA,
    probes, nsim, seed = NULL,
    params, rinit, rprocess, rmeasure, partrans,
    verbose = getOption("verbose", FALSE)
  ) {

    tryCatch(
      pmof_internal(
        data,
        ...,
        est=est,
        fail.value=fail.value,
        probes=probes,
        nsim=nsim,
        seed=seed,
        params=params,
        rinit=rinit,
        rprocess=rprocess,
        rmeasure=rmeasure,
        partrans=partrans,
        verbose=verbose
      ),
      error = function (e) pStop(who="probe_objfun",conditionMessage(e))
    )

  }
)

##' @rdname probe_match
##' @export
setMethod(
  "probe_objfun",
  signature=signature(data="pomp"),
  definition=function (
    data,
    ...,
    est = character(0),
    fail.value = NA,
    probes, nsim, seed = NULL,
    verbose = getOption("verbose", FALSE)
  ) {

    tryCatch(
      pmof_internal(
        data,
        ...,
        est=est,
        fail.value=fail.value,
        probes=probes,
        nsim=nsim,
        seed=seed,
        verbose=verbose
      ),
      error = function (e) pStop(who="probe_objfun",conditionMessage(e))
    )

  }
)

##' @rdname probe_match
##' @export
setMethod(
  "probe_objfun",
  signature=signature(data="probed_pomp"),
  definition=function (
    data,
    ...,
    est = character(0),
    fail.value = NA,
    probes, nsim, seed = NULL,
    verbose = getOption("verbose", FALSE)
  ) {

    if (missing(probes)) probes <- data@probes
    if (missing(nsim)) nsim <- data@nsim

    probe_objfun(
      as(data,"pomp"),
      ...,
      est=est,
      fail.value=fail.value,
      probes=probes,
      nsim=nsim,
      seed=seed,
      verbose=verbose
    )

  }
)

##' @rdname probe_match
##' @export
setMethod(
  "probe_objfun",
  signature=signature(data="probe_match_objfun"),
  definition=function (
    data,
    ...,
    est, fail.value, seed = NULL,
    verbose = getOption("verbose", FALSE)
  ) {

    if (missing(est)) est <- data@est
    if (missing(fail.value)) fail.value <- data@env$fail.value

    probe_objfun(
      data@env$object,
      ...,
      est=est,
      fail.value=fail.value,
      seed=seed,
      verbose=verbose
    )

  }
)

pmof_internal <- function (
  object,
  ...,
  est, fail.value = NA,
  probes, nsim, seed = NULL,
  verbose
) {

  verbose <- as.logical(verbose)

  object <- probe(object,probes=probes,nsim=nsim,seed=seed,...,verbose=verbose)

  fail.value <- as.numeric(fail.value)
  loglik <- logLik(object)
  .gnsi <- TRUE

  est <- as.character(est)
  est <- est[nzchar(est)]

  params <- coef(object,transform=TRUE)

  idx <- match(est,names(params))
  if (any(is.na(idx))) {
    missing <- est[is.na(idx)]
    pStop_("parameter",ngettext(length(missing),"","s")," ",
      paste(sQuote(missing),collapse=",")," not found in ",sQuote("params"),".")
  }

  pompLoad(object,verbose=verbose)

  ofun <- function (par = numeric(0)) {
    params[idx] <- par
    coef(object,transform=TRUE,.gnsi=.gnsi) <<- params
    loglik <<- probe.eval(object,.gnsi=.gnsi)
    .gnsi <<- FALSE
    if (is.finite(loglik) || is.na(fail.value)) -loglik else fail.value
  }

  environment(ofun) <- list2env(
    list(object=object,fail.value=fail.value,.gnsi=.gnsi,
      params=params,idx=idx,loglik=loglik,seed=seed),
    parent=parent.frame(2)
  )

  new("probe_match_objfun",ofun,env=environment(ofun),est=est)

}

probe.eval <- function (object, .gnsi = TRUE) {

  ## apply probes to model simulations
  simvals <- tryCatch(
    freeze(
      .Call(P_apply_probe_sim,object=object,nsim=object@nsim,params=object@params,
        probes=object@probes,datval=object@datvals,.gnsi=.gnsi),
      seed=object@seed
    ),
    error = function (e) pStop_("applying probes to simulated data: ",conditionMessage(e))
  )

  tryCatch(
    .Call(P_synth_loglik,simvals,object@datvals),
    error = function (e) pStop_("in synthetic likelihood computation: ",conditionMessage(e))
  )

}

##' @rdname probe
##' @details
##' When \code{probe} operates on a probe-matching objective function (a \sQuote{probe_match_objfun} object), by default, the
##' random-number generator seed is fixed at the value given when the objective function was constructed.
##' Specifying \code{NULL} or an integer for \code{seed} overrides this behavior.
##' @export
setMethod(
  "probe",
  signature=signature(data="probe_match_objfun"),
  definition=function (
    data,
    ...,
    seed,
    verbose = getOption("verbose", FALSE)
  ) {

    if (missing(seed)) seed <- data@env$seed

    probe(
      data@env$object,
      ...,
      seed=seed,
      verbose=verbose
    )

  }
)

setAs(
  from="probe_match_objfun",
  to="probed_pomp",
  def = function (from) {
    from@env$object
  }
)

##' @rdname plot
##' @export
setMethod(
  "plot",
  signature=signature(x="probe_match_objfun"),
  definition=function (x, ...) {
    plot(as(x,"probed_pomp"),...)
  }
)
