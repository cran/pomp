// dear emacs, please treat this as -*- C++ -*-

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "pomp_internal.h"

SEXP do_init_state (SEXP object, SEXP params, SEXP t0)
{
  int nprotect = 0;
  SEXP X, fcall, fn, rho, covar, tcovar, covars;
  int index = 0;

  // extract the initializer function and its environment
  PROTECT(fn = GET_SLOT(object,install("initializer"))); nprotect++;
  PROTECT(rho = (CLOENV(fn))); nprotect++;

  // begin to construct the call
  PROTECT(fcall = VectorToPairList(GET_SLOT(object,install("userdata")))); nprotect++;
  PROTECT(fcall = LCONS(GET_SLOT(object,install("covarnames")),fcall)); nprotect++;
  SET_TAG(fcall,install("covarnames"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("paramnames")),fcall)); nprotect++;
  SET_TAG(fcall,install("paramnames"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("statenames")),fcall)); nprotect++;
  SET_TAG(fcall,install("statenames"));

  // extract covariates and interpolate
  PROTECT(tcovar = GET_SLOT(object,install("tcovar"))); nprotect++;
  if (LENGTH(tcovar) > 0) {	// do table lookkup
    PROTECT(covar = GET_SLOT(object,install("covar"))); nprotect++;
    PROTECT(covars = lookup_in_table(tcovar,covar,t0,&index)); nprotect++;
    PROTECT(fcall = LCONS(covars,fcall)); nprotect++;
    SET_TAG(fcall,install("covars"));
  }

  // finish constructing the call
  PROTECT(fcall = LCONS(t0,fcall)); nprotect++;
  SET_TAG(fcall,install("t0"));
  PROTECT(fcall = LCONS(params,fcall)); nprotect++;
  SET_TAG(fcall,install("params"));
  PROTECT(fcall = LCONS(fn,fcall)); nprotect++;

  PROTECT(X = eval(fcall,rho)); nprotect++; // do the call

  if (!IS_NUMERIC(X) || isNull(GET_NAMES(X)))
    error("init.state error: user 'initializer' must return a named numeric vector");

  UNPROTECT(nprotect);
  return X;
}
