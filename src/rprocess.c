// dear emacs, please treat this as -*- C++ -*-

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <string.h>

#include "pomp_internal.h"

SEXP do_rprocess (SEXP object, SEXP xstart, SEXP times, SEXP params, SEXP offset)
{
  int nprotect = 0;
  int *xdim, nvars, nreps, ntimes, off;
  SEXP X, Xoff, fn, fcall, rho;
  SEXP dimXstart, dimP, dimX;
  ntimes = length(times);
  if (ntimes < 2) {
    error("rprocess error: no transitions, no work to do");
  }
  off = *(INTEGER(AS_INTEGER(offset)));
  if ((off < 0)||(off>=ntimes))
    error("illegal 'offset' value %d",off);
  PROTECT(dimXstart = GET_DIM(xstart)); nprotect++;
  if ((isNull(dimXstart)) || (length(dimXstart)!=2)) {
    error("rprocess error: 'xstart' must be a rank-2 array");
  }
  PROTECT(dimP = GET_DIM(params)); nprotect++;
  if ((isNull(dimP)) || (length(dimP)!=2)) {
    error("rprocess error: 'params' must be a rank-2 array");
  }
  xdim = INTEGER(dimXstart);
  nvars = xdim[0]; nreps = xdim[1];
  if (nreps != INTEGER(dimP)[1]) {
    error("rprocess error: number of columns of 'params' and 'xstart' do not agree");
  }
  // extract the process function
  PROTECT(fn = GET_SLOT(object,install("rprocess"))); nprotect++;
  // construct the call
  PROTECT(fcall = VectorToPairList(GET_SLOT(object,install("userdata")))); nprotect++;
  PROTECT(fcall = LCONS(GET_SLOT(object,install("PACKAGE")),fcall)); nprotect++;
  SET_TAG(fcall,install("PACKAGE"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("covarnames")),fcall)); nprotect++;
  SET_TAG(fcall,install("covarnames"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("paramnames")),fcall)); nprotect++;
  SET_TAG(fcall,install("paramnames"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("statenames")),fcall)); nprotect++;
  SET_TAG(fcall,install("statenames"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("covar")),fcall)); nprotect++;
  SET_TAG(fcall,install("covar"));
  PROTECT(fcall = LCONS(GET_SLOT(object,install("tcovar")),fcall)); nprotect++;
  SET_TAG(fcall,install("tcovar"));
  PROTECT(fcall = LCONS(params,fcall)); nprotect++;
  SET_TAG(fcall,install("params"));
  PROTECT(fcall = LCONS(AS_NUMERIC(times),fcall)); nprotect++;
  SET_TAG(fcall,install("times"));
  PROTECT(fcall = LCONS(xstart,fcall)); nprotect++;
  SET_TAG(fcall,install("xstart"));
  PROTECT(fcall = LCONS(fn,fcall)); nprotect++;
  PROTECT(rho = (CLOENV(fn))); nprotect++; // environment of the function
  PROTECT(X = eval(fcall,rho)); nprotect++; // do the call
  PROTECT(dimX = GET_DIM(X)); nprotect++;
  if ((isNull(dimX)) || (length(dimX) != 3)) {
    error("rprocess error: user 'rprocess' must return a rank-3 array");
  }
  xdim = INTEGER(dimX);
  if ((xdim[0] != nvars) || (xdim[1] != nreps) || (xdim[2] != ntimes)) {
    error("rprocess error: user 'rprocess' must return a %d x %d x %d array",nvars,nreps,ntimes);
  }
  if (isNull(GET_ROWNAMES(GET_DIMNAMES(X)))) {
    error("rprocess error: user 'rprocess' must return an array with rownames");
  }
  if (off > 0) {
    xdim[2] -= off;
    PROTECT(Xoff = makearray(3,xdim)); nprotect++;
    setrownames(Xoff,GET_ROWNAMES(GET_DIMNAMES(X)),3);
    memcpy(REAL(Xoff),REAL(X)+off*nvars*nreps,(ntimes-off)*nvars*nreps*sizeof(double));
    UNPROTECT(nprotect);
    return Xoff;
  } else {
    UNPROTECT(nprotect);
    return X;
  }
}
