// dear emacs, please treat this as -*- C++ -*-

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Arith.h>
#include <string.h>

#include "pomp_internal.h"

static SEXP pomp_default_rprocess (SEXP xstart, int nvars, int nreps, int ntimes)
{
  SEXP Snames, X;
  int dim[3] = {nvars, nreps, ntimes};
  int i, n = nvars*nreps*ntimes;
  double *xp = 0;
  PROTECT(Snames = GET_ROWNAMES(GET_DIMNAMES(xstart)));
  PROTECT(X = makearray(3,dim));
  setrownames(X,Snames,3);
  for (i= 0, xp = REAL(X); i < n; i++, xp++) *xp = R_NaReal;
  warningcall(R_NilValue,"'rprocess' unspecified: NAs generated.");
  UNPROTECT(2);
  return X;
}

SEXP do_rprocess (SEXP object, SEXP xstart, SEXP tstart, SEXP times, SEXP params, SEXP gnsi)
{
  int nprotect = 0;
  int *xdim, type, nvars, npars, nreps, nrepsx, ntimes;
  SEXP X, copy, rproc, args, accumvars, covar;
  SEXP dimXstart, dimP;
  const char *dimnm[3] = {"variable","rep","time"};

  PROTECT(gnsi = duplicate(gnsi)); nprotect++;

  PROTECT(tstart = AS_NUMERIC(tstart)); nprotect++;

  PROTECT(times = AS_NUMERIC(times)); nprotect++;
  ntimes = length(times);
  if (ntimes < 1) {
    errorcall(R_NilValue,"length(times) < 1: no work to do.");
  }

  PROTECT(xstart = as_matrix(xstart)); nprotect++;
  PROTECT(dimXstart = GET_DIM(xstart)); nprotect++;
  xdim = INTEGER(dimXstart);
  nvars = xdim[0]; nrepsx = xdim[1];

  PROTECT(params = as_matrix(params)); nprotect++;
  PROTECT(dimP = GET_DIM(params)); nprotect++;
  xdim = INTEGER(dimP);
  npars = xdim[0]; nreps = xdim[1];

  if (nrepsx > nreps) {		// more ICs than parameters
    if (nrepsx % nreps != 0) {
      errorcall(R_NilValue,"the larger number of replicates is not a multiple of smaller.");
    } else {
      double *src, *tgt;
      int dims[2];
      int j, k;
      dims[0] = npars; dims[1] = nrepsx;
      PROTECT(copy = duplicate(params)); nprotect++;
      PROTECT(params = makearray(2,dims)); nprotect++;
      setrownames(params,GET_ROWNAMES(GET_DIMNAMES(copy)),2);
      src = REAL(copy);
      tgt = REAL(params);
      for (j = 0; j < nrepsx; j++) {
        for (k = 0; k < npars; k++, tgt++) {
          *tgt = src[k+npars*(j%nreps)];
        }
      }
    }
    nreps = nrepsx;
  } else if (nrepsx < nreps) {	// more parameters than ICs
    if (nreps % nrepsx != 0) {
      errorcall(R_NilValue,"the larger number of replicates is not a multiple of smaller.");
    } else {
      double *src, *tgt;
      int dims[2];
      int j, k;
      dims[0] = nvars; dims[1] = nreps;
      PROTECT(copy = duplicate(xstart)); nprotect++;
      PROTECT(xstart = makearray(2,dims)); nprotect++;
      setrownames(xstart,GET_ROWNAMES(GET_DIMNAMES(copy)),2);
      src = REAL(copy);
      tgt = REAL(xstart);
      for (j = 0; j < nreps; j++) {
        for (k = 0; k < nvars; k++, tgt++) {
          *tgt = src[k+nvars*(j%nrepsx)];
        }
      }
    }
  }

  PROTECT(rproc = GET_SLOT(object,install("rprocess"))); nprotect++;
  PROTECT(args = VectorToPairList(GET_SLOT(object,install("userdata")))); nprotect++;
  PROTECT(accumvars = GET_SLOT(object,install("accumvars"))); nprotect++;
  PROTECT(covar = GET_SLOT(object,install("covar"))); nprotect++;

  // extract the process function
  type = *(INTEGER(GET_SLOT(rproc,install("type"))));
  switch (type) {
  case onestep: // one-step simulator
  {
    SEXP fn;
    double deltat = 1.0;
    PROTECT(fn = GET_SLOT(rproc,install("step.fn"))); nprotect++;
    PROTECT(X = euler_model_simulator(fn,xstart,tstart,times,params,deltat,type,
      accumvars,covar,args,gnsi)); nprotect++;
  }
    break;
  case discrete: case euler: // discrete-time and Euler
  {
    SEXP fn;
    double deltat;
    PROTECT(fn = GET_SLOT(rproc,install("step.fn"))); nprotect++;
    deltat = *(REAL(AS_NUMERIC(GET_SLOT(rproc,install("delta.t")))));
    PROTECT(X = euler_model_simulator(fn,xstart,tstart,times,params,deltat,type,
      accumvars,covar,args,gnsi)); nprotect++;
  }
    break;
  case gill: // Gillespie's method
  {
    SEXP fn, vmatrix, hmax;
    PROTECT(fn = GET_SLOT(rproc,install("rate.fn"))); nprotect++;
    PROTECT(vmatrix = GET_SLOT(rproc,install("v"))); nprotect++;
    PROTECT(hmax = GET_SLOT(rproc,install("hmax"))); nprotect++;
    PROTECT(X = SSA_simulator(fn,xstart,tstart,times,params,vmatrix,covar,
      accumvars,hmax,args,gnsi)); nprotect++;
  }
    break;
  case dflt: default:
    PROTECT(X = pomp_default_rprocess(xstart,nvars,nreps,ntimes)); nprotect++;
    break;
  }

  fixdimnames(X,dimnm,3);
  UNPROTECT(nprotect);
  return X;

}
