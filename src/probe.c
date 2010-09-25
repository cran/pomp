// -*- mode: C++; -*-
#include "pomp_internal.h"

SEXP apply_probe_data (SEXP object, SEXP probes) {
  int nprotect = 0;
  SEXP retval, data, vals;
  int nprobe;
  int i;

  nprobe = LENGTH(probes);
  PROTECT(data = GET_SLOT(object,install("data"))); nprotect++;
  PROTECT(vals = NEW_LIST(nprobe)); nprotect++;
  SET_NAMES(vals,GET_NAMES(probes));

  for (i = 0; i < nprobe; i++) {
    SET_ELEMENT(vals,i,eval(lang2(VECTOR_ELT(probes,i),data),CLOENV(VECTOR_ELT(probes,i))));
    if (!IS_NUMERIC(VECTOR_ELT(vals,i))) {
      UNPROTECT(nprotect);
      error("probe %ld returns a non-numeric result",i);
    }
  }

  PROTECT(retval = eval(LCONS(install("c"),VectorToPairList(vals)),R_BaseEnv)); nprotect++;

  UNPROTECT(nprotect);
  return retval;
}

SEXP apply_probe_sim (SEXP object, SEXP nsim, SEXP params, SEXP seed, SEXP probes, SEXP datval) {
  int nprotect = 0;
  SEXP y, obs, call, names;
  SEXP retval, val, valnames, x;
  int nprobe, nsims, nvars, ntimes, nvals;
  int xdim[2];
  double *xp, *yp;
  int p, s, i, j, k, len0, len = 0;

  PROTECT(nsim = AS_INTEGER(nsim)); nprotect++;
  if ((LENGTH(nsim)>1) || (INTEGER(nsim)[0]<=0))
    error("'nsim' must be a positive integer");

  // 'names' holds the names of the probe values
  // we get these from a previous call to 'apply_probe_data'
  nprobe = LENGTH(probes);
  nvals = LENGTH(datval);
  PROTECT(names = GET_NAMES(datval)); nprotect++; 

  // call 'simulate' to get simulated data sets
  PROTECT(obs = NEW_LOGICAL(1)); nprotect++;
  LOGICAL(obs)[0] = 1;		// we set obs=TRUE
  PROTECT(call = LCONS(obs,R_NilValue)); nprotect++;
  SET_TAG(call,install("obs"));
  PROTECT(call = LCONS(params,call)); nprotect++;
  SET_TAG(call,install("params"));
  PROTECT(call = LCONS(seed,call)); nprotect++;
  SET_TAG(call,install("seed"));
  PROTECT(call = LCONS(nsim,call)); nprotect++;
  SET_TAG(call,install("nsim"));
  PROTECT(call = LCONS(object,call)); nprotect++;
  SET_TAG(call,install("object"));
  PROTECT(call = LCONS(install("simulate"),call)); nprotect++;
  PROTECT(y = eval(call,R_GlobalEnv)); nprotect++;

  nvars = INTEGER(GET_DIM(y))[0];
  nsims = INTEGER(GET_DIM(y))[1];
  ntimes = INTEGER(GET_DIM(y))[2]; // recall that 'simulate' returns a value for time zero

  // set up temporary storage
  xdim[0] = nvars; xdim[1] = ntimes-1; 
  PROTECT(x = makearray(2,xdim)); nprotect++;
  setrownames(x,GET_ROWNAMES(GET_DIMNAMES(y)),2);

  // set up matrix to hold results
  xdim[0] = nsims; xdim[1] = nvals;
  PROTECT(retval = makearray(2,xdim)); nprotect++;
  PROTECT(valnames = NEW_LIST(2)); nprotect++;
  SET_ELEMENT(valnames,1,names);	// set column names
  SET_DIMNAMES(retval,valnames);

  for (p = 0, k = 0; p < nprobe; p++, k += len) { // loop over probes

    for (s = 0; s < nsims; s++) { // loop over simulations

      // copy the data from y[,s,-1] to x[,]
      xp = REAL(x);
      yp = REAL(y)+nvars*(s+nsims);
      for (j = 1; j < ntimes; j++, yp += nvars*nsims) {
	for (i = 0; i < nvars; i++, xp++) *xp = yp[i];
      }

      // evaluate the probe on the simulated data
      PROTECT(val = eval(lang2(VECTOR_ELT(probes,p),x),CLOENV(VECTOR_ELT(probes,p)))); nprotect++;
      if (!IS_NUMERIC(val)) {
	UNPROTECT(nprotect);
	error("probe %ld returns a non-numeric result",p);
      }

      len = LENGTH(val);
      if (s == 0)
	len0 = len;
      else if (len != len0) {
	UNPROTECT(nprotect);
	error("variable-sized results returned by probe %ld",p);
      }
      if (k+len > nvals)
	error("probes return different number of values on different datasets");

      xp = REAL(retval); yp = REAL(val);
      for (i = 0; i < len; i++) xp[s+nsims*(i+k)] = yp[i];

    }
    
  }
  if (k != nvals)
    error("probes return different number of values on different datasets");
  
  UNPROTECT(nprotect);
  return retval;
}
