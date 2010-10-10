// -*- C++ -*-

#include "pomp_internal.h"
#include <Rdefines.h>

SEXP simulation_computations (SEXP object, SEXP params, SEXP times, SEXP t0, SEXP nsim, SEXP obs, SEXP states)
{
  int nprotect = 0;
  SEXP xstart, x, xx, x0, y, p, alltimes, coef, yy, offset;
  SEXP ans, ans_names;
  SEXP po, popo;
  SEXP statenames, paramnames, obsnames, statedim, obsdim;
  int nsims, nparsets, nreps, npars, nvars, ntimes, nobs;
  int qobs, qstates;
  int *dim, dims[3];
  double *s, *t, *xs, *xt, *ys, *yt, *ps, *pt, tt;
  int i, j, k;

  PROTECT(offset = NEW_INTEGER(1)); nprotect++;
  INTEGER(offset)[0] = 1;

  nsims = INTEGER(AS_INTEGER(nsim))[0]; // number of simulations per parameter set
  if (LENGTH(nsim)>1)
    warning("only the first number in 'nsim' is significant");
  if (nsims < 1) 
    return R_NilValue;		// no work to do  

  qobs = LOGICAL(AS_LOGICAL(obs))[0];	    // 'obs' flag set?
  qstates = LOGICAL(AS_LOGICAL(states))[0]; // 'states' flag set?

  PROTECT(paramnames = GET_ROWNAMES(GET_DIMNAMES(params))); nprotect++;
  dim = INTEGER(GET_DIM(params));
  npars = dim[0]; nparsets = dim[1];

  nreps = nsims*nparsets;

  // initialize the simulations
  PROTECT(xstart = do_init_state(object,params,t0)); nprotect++;
  PROTECT(statenames = GET_ROWNAMES(GET_DIMNAMES(xstart))); nprotect++;
  dim = INTEGER(GET_DIM(xstart));
  nvars = dim[0];

  // augment the 'times' vector with 't0'
  ntimes = LENGTH(times);

  if (ntimes < 1)
    error("if 'times' is empty, there is no work to do");
  
  PROTECT(alltimes = NEW_NUMERIC(ntimes+1)); nprotect++;
  tt = REAL(t0)[0];
  s = REAL(times);
  t = REAL(alltimes);

  if (tt > *s)
    error("the zero-time 't0' must occur no later than the first observation 'times[1]'");
  
  *(t++) = tt;			// copy t0 into alltimes[1]
  tt = *(t++) = *(s++);		// copy times[1] into alltimes[2]
  
  for (j = 1; j < ntimes; j++) { // copy times[2:ntimes] into alltimes[3:(ntimes+1)]
    if (tt >= *s)
      error("'times' must be a nondecreasing sequence of times");
    tt = *(t++) = *(s++);
  }

  // call 'rprocess' to simulate state process
  if (nsims == 1) {		// only one simulation per parameter set

    PROTECT(x = do_rprocess(object,xstart,alltimes,params,offset)); nprotect++;

  } else {			// nsim > 1

    dims[0] = npars; dims[1] = nreps;
    PROTECT(p = makearray(2,dims)); nprotect++;
    setrownames(p,paramnames,2);

    dims[0] = nvars;
    PROTECT(x0 = makearray(2,dims)); nprotect++;
    setrownames(x0,statenames,2);

    // make 'nsims' copies of the parameters and initial states
    for (k = 0, pt = REAL(p), xt = REAL(x0); k < nsims; k++) {
      for (j = 0, ps = REAL(params), xs = REAL(xstart); j < nparsets; j++) {
	for (i = 0; i < npars; i++, pt++, ps++) *pt = *ps;
	for (i = 0; i < nvars; i++, xt++, xs++) *xt = *xs;
      }
    }

    PROTECT(x = do_rprocess(object,x0,alltimes,p,offset)); nprotect++;

  }

  if (!qobs && qstates) {	// obs=F,states=T: return states only

    UNPROTECT(nprotect);
    return x;

  } else {

    if (nsims == 1) {
      PROTECT(y = do_rmeasure(object,x,times,params)); nprotect++;
    } else {
      PROTECT(y = do_rmeasure(object,x,times,p)); nprotect++;
    }
    
    if (qobs) {
    
      if (qstates) { // obs=T,states=T: return a list with states and obs'ns

	PROTECT(ans = NEW_LIST(2)); nprotect++;
	PROTECT(ans_names = NEW_CHARACTER(2)); nprotect++;
	SET_NAMES(ans,ans_names);
	SET_STRING_ELT(ans_names,0,mkChar("states"));
	SET_STRING_ELT(ans_names,1,mkChar("obs"));
	SET_ELEMENT(ans,0,x);
	SET_ELEMENT(ans,1,y);
	UNPROTECT(nprotect);
	return ans;

      } else {		   // obs=T,states=F: return observations only

	UNPROTECT(nprotect);
	return y;

      } 

    } else {	    // obs=F,states=F: return one or more pomp objects

      PROTECT(obsnames = GET_ROWNAMES(GET_DIMNAMES(y))); nprotect++;
      nobs = INTEGER(GET_DIM(y))[0];

      PROTECT(obsdim = NEW_INTEGER(2)); nprotect++;
      INTEGER(obsdim)[0] = nobs;
      INTEGER(obsdim)[1] = ntimes;

      PROTECT(statedim = NEW_INTEGER(2)); nprotect++;
      INTEGER(statedim)[0] = nvars;
      INTEGER(statedim)[1] = ntimes;

      PROTECT(coef = NEW_NUMERIC(npars)); nprotect++;
      SET_NAMES(coef,paramnames);

      PROTECT(po = duplicate(object)); nprotect++;
      SET_SLOT(po,install("t0"),t0);
      SET_SLOT(po,install("times"),times);
      SET_SLOT(po,install("params"),coef);

      if (nreps == 1) {
      
	SET_DIM(y,obsdim);
	setrownames(y,obsnames,2);
	SET_SLOT(po,install("data"),y);
      
	SET_DIM(x,statedim);
	setrownames(x,statenames,2);
	SET_SLOT(po,install("states"),x);

	ps = REAL(params);
	pt = REAL(GET_SLOT(po,install("params")));
	for (i = 0; i < npars; i++, ps++, pt++) *pt = *ps;

	UNPROTECT(nprotect);
	return po;

      } else {
      
	// a list to hold the pomp objects
	PROTECT(ans = NEW_LIST(nreps)); nprotect++; 

	// create an array for the 'states' slot
	PROTECT(xx = makearray(2,INTEGER(statedim))); nprotect++;
	setrownames(xx,statenames,2);
	SET_SLOT(po,install("states"),xx);

	// create an array for the 'data' slot
	PROTECT(yy = makearray(2,INTEGER(obsdim))); nprotect++;
	setrownames(yy,obsnames,2);
	SET_SLOT(po,install("data"),yy);

	xs = REAL(x); 
	ys = REAL(y); 
	if (nsims == 1)
	  ps = REAL(params);
	else
	  ps = REAL(p);

	for (k = 0; k < nreps; k++) { // loop over replicates

	  PROTECT(popo = duplicate(po));

	  // copy x[,k,] and y[,k,] into popo
	  xt = REAL(GET_SLOT(popo,install("states"))); 
	  yt = REAL(GET_SLOT(popo,install("data")));
	  pt = REAL(GET_SLOT(popo,install("params")));

	  for (i = 0; i < npars; i++, pt++, ps++) *pt = *ps;
	  for (j = 0; j < ntimes; j++) {
	    for (i = 0; i < nvars; i++, xt++) *xt = xs[i+nvars*(k+nreps*j)];
	    for (i = 0; i < nobs; i++, yt++) *yt = ys[i+nobs*(k+nreps*j)];
	  }

	  SET_ELEMENT(ans,k,popo);
	  UNPROTECT(1);

	}

	UNPROTECT(nprotect);
	return ans;

      }

    }

  }

}

