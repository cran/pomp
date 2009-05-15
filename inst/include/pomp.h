// -*- C++ -*-

#ifndef _POMP_H_
#define _POMP_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

// prototypes for C-level access to Euler-multinomial distribution functions
// NB: 'reulermultinom' does not call GetRNGstate() and PutRNGstate() internally, so the user must do so
void reulermultinom (int ntrans, double size, double *rate, double dt, double *trans);
double deulermultinom (int ntrans, double size, double *rate, double dt, double *trans, int give_log);

// facility for dotting a vector of parameters ('coef') against a vector of basis-function values ('basis')
double dot_product (int dim, const double *basis, const double *coef);

// Prototype for one-step simulator, as used by "euler.simulate" and "onestep.simulate":
typedef void pomp_onestep_sim(double *x, const double *p, 
			      const int *stateindex, const int *parindex, const int *covindex,
			      int ncovars, const double *covars,
			      double t, double dt);
// Description:
//  on input:
// x          = pointer to state vector
// p          = pointer to parameter vector
// stateindex = pointer to vector of integers pointing to the states in 'x' in the order specified by 
//                the 'statenames' argument of 'euler.simulator'
// parindex   = pointer to vector of integers pointing to the parameters in 'p' in the order specified by 
//                the 'paramnames' argument of 'euler.simulator'
// covindex   = pointer to vector of integers pointing to the covariates in 'covars' in the order 
//                specified by the 'covarnames' argument of 'euler.simulator'
// ncovars    = number of covariates
// covars     = pointer to a vector containing the values of the covariates at time t, as interpolated 
//                from the covariate table supplied to 'euler.simulator'
// t          = time at the beginning of the Euler step
// dt         = size (duration) of the Euler step
//  on output:
// x          = contains the new state vector (i.e., at time t+dt)
//
// NB: There is no need to call GetRNGstate() or PutRNGstate() in the body of the user-defined function.
//     The RNG is initialized before any call to this function, and the RNG state is written afterward.
//     Inclusion of these calls in the user-defined function may result in significant slowdown.


// Prototype for one-step log probability density function, as used by "onestep.density":
typedef void pomp_onestep_pdf(double *f, 
			      double *x1, double *x2, double t1, double t2, const double *p, 
			      const int *stateindex, const int *parindex, const int *covindex,
			      int ncovars, const double *covars);
// Description:
//  on input:
// x1         = pointer to state vector at time t1
// x2         = pointer to state vector at time t2
// t1         = time corresponding to x1
// t2         = time corresponding to x2
// p          = pointer to parameter vector
// stateindex = pointer to vector of integers indexing the states in 'x' in the order specified by 
//                the 'statenames' argument of 'euler.density'
// parindex   = pointer to vector of integers indexing the parameters in 'p' in the order specified by 
//                the 'paramnames' argument of 'euler.density'
// covindex   = pointer to vector of integers indexing the parameters in 'covar'' in the order specified by 
//                the 'covarnames' argument of 'euler.density'
// ncovars    = number of covariates
// covars     = pointer to a vector containing the values of the covariates at time t, as interpolated 
//                from the covariate table supplied to 'euler.density'
//  on output:
// f          = pointer to the probability density (a single scalar)

// Prototype for deterministic skeleton evaluation
typedef void pomp_vectorfield_map (double *f, double *x, double *p, 
				   int *stateindex, int *parindex, int *covindex, 
				   int ncovars, double *covars, double t);

// Description:
//  on input:
// x          = pointer to state vector at time t
// p          = pointer to parameter vector
// stateindex = pointer to vector of integers indexing the states in 'x' in the order specified by 
//                the 'statenames' slot
// parindex   = pointer to vector of integers indexing the parameters in 'p' in the order specified by 
//                the 'paramnames' slot
// covindex   = pointer to vector of integers indexing the parameters in 'covar'' in the order specified by 
//                the 'covarnames' slot
// ncovars    = number of covariates
// covars     = pointer to a vector containing the values of the covariates at time t, as interpolated 
//                from the covariate table supplied to 'pomp.skeleton'
// t          = time at the beginning of the Euler step
//  on output:
// f          = pointer to value of the map or vectorfield (a vector of the same length as 'x')

// Prototype for measurement model simulation
typedef void pomp_measure_model_simulator (double *y, double *x, double *p, 
					   int *stateindex, int *parindex, int *covindex,
					   int ncovars, double *covars, double t);
// Description:
//  on input:
// x          = pointer to state vector at time t
// p          = pointer to parameter vector
// stateindex = pointer to vector of integers indexing the states in 'x' in the order specified by 
//                the 'statenames' slot
// parindex   = pointer to vector of integers indexing the parameters in 'p' in the order specified by 
//                the 'paramnames' slot
// covindex   = pointer to vector of integers indexing the parameters in 'covar'' in the order specified by 
//                the 'covarnames' slot
// ncovars    = number of covariates
// covars     = pointer to a vector containing the values of the covariates at time t, as interpolated 
//                from the covariate table supplied to 'pomp.skeleton'
// t          = time at the beginning of the Euler step
//  on output:
// y          = pointer to vector containing simulated observations (length = nobs = nrow(data))
//
// NB: There is no need to call GetRNGstate() or PutRNGstate() in the body of the user-defined function.
//     The RNG is initialized before any call to this function, and the RNG state is written afterward.
//     Inclusion of these calls in the user-defined function may result in significant slowdown.


// Prototype for measurement model density evaluator
typedef void pomp_measure_model_density (double *lik, double *y, double *x, double *p, int give_log,
					 int *stateindex, int *parindex, int *covindex,
					 int ncovars, double *covars, double t);
// Description:
//  on input:
// y          = pointer to vector of observables at time t
// x          = pointer to state vector at time t
// p          = pointer to parameter vector
// give_log   = should the log likelihood be returned?
// stateindex = pointer to vector of integers indexing the states in 'x' in the order specified by 
//                the 'statenames' slot
// parindex   = pointer to vector of integers indexing the parameters in 'p' in the order specified by 
//                the 'paramnames' slot
// covindex   = pointer to vector of integers indexing the parameters in 'covar'' in the order specified by 
//                the 'covarnames' slot
// ncovars    = number of covariates
// covars     = pointer to a vector containing the values of the covariates at time t, as interpolated 
//                from the covariate table supplied to 'pomp.skeleton'
// t          = time at the beginning of the Euler step
//  on output:
// lik        = pointer to scalar containing (log) likelihood

#endif
