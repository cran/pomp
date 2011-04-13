// dear emacs, please treat this as -*- C++ -*-

#include <Rmath.h>

#include "pomp.h"

#define LOG_BETA    (p[parindex[0]]) // transmission rates
#define LOG_M       (p[parindex[1]]) // Poisson import rate
#define LOG_ALPHA   (p[parindex[2]]) // mixing exponent
#define LOG_RHO     (p[parindex[3]]) // under-reporting
#define PERIOD      (p[parindex[4]]) // period of seasonality
#define DEGREE      (p[parindex[5]]) // degree of B-splines
#define NBASIS      (p[parindex[6]]) // number of B-spline basis functions

#define BIRTHS      (covar[covindex[0]]) // numbers of births

#define SUS         (x[stateindex[0]]) // susceptibles
#define INF         (x[stateindex[1]]) // infectives
#define THETA       (x[stateindex[2]]) // imports

#define REPORTS     (y[obsindex[0]]) // reported cases

void _tsir_binom_dmeasure (double *lik, double *y, double *x, double *p, int give_log,
			  int *obsindex, int *stateindex, int *parindex, int *covindex,
			  int ncovars, double *covars, double t) {
  *lik = dbinom(REPORTS,nearbyint(INF),exp(LOG_RHO),give_log);
}

void _tsir_binom_rmeasure (double *y, double *x, double *p, 
			  int *obsindex, int *stateindex, int *parindex, int *covindex,
			  int ncovars, double *covars, double t) {
  REPORTS = rbinom(nearbyint(INF),exp(LOG_RHO));
}

#undef REPORTS

// Ricker model with log-normal process noise
void _tsir_simulator (double *x, const double *p, 
		      const int *stateindex, const int *parindex, const int *covindex,
		      int covdim, const double *covar, 
		      double t, double dt)
{
  double beta;
  double em;
  double alpha;
  double prob, lambda;
  int nbasis;
  int degree;

  nbasis = (int) NBASIS;
  degree = (int) DEGREE;

  em = exp(LOG_M);
  alpha = exp(LOG_ALPHA);

  {
    double seasonality[nbasis];
    periodic_bspline_basis_eval(t,PERIOD,degree,nbasis,&seasonality[0]);
    beta = exp(dot_product(nbasis,&seasonality[0],&LOG_BETA));
  }

  THETA = rpois(em);			  // imports
  lambda = beta*pow(INF+THETA,alpha)*SUS; // expected number of new infections
  prob = 1.0/(1.0+lambda/INF);
  //  Rprintf("%lg %lg %lg %lg\n",BIRTHS,THETA,INF,SUS);

  INF = rnbinom(INF,prob);	// number of new infections
  INF = (INF > SUS) ? SUS : INF;
  SUS += BIRTHS - INF;		 // susceptible balance
}

void _tsir_skeleton (double *f, double *x, const double *p, 
		       const int *stateindex, const int *parindex, const int *covindex,
		       int covdim, const double *covar, double t) 
{
  double beta;
  double em;
  double alpha;
  double lambda;
  int nbasis;
  int degree;

  nbasis = (int) NBASIS;
  degree = (int) DEGREE;

  em = exp(LOG_M);
  alpha = exp(LOG_ALPHA);

  {
    double seasonality[nbasis];
    periodic_bspline_basis_eval(t,PERIOD,degree,nbasis,&seasonality[0]);
    beta = exp(dot_product(nbasis,&seasonality[0],&LOG_BETA));
  }

  THETA = em;				  // imports
  lambda = beta*pow(INF+THETA,alpha)*SUS; // expected number of new infections

  INF = lambda;	 		// number of new infections
  INF = (INF > SUS) ? SUS : INF;
  SUS += BIRTHS - INF;		 // susceptible balance

}

#undef LOG_BETA
#undef LOG_M
#undef LOG_ALPHA
#undef LOG_RHO
#undef PERIOD
#undef DEGREE
#undef NBASIS

#undef BIRTHS

#undef SUS
#undef INF
#undef THETA

