// -*- C++ -*-

#include "pomp_internal.h"

static void bspline_internal (double *y, const double *x, int nx, int i, int p, int d, const double *knots);

// B-spline basis

SEXP bspline_basis (SEXP x, SEXP nbasis, SEXP degree, SEXP deriv) {
  SEXP y, xr;
  int nx = LENGTH(x);
  int nb = INTEGER_VALUE(nbasis);
  int deg = INTEGER_VALUE(degree);
  int d = INTEGER_VALUE(deriv);
  int nk = nb+deg+1;
  double dx, minx, maxx;
  double *knots = NULL;
  double *xdata, *ydata;
  int i;
  if (deg < 0) err("must have degree > 0");
  if (nb <= deg) err("must have nbasis > degree");
  if (d < 0) err("must have deriv >= 0");
  knots = (double *) R_Calloc(nk,double);
  PROTECT(xr = AS_NUMERIC(x));
  PROTECT(y = allocMatrix(REALSXP,nx,nb));
  xdata = REAL(xr);
  ydata = REAL(y);
  for (i = 1, minx = maxx = xdata[0]; i < nx; i++) {
    minx = (minx > xdata[i]) ? xdata[i] : minx;
    maxx = (maxx < xdata[i]) ? xdata[i] : maxx;
  }
  dx = (maxx-minx)/((double) (nb-deg));
  knots[0] = minx-deg*dx;
  for (i = 1; i < nk; i++) knots[i] = knots[i-1]+dx;
  for (i = 0; i < nb; i++) {
    bspline_internal(ydata,xdata,nx,i,deg,d,&knots[0]);
    ydata += nx;
  }
  R_Free(knots);
  UNPROTECT(2);
  return(y);
}

SEXP periodic_bspline_basis (SEXP x, SEXP nbasis, SEXP degree, SEXP period,
  SEXP deriv) {

  SEXP y, xr;
  int nx = LENGTH(x);
  int nb = INTEGER_VALUE(nbasis);
  int deg = INTEGER_VALUE(degree);
  int d = INTEGER_VALUE(deriv);
  double pd = NUMERIC_VALUE(period);
  int j, k;
  double *xrd, *ydata, *val;
  PROTECT(xr = AS_NUMERIC(x));
  xrd = REAL(xr);
  PROTECT(y = allocMatrix(REALSXP,nx,nb));
  ydata = REAL(y);
  val = (double *) R_alloc(nb,sizeof(double));
  for (j = 0; j < nx; j++) {
    periodic_bspline_basis_eval_deriv(xrd[j],pd,deg,nb,d,val);
    for (k = 0; k < nb; k++) ydata[j+nx*k] = val[k];
  }
  UNPROTECT(2);
  return y;
}

void periodic_bspline_basis_eval (double x, double period, int degree,
  int nbasis, double *y) {
  periodic_bspline_basis_eval_deriv(x,period,degree,nbasis,0,y);
}

void periodic_bspline_basis_eval_deriv (double x, double period, int degree,
  int nbasis, int deriv, double *y)
{
  int nknots = nbasis+2*degree+1;
  int shift = (degree-1)/2;
  double *knots = NULL, *yy = NULL;
  double dx;
  int j, k;

  if (period <= 0.0) err("must have period > 0");
  if (nbasis <= 0) err("must have nbasis > 0");
  if (degree < 0) err("must have degree >= 0");
  if (nbasis < degree) err("must have nbasis >= degree");
  if (deriv < 0) err("must have deriv >= 0");

  knots = (double *) R_Calloc(nknots+degree+1,double);
  yy = (double *) R_Calloc(nknots,double);

  dx = period/((double) nbasis);
  for (k = -degree; k <= nbasis+degree; k++) {
    knots[degree+k] = k*dx;
  }
  x = fmod(x,period);
  if (x < 0.0) x += period;
  for (k = 0; k < nknots; k++) {
    bspline_internal(&yy[k],&x,1,k,degree,deriv,knots);
  }
  for (k = 0; k < degree; k++) yy[k] += yy[nbasis+k];
  for (k = 0; k < nbasis; k++) {
    j = (shift+k)%nbasis;
    y[k] = yy[j];
  }
  R_Free(yy); R_Free(knots);
}

// The following function computes the derivative of order d of the i-th
// B-spline of degree p with given knots at each of the nx points in x.
// The results are stored in y.
static void bspline_internal (double *y, const double *x, int nx,
  int i, int p, int d, const double *knots)
{
  int j;
  if (d > p) {
    for (j = 0; j < nx; j++) y[j] = 0.0;
  } else if (d > 0) {
    int i2 = i+1, p2 = p-1, d2 = d-1;
    double *y1 = (double *) R_Calloc(nx,double);
    double *y2 = (double *) R_Calloc(nx,double);
    double a, b;
    bspline_internal(y1,x,nx,i,p2,d2,knots);
    bspline_internal(y2,x,nx,i2,p2,d2,knots);
    for (j = 0; j < nx; j++) {
      a = p / (knots[i+p]-knots[i]);
      b = p / (knots[i2+p]-knots[i2]);
      y[j] = a * y1[j] - b * y2[j];
    }
    R_Free(y1); R_Free(y2);
  } else { // d == 0
    int i2 = i+1, p2 = p-1;
    if (p > 0) {  // case d < p
      double *y1 = (double *) R_Calloc(nx,double);
      double *y2 = (double *) R_Calloc(nx,double);
      double a, b;
      bspline_internal(y1,x,nx,i,p2,d,knots);
      bspline_internal(y2,x,nx,i2,p2,d,knots);
      for (j = 0; j < nx; j++) {
        a = (x[j]-knots[i]) / (knots[i+p]-knots[i]);
        b = (knots[i2+p]-x[j]) / (knots[i2+p]-knots[i2]);
        y[j] = a * y1[j] + b * y2[j];
      }
      R_Free(y1); R_Free(y2);
    } else if (p == 0) { // case d == p
      for (j = 0; j < nx; j++)
        y[j] = (double) ((knots[i] <= x[j]) && (x[j] < knots[i2]));
    }
  }
}
