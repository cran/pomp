#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

#include "pomp.h"

SEXP lookup_in_table (SEXP ttable, SEXP xtable, SEXP t, int *index) {
  int nprotect = 0;
  int flag = 0;
  int *dim, length, width, j, p;
  double x, *xx, *yy, e;
  SEXP X;
  length = LENGTH(ttable);
  dim = INTEGER(GET_DIM(xtable));
  if (length != dim[0]) {
    UNPROTECT(nprotect);
    error("incommensurate dimensions in 'lookup_in_table'");
  }
  width = dim[1];
  x = *(REAL(t));
  xx = REAL(ttable);
  yy = REAL(xtable);
  PROTECT(X = NEW_NUMERIC(width)); nprotect++;
  SET_NAMES(X,GET_COLNAMES(GET_DIMNAMES(xtable)));
  *index = findInterval(REAL(ttable),length,x,TRUE,TRUE,*index,&flag);
  if (flag != 0)
    warning("table_lookup: extrapolating (flag %d) at %le",flag,*(REAL(t)));
  e = (x-xx[*index-1])/(xx[*index]-xx[*index-1]);
  xx = REAL(X);
  for (j = 0; j < width; j++) {
    p = *index+j*length;
    xx[j] = e*yy[p]+(1-e)*yy[p-1];
  }
  UNPROTECT(nprotect);
  return X;
}

// linear interpolation on a lookup table
void table_lookup (struct lookup_table *tab, double x, double *y, double *dydt)
{
  int flag = 0;
  int j, k;
  double e;
  tab->index = findInterval(tab->x,tab->length,x,TRUE,TRUE,tab->index,&flag);
  if (flag != 0)              // we are extrapolating
    warning("table_lookup: extrapolating (flag %d) at %le", flag, x);
  e = (x - tab->x[tab->index-1]) / (tab->x[tab->index] - tab->x[tab->index-1]);
  for (j = 0; j < tab->width; j++) {
    k = j*(tab->length)+(tab->index);
    y[j] = e*(tab->y[k])+(1-e)*(tab->y[k-1]);
    if (dydt != 0)
      dydt[j] = ((tab->y[k])-(tab->y[k-1]))/((tab->x[tab->index])-(tab->x[tab->index-1]));
  }
}

// compute the transmission coefficient using the basis functions
double dot_product (int dim, const double *basis, const double *coef)
{
  int j;
  double trans = 0.0;
  for (j = 0; j < dim; j++)
    trans += coef[j]*basis[j];
  return(trans);
}
