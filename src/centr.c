#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

SEXP shp_centroids(SEXP slist) {
    SEXP res, rn;
    double *cx, *cy, *a;
    int i, ns;
    if (TYPEOF(slist) != VECSXP || !inherits(slist, "shp"))
	Rf_error("input must be a list of shapes (shp object)");
    ns = LENGTH(slist);
    res = PROTECT(mkNamed(VECSXP,
			  (const char *[]) { "cx", "cy", "area", "" }));
    cx = REAL(SET_VECTOR_ELT(res, 0, allocVector(REALSXP, ns)));
    cy = REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, ns)));
    a = REAL(SET_VECTOR_ELT(res, 2, allocVector(REALSXP, ns)));    
    for (i = 0; i < ns; i++) {
	int *pp, np, j;
	double *px, *py,  X = 0, Y = 0, A = 0;
	SEXP shp = VECTOR_ELT(slist, i), pv = VECTOR_ELT(shp, 3);
	if (LENGTH(pv) > 1) {
	    Rf_warning("shape[%d] has more than one part, using only the first part", i + 1);
	    np = INTEGER(pv)[1];
	}
	px = REAL(VECTOR_ELT(shp, 4));
	py = REAL(VECTOR_ELT(shp, 5));
	np = LENGTH(VECTOR_ELT(shp, 4));
	for (j = 0; j < np; j++) {
	    double x_i = px[j], y_i = py[j];
	    int i1 = (j == np - 1) ? 0 : (j + 1);
	    double x_i1 = px[i1], y_i1 = py[i1];
	    double q = x_i * y_i1 - x_i1 * y_i;
	    A += q;
	    X += (x_i + x_i1) * q;
	    Y += (y_i + y_i1) * q;
	}
	A *= 0.5;
	if (A != 0) {
	    X *= 1 / (6 * A);
	    Y *= 1 / (6 * A);
	} else {
	  X = NA_REAL;
	  Y = NA_REAL;
	}
	a[i]  = A;
	cx[i] = X;
	cy[i] = Y;
    }
    rn = allocVector(INTSXP, 2);
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = -ns;
    setAttrib(res, R_RowNamesSymbol, rn);
    setAttrib(res, R_ClassSymbol, mkString("data.frame"));
    UNPROTECT(1);
    return res;
}
