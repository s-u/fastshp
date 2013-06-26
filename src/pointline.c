#define USE_RINTERNALS 1
#include <Rinternals.h>

#define pow2(X) ((X) * (X))
#define dist2(VX, VY, WX, WY) (pow2((VX) - (WX)) + pow2((VY) - (WY)))

/* dist^2 from a point to a line (finite) */
static double ptdist2(double px, double py, double vx, double vy, double wx, double wy) {
    double l2 = dist2(vx, vy, wx, wy), r;
    if (l2 == 0.0) return dist2(px, py, vx, vy);
    r = ((px - vx) * (wx - vx) + (py - vy) * (wy - vy)) / l2;
    if (r < 0) return dist2(px, py, vx, vy);
    if (r > 1) return dist2(px, py, wx, wy);
    return dist2(px, py, vx + r * (wx - vx), vy + r * (wy - vy));
}

SEXP shp_closest(SEXP slist, SEXP pxv, SEXP pyv) {
    SEXP xv = 0, yv = 0; /* to appease dumb compilers */
    SEXP res;
    double *x, *y, *px, *py, *dist;
    int up = 0, *r, np, ns, i, is_shp = 0;
    if (TYPEOF(slist) != VECSXP)
	Rf_error("input must be a list of shapes (shp object) or coorditate pairs");
    is_shp = inherits(slist, "shp");
    res = PROTECT(mkNamed(VECSXP, (const char *[]) { "closest", "dist" }));
    if (LENGTH(slist) == 0) {
	SET_VECTOR_ELT(res, 0, allocVector(INTSXP, 0));
	SET_VECTOR_ELT(res, 1, allocVector(REALSXP, 0));
	UNPROTECT(1);
	return res;
    }
    if (LENGTH(pxv) != LENGTH(pyv))
	Rf_error("point coordinates must have the same length");
    if (TYPEOF(pxv) != REALSXP) {
	pxv = PROTECT(coerceVector(pxv, REALSXP)); up++;
    }
    if (TYPEOF(pyv) != REALSXP) {
	pyv = PROTECT(coerceVector(pyv, REALSXP)); up++;
    }
    px = REAL(pxv);
    py = REAL(pyv);
    np = LENGTH(pxv);
    r = INTEGER(SET_VECTOR_ELT(res, 0, allocVector(INTSXP, np)));
    dist = REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, np)));
    ns = LENGTH(slist);
    for (i = 0; i < np; i++) {
	double md;
	int which, first = 1, j;
	double X = px[i], Y = py[i];
	for (j = 0; j < ns; j++) {
	    int k = 0, ls = 0, snp;
	    SEXP shp = VECTOR_ELT(slist, i);
	    if (is_shp) {
		xv = VECTOR_ELT(shp, 4);
		yv = VECTOR_ELT(shp, 5);
	    } else { 
		if (shp == R_NilValue)
		    continue;
		if (TYPEOF(shp) != VECSXP || LENGTH(shp) < 2 ||
		    TYPEOF((xv = VECTOR_ELT(shp, 0))) != REALSXP ||
		    TYPEOF((yv = VECTOR_ELT(shp, 1))) != REALSXP)
		    Rf_error("list element %d is not a valid coordinate pair", j + 1);
	    }
	    y = REAL(yv);
	    x = REAL(xv);
	    snp = LENGTH(xv);
	    while (k < snp) {
		int k2 = (k + 1 >= snp || ISNA(x[k + 1])) ? ls : (k + 1);
		double d = ptdist2(X, Y, x[k], y[k], x[k2], y[k2]);
		if (first) {
		    md = d;
		    which = j;
		    first = 0;
		} else if (d < md) {
		    md = d;
		    which = j;
		}
		k++;
		if (ISNA(x[k]))
		    ls = ++k;
	    }
	}
	if (first) {
	    r[i] = NA_INTEGER;
	    dist[i] = NA_REAL;
	} else {
	    r[i] = which + 1;
	    dist[i] = sqrt(md);
	}
    }
    UNPROTECT(up + 1);
    return res;
}

