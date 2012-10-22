#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

SEXP shp_inside(SEXP slist, SEXP pxv, SEXP pyv, SEXP clockw) {
    SEXP xv, yv, pv, res;
    double *x, *y, *px, *py;
    int *p, up = 0, *r, np, ns, mp = 0, i,
      expected = (asInteger(clockw) == TRUE) ? 1 : -1;
    if (TYPEOF(slist) != VECSXP || !inherits(slist, "shp"))
	Rf_error("input must be a list of shapes (shp object)");
    if (LENGTH(slist) == 0)
	return allocVector(INTSXP, 0);
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
    res = allocVector(INTSXP, np);
    r = INTEGER(res);
    memset(r, 0, sizeof(*r) * np);
    ns = LENGTH(slist);
    for (i = 0; i < ns; i++) {
	int j;
	double *bb;
	SEXP shp = VECTOR_ELT(slist, i);
	bb = REAL(VECTOR_ELT(shp, 2));
	pv = VECTOR_ELT(shp, 3); p = INTEGER(pv);
	xv = VECTOR_ELT(shp, 4); x = REAL(xv);
	yv = VECTOR_ELT(shp, 5); y = REAL(yv);
	for (j = 0; j < np; j++) {
	    double X = px[j], Y = py[j];
	    /* is the point inside the bounding box? */
	    if (X >= bb[0] && X <= bb[2] && Y >= bb[1] && Y <= bb[3]) {
		/* then find crossing index */
		int ci = 0, k = 0, ls = 0, snp = LENGTH(xv);
		while (k < snp) {
		    int k2 = (k + 1 >= snp || ISNA(x[k + 1])) ? ls : (k + 1);
		    int cd = 0;
		    if (y[k] <= Y && y[k2] > Y)
			cd = -1;
		    else if (y[k] >= Y && y[k2] < Y)
			cd = 1;
		    if (cd) {
			/* head right from the point and see if k->k2 intersects */
			/* to avoid intersect computation, first check if we don't need to since both points are beyond */
			if (x[k2] > X && x[k] > X)
			    ci += cd;
			else if (!(x[k2] < X && x[k] < X)) { /* only if there is doubt, compute the intersecting point */
			    /* for numerical stability pick the longer fraction */
			    double dy1 = y[k] - Y, dy2 = y[k2] - Y, dx;
			    if ((cd == 1 && dy2 > dy1) || (cd == -1 && dy2 < dy1))
				dx = x[k2] - (x[k2] - x[k] ) * (y[k2] - y[k]) / (y[k2] - Y) - X;
			    else
				dx = x[k]  - (x[k]  - x[k2]) * (y[k] - y[k2]) / (y[k]  - Y) - X;
			    if (dx > 0.0)
				ci += cd;
			    /* Rprintf("%d[%d,%d]: cd = %d, intersect at %g (ci = %d after)\n", i+1, k, k2, cd, dx, ci); */
			}
		    }
		    k++;
		    if (ISNA(x[k])) { /* end of polygon -> check now and reset ci for the next polygon */
			ls = ++k;
			if (ci == expected && !r[j]) {
			    mp++;
			    r[j] = i + 1;
			    if (mp >= np) { /* if all points got matched, get out */
				i = ns;
				break;
			    }
			}
			ci = 0;
		    }
		}
		if (ci == expected && !r[j]) {
		    mp++;
		    r[j] = i + 1;
		    if (mp >= np) { /* if all points got matched, get out */
			i = ns;
			break;
		    }
		}
	    }
	}
    }
    if (mp < np) /* replace 0 (no match) with NA */
	for (i = 0; i < np; i++)
	    if (r[i] == 0) r[i] = NA_INTEGER;
    if (up) UNPROTECT(up);
    return res;
}
