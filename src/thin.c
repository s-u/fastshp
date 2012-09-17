#include <string.h>

#define USE_RINTERNALS 1
#include <Rinternals.h>

#define ALG_N2  2

SEXP thin(SEXP xv, SEXP yv, SEXP stol, SEXP lock, SEXP method, SEXP idv) {
    SEXP res;
    int up = 0, n, *keep, i = 0, a = 0, alg = asInteger(method), lps = 0, *id = 0;
    double *x, *y, tol = asReal(stol);
    if (TYPEOF(xv) != REALSXP) {
	xv = PROTECT(coerceVector(xv, REALSXP));
	up++;
    }
    if (TYPEOF(yv) != REALSXP) {
	yv = PROTECT(coerceVector(yv, REALSXP));
	up++;
    }
    x = REAL(xv);
    y = REAL(yv);
    n = LENGTH(xv);
    if (LENGTH(yv) != n) Rf_error("x and y must be of the same length");
    if (TYPEOF(idv) == INTSXP && LENGTH(idv) == n) id = INTEGER(idv);
    if (n < 3) { /* need at least 3 points for thinning */
	res = allocVector(LGLSXP, n);
	while (i < n) LOGICAL(res)[i++] = 1;
	if (up) UNPROTECT(up);
	return res;
    }
    res = PROTECT(allocVector(LGLSXP, n)); up++;
    memset(LOGICAL(res), 0, n * sizeof(LOGICAL(res)[0]));
    keep = LOGICAL(res);
    if (TYPEOF(lock) == REALSXP) {
	lock = PROTECT(coerceVector(lock, INTSXP));
	up++;
    }
    if (TYPEOF(lock) == INTSXP) {
	int nl = LENGTH(lock), *il = INTEGER(lock);
	while (i < nl) {
	    if (il[i] > 0 && il[i] <= n)
		keep[i - 1] = 1;
	    i++;
	}
    } else if (TYPEOF(lock) == LGLSXP) {
	if (LENGTH(lock) != n)
	    Rf_error("Logical lock and the points must be of the same length");
	memcpy(keep, LOGICAL(lock), n * sizeof(LOGICAL(lock)[0]));
    } else if (lock != R_NilValue)
	Rf_error("lock must be logical or numeric vector or NULL");
    /* always keep the first point */
    keep[lps = 0] = 1;
    tol = tol * tol; /* to avoid sqrt() we compare t^2 against tol^2 */
    i = 1;
    while (i < n) {
	if (keep[i])
	    a = i;
	else {
	    double ax = x[i] - x[a], ay = y[i] - y[a];
	    int b_i = (i < n - 1 && !ISNA(x[i + 1]) && (!id || id[i] == id[i - 1])) ? (i + 1) : lps; /* loop over at the end */
	    double bx = x[b_i] - x[a], by = y[b_i] - y[a];
	    double a_b = ax * bx + ay * by; /* dot product */
	    if (a_b > 0.0) { /* we can only thin convex parts */
		double aa = ax * ax + ay * ay; /* |a|^2 */
		double bb = bx * bx + by * by; /* |b|^2 */
		double t = aa - (a_b * a_b) / bb; /* |a|^2 - (a.b / |b|)^2 */
		if (t >= tol) { /* above tolerance, keep */
		    keep[i] = 1;
		    a = i;
		} else if (i - a > 1 && alg == 2) { /* conservative n^2 algorithm - check back on previous points */
		    int j = a + 1;
		    while (j < i) { /* beside us, check all previously removed points as well */
			ax = x[j] - x[a]; ay = y[j] - y[a];
			a_b = ax *bx + ay *by;
			if (a_b > 0.0) {
			    aa = ax * ax + ay * ay;
			    t = aa - (a_b * a_b) / bb;
			    if (t >= tol) {
				keep[i] = 1;
				a = i;
				break;
			    }
			}
			j++;
		    }
		}
	    } else if (a_b == 0.0 && bx == 0.0 && by == 0.0 && (ax != 0.0 || ay != 0.0)) keep[i] = 1; /* line */
	    if (i + 1 < n) {
		if (ISNA(x[i + 1])) {
		    keep[i + 1] = 1;
		    if (lps == i - 1) keep[i] = 1;
		    lps = i + 2;
		    while (lps < n && ISNA(lps)) lps++;
		    if (lps < n) keep[lps] = 1;
		    i = lps;
		} else if (id && id[i + 1] != id[i]) {
		    lps = i + 1;
		    keep[i + 1] = 1;
		    i = lps;
		}
	    }
	}
	i++;
    }
    UNPROTECT(up);
    return res;
}
