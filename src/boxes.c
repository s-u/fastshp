#define USE_RINTERNALS
#include <Rinternals.h>

SEXP C_bboxes(SEXP sID, SEXP sX, SEXP sY) {
    SEXP res;
    int *id, ids = 0, i, n, lid, *idv;
    double *x, *y, *A, box[4];
    if (TYPEOF(sID) != INTSXP || TYPEOF(sX) != REALSXP || TYPEOF(sY) != REALSXP)
	Rf_error("invalid id/x/y types");
    n = LENGTH(sID);
    if (LENGTH(sX) != n || LENGTH(sY) != n)
	Rf_error("vector length mismatch between x, y and id");
    if (!n) return R_NilValue;
    lid = id[0];
    ids = 1;
    for (i = 1; i < n; i++)
	if (id[i] != lid) { lid = id[i]; ids++; }
    res = PROTECT(mkNamed(VECSXP, (const char*[]) { "ids", "boxes", "" }));
    idv = INTEGER(SET_VECTOR_ELT(res, 0, allocVector(INTSXP, ids)));
    A   = REAL   (SET_VECTOR_ELT(res, 1, allocMatrix(REALSXP, 4, ids)));
    ids = 0; lid = id[0];
    A[0] = A[2] = x[0];
    A[1] = A[3] = y[0];
    idv[0] = lid;
    for (i = 1; i < n; i++) {
	if (id[i] != lid) {
	    lid = id[i];
	    ids += 4;
	    A[ids]     = A[ids + 2] = x[i];
	    A[ids + 1] = A[ids + 3] = y[i];
	    idv[ids / 4] = lid;
	} else {
	    if (!ISNA(x[i])) {
		if (x[i] > A[ids + 2]) A[ids + 2] = x[i];
		if (x[i] < A[ids]    ) A[ids]     = x[i];
	    }
	    if (!ISNA(y[i])) {
		if (y[i] > A[ids + 3]) A[ids + 3] = y[i];
		if (y[i] < A[ids + 1]) A[ids + 1] = y[i];
	    }
	}
    }
    UNPROTECT(1);
    return res;
}
