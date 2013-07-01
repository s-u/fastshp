#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

SEXP C_expand_vec(SEXP sV, SEXP sPart) {
    int n = LENGTH(sV), pc = 0;
    int *part  = INTEGER(sPart);
    if (TYPEOF(sPart) != INTSXP || LENGTH(sPart) == 0) Rf_error("invalid parts vector");
    if (TYPEOF(sV) != REALSXP) Rf_error("invalid vector");
    if (*part != 0) { /* full part list (>0 part numbers) -> create index */
	int i, nparts = 1, lp, *nii;
	if (LENGTH(sPart) < 2) return sV;
	lp = part[0];
	for (i = 1; i < n; i++) /* pass 1: count the parts */
	    if (lp != part[i]) {
		nparts++;
		lp = part[i];
	    }
	if (nparts == 1) return sV;
	nii = INTEGER(sPart = PROTECT(allocVector(INTSXP, nparts)));
	pc++;
	*(nii++) = 0;
	lp = part[0];
	for (i = 1; i < n; i++) /* pass 2: create index */
	    if (lp != part[i]) {
		*(nii++) = i;
		lp = part[i];
	    }
	part = nii;
    }
    {
	SEXP nv;
	double *v  = REAL(sV), *tv;
	int nparts = LENGTH(sPart), i0 = 1;
	if (nparts == 1) return sV;
	tv = REAL(nv = allocVector(REALSXP, n + nparts - 1));
	memcpy(tv, v, sizeof(double) * part[i0]);
	tv += part[i0];
	while (++i0 < nparts) {
	    int eop = (i0 < nparts) ? part[i0] : n;
	    int bop = part[i0 - 1];
	    *(tv++) = R_NaReal;
	    memcpy(tv, v + bop, sizeof(double) * (eop - bop));
	    tv += eop - bop;
	}
	if (pc) UNPROTECT(pc);
	return nv;
    }
}
