#include <string.h>

#include <Rinternals.h>

static void qs(const double *x, const double *y, int *perm, long left, long right)
{
    int i = left, j = right;
    int sp = perm[(i + j) / 2];
    double lx = x[sp], ly = y[sp];
    do {
	while (x[perm[i]] < lx || (x[perm[i]] == lx && y[perm[i]] < ly)) i++;
	while (x[perm[j]] > lx || (x[perm[j]] == lx && y[perm[j]] > ly)) j--;
	if (i <= j) {
	    int ix = perm[i];
	    perm[i] = perm[j];
	    perm[j] = ix;
	    i++;
	    j--;
	}
    } while (i <= j);
    if (left < j) qs(x, y, perm, left, j);
    if (i < right) qs(x, y, perm, i, right);
}


SEXP merge_pts(SEXP X, SEXP Y, SEXP ID) {
    SEXP res;
    double *x = REAL(X), *y = REAL(Y), *ox, *oy;
    int *id = INTEGER(ID), n = LENGTH(X);
    int *oid, *next, *cont;
    int *perm, i, oi, lid, lids, nn;
    char *tag;
    if (TYPEOF(X) != REALSXP || TYPEOF(Y) != REALSXP || TYPEOF(ID) != INTSXP) Rf_error("incompatible vector types");
    if (n != LENGTH(Y) || n != LENGTH(ID)) Rf_error("incompatible vector lengths");
    res  = PROTECT(mkNamed(VECSXP, (const char*[]) { "x", "y", "id", "" }));
    if (n == 0) {
	ox   =    REAL(SET_VECTOR_ELT(res, 0, allocVector(REALSXP, n)));
	oy   =    REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, n)));
	oid  = INTEGER(SET_VECTOR_ELT(res, 2, allocVector(INTSXP, n)));
	UNPROTECT(1);
	return res;
    }
    perm = INTEGER(PROTECT(allocVector(INTSXP, n))); /* permutation */
    next = INTEGER(PROTECT(allocVector(INTSXP, n))); /* next vertex in the segment */
    lid = id[0];
    lids = 0;
    for (i = 0; i < n; i++) {
	perm[i] = i;
	if (lid != id[i]) {
	    lid = id[i];
	    lids = i;
	}
	next[i] = (i < n - 1 && id[i + 1] == lid) ? (i + 1) : lids;
    }

    /* FIXME: do we really need a copy of next to track continuation points?
       Maybe we can re-use next ... need to think about it ... */
    cont = INTEGER(PROTECT(allocVector(INTSXP, n)));
    memcpy(cont, next, sizeof(int) * n);

    /* sort all points so we can find duplicates */
    qs(x, y, perm, 0, n - 1);

    tag = (char*) RAW(PROTECT(allocVector(RAWSXP, n)));
    memset(tag, 0, n);
    /* tags:
       0 = no removal, 1 = remove segment from this point on,
       2 = remove segment to this point, 3 = remove point altogether */
    for (i = 0; i < n - 1; i++) { /* find identical points */
	int k = i + 1; /* count how many there are */
	while (k < n && x[perm[i]] == x[perm[k]] && y[perm[i]] == y[perm[k]])
	    k++;
	if (k > i + 1) { /* go through all combinations */
	    int i0 = i, j;
	    for (; i < k; i++)
		for (j = i0; j < k; j++)
		    if (j != i) {
			int pt1 = perm[i], pt2 = perm[j];
			int next1 = next[pt1], prev2 = pt2 - 1;
			/* we don't have a vector of previous points but this will only fail
			   for the first point, hopefully that's not too much work */
			if (prev2 < 0 || id[prev2] != id[pt2]) { /* find it by iteration */
			    prev2 = pt2;
			    /* the first condition should *never* fire because the last point
			       is always the loop-back point, but just in case ...*/
			    while (prev2 < n && next[prev2] != pt2)
				prev2++;
			}
			/* ok, now check if those share opposite segments */
			if (x[next1] == x[prev2] && y[next1] == y[prev2]) {
			    /* they do! flag the segments (pt1->next1) and (prev2->pt2) */
			    tag[pt1]   |= 1;  tag[prev2] |= 1;
			    tag[next1] |= 2;  tag[pt2]   |= 2;
			    /* mark the continuation points (for points with tag == 1) */
			    cont[pt1] = next[pt2];
			    cont[prev2] = next[next1];
			}
		    }
	    i = k - 1; /* skip the block */
	}
    }

    /* count the number of points that will be left */
    nn = n;
    for (i = 0; i < n; i++) {
	Rprintf("%d ", tag[i]);
	if (tag[i] == 3) nn--;
    }

    /* allocate the result vectors */
    ox   =    REAL(SET_VECTOR_ELT(res, 0, allocVector(REALSXP, nn)));
    oy   =    REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, nn)));
    oid  = INTEGER(SET_VECTOR_ELT(res, 2, allocVector(INTSXP,  nn)));
    oi = 0;
    for (i = 0; i < n; i++)
	if (tag[i] < 3) { /* unprocessed, unremoved ID */
	    int cid = id[i];
	    int j = i, o0 = oi;
	    while (tag[j] < 4) {
		if (tag[j] < 3) { /* tag==3 shoudl not really happen once we're going */
		    if (oi >= nn)
			Rf_error("internal error - attempt to create more than the estimated %d output points", nn);
		    ox[oi] = x[j];
		    oy[oi] = y[j];
		    oid[oi] = cid;
		    oi++;
		}
		{
		    int nj = (tag[j] == 1) ? cont[j] : next[j];
		    tag[j] = 4; /* tag as processed */
		    j = nj;
		}
	    }
	    /* FIXME: how can we get singleton points?!? */
	    if (oi == o0 + 1)
		oi--;
	}
    if (oi < nn) {
	SETLENGTH(VECTOR_ELT(res, 0), oi);
	SETLENGTH(VECTOR_ELT(res, 1), oi);
	SETLENGTH(VECTOR_ELT(res, 2), oi);
    }

    UNPROTECT(5);
    return res;
}
