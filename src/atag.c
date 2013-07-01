/* This is a two-step algorithm used to facilitate simultaneous thinning of shapes
   that share boundaries in a way that preserves the boundaries.

   In the first step a matrix of adjacencies is created for each point,
   i.e. for each point it stores the index of all points that share the same
   location (including the point itself). If the point is unique, nothing
   is stored (unused values are 0).

   In the second step, all polygons are scanned sequentially and all points
   where the adjacency changes are marked as fixed (those cannot be thinned).
   In addition, segments that coincide with any previously processed segments
   are referenced and marked as fixed (so thinning does not need to be performed
   since the thinned points will come from another segment).

   The output is a list of three objects:
   adj:  matrix of adjacencies (points, adj. sequence)
   fix:  logical vector marking fixed points
   ref:  integer vector referencing point replacements (index)

   The first segment encountered in a the sequence will be the one used
   as a reference (i.e., subsequent regments will reference points from the 
   first coinciding segment).

   This algorithm is entriely original, made up by me with no external input,
   and is herewith released to the public domain.

   (C)Copyright 2012 Simon Urbanek.

   This file is licensed under BSD license: open source, no warranties.

*/

#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

static void add(int *where, int what, int width, int n) {
    int i = 1;
    while (i < width && where[i * n] != 0) i++;
    if (i == width) Rf_error("Insufficient width to accomodate adjacencies");
    where[i * n] = what;
}

SEXP C_atag(SEXP xv, SEXP yv, SEXP idv, SEXP ov, SEXP width) {
    int *id = INTEGER(idv), *o = INTEGER(ov), i, n = LENGTH(xv), *m, li, w = asInteger(width);
    double *x = REAL(xv), *y = REAL(yv), lx, ly;
    SEXP res = PROTECT(mkNamed(VECSXP, (const char*[]){ "adj", "fix", "ref", "" }));
    int *fix = LOGICAL(SET_VECTOR_ELT(res, 1, allocVector(LGLSXP, n)));
    int *ref = INTEGER(SET_VECTOR_ELT(res, 2, allocVector(INTSXP, n)));
    m = INTEGER(SET_VECTOR_ELT(res, 0, allocMatrix(INTSXP, n, w)));
    memset(m, 0, n * w * sizeof(*m));
    lx = x[o[0] - 1]; ly = y[o[0] - 1]; li = 0;
    for (i = 1; i < n; i++) {
	if (lx == x[o[i] - 1] && ly == y[o[i] - 1]) {
	    int j = li;
	    if (i - j >= w) Rf_error("Insufficient width to accomodate adjacencies");
	    while (j < i) {
		if (!m[o[j] - 1]) m[o[j] - 1] = o[j];
		add(m + o[j] - 1, o[i], w, n); /* add myself to all */
		m[o[i] - 1 + (j - li) * n] = o[j]; /* add all to myself */
		j++;
	    }
	    m[o[i] - 1 + (i - li) * n] = o[i]; /* add myself to myself */
	} else {
	    lx = x[o[i] - 1]; ly = y[o[i] - 1];
	    li = i;
	}
    }
    /* first point is always a fixpoint */
    ref[0] = 1;
    fix[0] = 1;
    for (i = 1; i < n; i++) {
	int j = 0, same = 1;
	while (j < w) { /* is the adjacency the same as the previous point? */
	    if ((m[i + j * n] == 0 && m[i + j * n - 1] != 0) ||
		(m[i + j * n] != 0 && m[i + j * n - 1] == 0) ||
		(id[m[i + j * n]] != id[m[i + j * n - 1]])) { same = 0; break; };
	    if (!m[i + j * n]) break;
	    j++;
	}
	if (!same) {
	    int fap = m[i + n];
	    if (fap) /* is this a shared point, now shared with someone else? */
		fix[i] = 1; /* fixpoint */
	    else /* our own point (just after a common segment) */
		fix[i] = 0;
	    ref[i] = i + 1; /* no common thread, use our own thinning */
	} else {
	    int fap = m[i + n];
	    if (!fap) { /* our own point (in the middle of our own segment) */
		fix[i] = 0;
		ref[i] = i + 1;
	    } else { /* shared point, same neighbors */
		int mini = i, k = 0; /* let's see if we are the one with the smallest index */
		while (k < w && m[i + k * n]) {
		    if (m[i + k * n] < i) mini = m[i + k * n];
		    k++;
		}
		if (mini == i) { /* smallest index -> we will be used for thinning, treat as a lone segment */
		    fix[i] = 0;
		    ref[i] = i + 1;
		} else { /* dependent index -> point to the first index */
		    fix[i] = 1; /* for efficiency -- this way thin doesn't need to worry about computing those */
		    ref[i] = mini + 1;
		}
	    }
	}
    }
    UNPROTECT(1);
    return res;
}
