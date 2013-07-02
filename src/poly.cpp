#include "clipper.hpp"

#include <R.h>

using namespace ClipperLib;

#define FSCALE 2251799813685248.0

static Polygons *polys_create(int n, int *part, double *x, double *y) {
    Polygons *p = new Polygons();
    Polygon cp;
    int i, last;
    if (part) /* index by part */
	for (i = 0; i < n; i++) {
	    if (i == 0)
		last = part[0];
	    else if (last != part[i]) {
		p->push_back(cp);
		cp.clear();
	    }
	    cp.push_back(IntPoint(x[i] * FSCALE, y[i] * FSCALE));
	}
    else /* NA splits polygons */
	for (i = 0; i < n; i++)
	    if (R_IsNA(x[i])) {
		if (cp.size()) p->push_back(cp);
		cp.clear();
	    } else 
		cp.push_back(IntPoint(x[i] * FSCALE, y[i] * FSCALE));
    if (cp.size())
	p->push_back(cp);
    return p;
}

#define USE_RINTERNALS 1
#include <Rinternals.h>

#define POLY_INT 0  /* intersection */
#define POLY_UNI 1  /* union */
#define POLY_DIF 2  /* difference */
#define POLY_XOR 3

extern "C" SEXP C_poly_op(SEXP p1_part, SEXP p1_x, SEXP p1_y, SEXP p2_part, SEXP p2_x, SEXP p2_y, SEXP sOP, SEXP sOutPars, SEXP sExpand) {
    int pc = 0;
    Polygons resp;
    ClipType cty;
    int op = asInteger(sOP), out_pars = asInteger(sOutPars), expand = asInteger(sExpand);
    switch (op) {
    case POLY_INT: cty = ctIntersection; break;
    case POLY_UNI: cty = ctUnion; break;
    case POLY_DIF: cty = ctDifference; break;
    case POLY_XOR: cty = ctXor; break;
    default: Rf_error("invalid operation");
    }
    if (p1_part != R_NilValue && TYPEOF(p1_part) != INTSXP) {
	if (TYPEOF(p1_part) == REALSXP) {
	    p1_part = PROTECT(coerceVector(p1_part, INTSXP));
	    pc++;
	} else Rf_error("invalid part ID type - must be a numeric vector or NULL");
    }
    if (p2_part != R_NilValue && TYPEOF(p2_part) != INTSXP) {
	if (TYPEOF(p2_part) == REALSXP) {
	    p2_part = PROTECT(coerceVector(p2_part, INTSXP));
	    pc++;
	} else Rf_error("invalid part ID type - must be a numeric vector or NULL");
    }
    if (TYPEOF(p1_x) != REALSXP) {
	p1_x = PROTECT(coerceVector(p1_x, REALSXP));
	pc++;
    }
    if (TYPEOF(p1_y) != REALSXP) {
	p1_y = PROTECT(coerceVector(p1_y, REALSXP));
	pc++;
    }
    if (TYPEOF(p2_x) != REALSXP) {
	p2_x = PROTECT(coerceVector(p2_x, REALSXP));
	pc++;
    }
    if (TYPEOF(p2_y) != REALSXP) {
	p2_y = PROTECT(coerceVector(p2_y, REALSXP));
	pc++;
    }
    if (LENGTH(p1_x) != LENGTH(p1_y) ||
	LENGTH(p2_x) != LENGTH(p2_y))
	Rf_error("vector length mismatch - both x and y must be of the same length");
    Polygons *p1 = polys_create(LENGTH(p1_x),
				(p1_part == R_NilValue) ? 0 : INTEGER(p1_part),
				REAL(p1_x), REAL(p1_y));
    Polygons *p2 = polys_create(LENGTH(p2_x),
				(p2_part == R_NilValue) ? 0 : INTEGER(p2_part),
				REAL(p2_x), REAL(p2_y));
    Clipper c;
    c.AddPolygons(*p1, ptSubject);
    c.AddPolygons(*p2, ptClip);
    c.Execute(cty, resp);
    delete p1;
    delete p2;
    int ps = resp.size(), totn = 0, i, k = 0, *pp = 0, exp_add = 0, *pix = 0;
    SEXP res = PROTECT(mkNamed(VECSXP, (const char*[]) { "part", "x", "y", "" }));
    pc++;
    if (out_pars) {
	SET_STRING_ELT(getAttrib(res, R_NamesSymbol), 0, mkChar("parts"));
	pix = INTEGER(SET_VECTOR_ELT(res, 0, allocVector(INTSXP, ps)));
    }
    for (i = 0; i < ps; i++) {
	if (pix) pix[i] = totn;
	totn += resp[i].size();
    }
    if (!out_pars)
	pp = INTEGER(SET_VECTOR_ELT(res, 0, allocVector(INTSXP, totn)));
    if (expand && ps > 1)
	exp_add = ps - 1;
    double *px = REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, totn + exp_add)));
    double *py = REAL(SET_VECTOR_ELT(res, 2, allocVector(REALSXP, totn + exp_add)));
    for (i = 0; i < ps; i++) {
	Polygon p = resp[i];
	int n = p.size();
	for (int j = 0; j < n; j++) {
	    px[k] = ((double) p[j].X ) / FSCALE;
	    py[k] = ((double) p[j].Y ) / FSCALE;
	    if (pp) pp[k] = i + 1;
	    k++;
	}
	if (expand && i < ps - 1) {
	    px[k] = R_NaReal;
	    py[k++] = R_NaReal;
	}
    }
    UNPROTECT(pc);
    return res;
}
