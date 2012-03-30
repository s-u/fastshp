#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

#define E_LITTLE 1
#define E_BIG    0

static int native_endian;

static int int_val(const void *ptr, int endianness) {
    union { int i; unsigned char c[4]; } buf;
    const unsigned char *src = (const unsigned char*) ptr;
    if (endianness == native_endian)
	memcpy(&buf.i, ptr, 4);
    else {
	buf.c[0] = src[3];
	buf.c[1] = src[2];
	buf.c[2] = src[1];
	buf.c[3] = src[0];
    }
    return buf.i;
}

static double double_val(const void *ptr, int endianness) {
    union { double d; unsigned char c[8]; } buf;
    const unsigned char *src = (const unsigned char*) ptr;
    if (endianness == native_endian)
	memcpy(&buf.d, ptr, 8);
    else {
	buf.c[0] = src[7];
	buf.c[1] = src[6];
	buf.c[2] = src[5];
	buf.c[3] = src[4];
	buf.c[4] = src[3];
	buf.c[5] = src[2];
	buf.c[6] = src[1];
	buf.c[7] = src[0];
    }
    return buf.d;
}

typedef struct point {
    double x, y;
} point_t;

typedef struct rect {
    double x1, y1, x2, y2;
} rect_t;

#if 0 /* currently unused */
static point_t point_val(const void *ptr) {
    point_t p;
    const char *buf = (const char*) ptr;
    p.x = double_val(buf, E_LITTLE);
    p.y = double_val(buf + 8, E_LITTLE);
    return p;
}
#endif

static rect_t rect_val(const void *ptr) {
    rect_t p;
    const char *buf = (const char*) ptr;
    p.x1 = double_val(buf, E_LITTLE);
    p.y1 = double_val(buf + 8, E_LITTLE);
    p.x2 = double_val(buf + 16, E_LITTLE);
    p.y2 = double_val(buf + 24, E_LITTLE);
    return p;
}

static SEXP rbox(rect_t box) {
    SEXP v = allocVector(REALSXP, 4);
    memcpy(REAL(v), (const double*)&box, 4 * 8);
    return v;
}

static SEXP rintv(int n, const char *ptr) {
    SEXP v = allocVector(INTSXP, n);
    int *di = INTEGER(v);
    int j;
    if (native_endian == E_LITTLE)
	memcpy(di, ptr, 4 * n);
    else
	for (j = 0; j < n; j++) di[j] = int_val(ptr + 4 * j, E_LITTLE);
    return v;
}

SEXP read_shp(SEXP what, SEXP format) {
    FILE *f;
    char buf[128];
    int i, n, shp_type, ftype = asInteger(format);
    rect_t bbox;
    long flen, pos = 100;
    SEXP root = 0, tail = 0, names;
    char *lbuf;
    int lbuf_size = 65536; /* # of points in the large points buffer */
    static const char *nam[] = { "id", "type", "box", "parts", "x", "y" };

    if (TYPEOF(what) != STRSXP || LENGTH(what) != 1)
	Rf_error("source must be a file name");
    f = fopen(CHAR(STRING_ELT(what, 0)), "rb");
    if (!f)
	Rf_error("cannot open '%s'", CHAR(STRING_ELT(what, 0)));
    n = fread(buf, 1, 100, f);
    if (n < 100) {
	fclose(f);
	Rf_error("read error while reading the file header (corrupted file?)");
    }
    i = 1;
    native_endian = (*((char*)&i) == 1) ? E_LITTLE : E_BIG;
    if (int_val(buf, E_BIG) != 0x270a) {
	fclose(f);
	Rf_error("invalid file (expected type 0x%x, found 0x%x)", 0x270a, int_val(buf, E_BIG));
    }
    shp_type = int_val(buf + 32, E_LITTLE);
    bbox = rect_val(buf + 36);
    Rprintf("type=%d, bbox = { %g, %g, %g, %g }\n", shp_type, bbox.x1, bbox.y1, bbox.x2, bbox.y2);
    flen = int_val(buf + 24, E_BIG);
    flen <<= 1;
    lbuf = R_alloc(lbuf_size, 16);
    names = PROTECT(allocVector(STRSXP, 6));
    for (i = 0; i < 6; i++) SET_STRING_ELT(names, i, mkChar(nam[i]));
    while (!feof(f) && pos < flen) {
	int rec, len, sty = 0;
	if ((n = fread(buf, 1, 12, f)) < 8) {
	    Rf_warning("Read error at position %ld, returning result so far", pos);
	    break;
	}
	rec = int_val(buf, E_BIG);
	len = int_val(buf + 4, E_BIG);
	if (n == 12) sty = int_val(buf + 8, E_LITTLE);
	if (len < 0 || sty < 0 || sty > 256) {
	    fclose(f);
	    Rf_error("invalid chunk at %ld (type=%d, length/2=%d), aborting", pos, sty, len);
	}
	len <<= 1;
	/* printf("%d) [%d] %d bytes\n", rec, sty, len); */
	pos += 8;
	if (sty == 3 || sty ==5) { /* poly{gon|line} */
	    rect_t box;
	    int npart, npts;
	    SEXP sx, sy;
	    double *xx, *yy;
	    SEXP v = allocVector(VECSXP, 6);
	    if (!root)
		PROTECT(root = tail = list1(v));
	    else {
		SEXP nl = list1(v);
		SETCDR(tail, nl);
		tail = nl;
	    }
	    setAttrib(v, R_NamesSymbol, names);
	    if ((n = fread(buf + 12, 1, 40, f)) != 40) {
		Rf_warning("Read error at position %ld (id=%d, type=%d), returning result so far", pos, rec, sty);
		break;
	    }
	    box = rect_val(buf + 12);
	    npart = int_val(buf + 44, E_LITTLE);
	    npts = int_val(buf + 48, E_LITTLE);
	    /* Rprintf("  parts: %d, pts: %d\n", npart, npts); */
	    SET_VECTOR_ELT(v, 0, ScalarInteger(rec));
	    SET_VECTOR_ELT(v, 1, ScalarInteger(sty));
	    SET_VECTOR_ELT(v, 2, rbox(box));
	    if (npts <= lbuf_size) {
		while (lbuf_size <= npts) lbuf_size <<= 1;
		lbuf = R_alloc(lbuf_size, 16);
	    }
	    if (fread(lbuf, 4, npart, f) != npart) {
		Rf_warning("Read error at position %ld (id=%d, type=%d), returning result so far", pos, rec, sty);
		break;
	    }
	    SET_VECTOR_ELT(v, 3, rintv(npart, lbuf));
	    if ((n = fread(lbuf, 16, npts, f)) != npts) {
		Rf_warning("Read error (%d != %d) at position %ld (id=%d, type=%d), returning result so far", n, npts, pos, rec, sty);
		break;
	    }
	    SET_VECTOR_ELT(v, 4, sx = allocVector(REALSXP, npts));
	    SET_VECTOR_ELT(v, 5, sy = allocVector(REALSXP, npts));
	    xx = REAL(sx);
	    yy = REAL(sy);
	    for (i = 0; i < npts; i++) {
		xx[i] = double_val(lbuf + i * 16, E_LITTLE);
		yy[i] = double_val(lbuf + i * 16 + 8, E_LITTLE);
	    }
	}
	pos += len;
	fseek(f, pos, SEEK_SET); /* skip unknown records */
    }
    fclose(f);
    if (ftype == 1) {
	SEXP res, l;
	i = 0;
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    i++;
	    l = CDR(l);
	}
	res = PROTECT(allocVector(VECSXP, i));
	i = 0;
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    SET_VECTOR_ELT(res, i, CAR(l));
	    i++;
	    l = CDR(l);
	}
	UNPROTECT(root ? 3 : 2);
	return res;
    }
    if (ftype == 2) {
	SEXP res, l;
	i = 0;
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    i++;
	    l = CDR(l);
	}
	res = PROTECT(allocVector(VECSXP, i));
	i = 0;
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    SEXP e = CAR(l);
	    int nparts = LENGTH(VECTOR_ELT(e, 3));
	    if (nparts > 1) { /* have to split up the polygons */
		int npts = LENGTH(VECTOR_ELT(e, 4));
		int *pt = INTEGER(VECTOR_ELT(e, 3));
		double *xp = REAL(VECTOR_ELT(e, 4));
		double *yp = REAL(VECTOR_ELT(e, 5));
		SEXP nxv = PROTECT(allocVector(REALSXP, npts + nparts - 1));
		SEXP nyv = PROTECT(allocVector(REALSXP, npts + nparts - 1));
		double *tx = REAL(nxv);
		double *ty = REAL(nyv);
		int cp = 0;
		while (cp < nparts) {
		    int p0 = pt[cp], p1 = (cp + 1 < nparts) ? pt[cp + 1] : npts;
		    memcpy(tx, xp + p0, (p1 - p0) * sizeof(double));
		    memcpy(ty, yp + p0, (p1 - p0) * sizeof(double));
		    tx += (p1 - p0) + 1;
		    ty += (p1 - p0) + 1;
		    cp++;
		}
		SET_VECTOR_ELT(e, 4, nxv);
		SET_VECTOR_ELT(e, 5, nyv);
		UNPROTECT(2);
	    }
	    SET_VECTOR_ELT(res, i, e);
	    i++;
	    l = CDR(l);
	}
	UNPROTECT(root ? 3 : 2);
	return res;
    }	
    if (ftype == 3) {
	int nshp = 0, npts = 0, pt = 0;
	SEXP res = PROTECT(allocVector(VECSXP, 6)), l, xv, yv, idv, tyv, pv;
	double *tx, *ty;
	int *tp, *ti, *tt;
	setAttrib(res, R_NamesSymbol, names);
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    nshp++;
	    npts += LENGTH(VECTOR_ELT(CAR(l), 4));
	    l = CDR(l);
	}
	SET_VECTOR_ELT(res, 0, idv = allocVector(INTSXP, npts));
	SET_VECTOR_ELT(res, 1, tyv = allocVector(INTSXP, npts));
	/* SET_VECTOR_ELT(res, 2, xv = allocVector(REALSXP, npts));*/
	SET_VECTOR_ELT(res, 3, pv = allocVector(INTSXP, npts));
	SET_VECTOR_ELT(res, 4, xv = allocVector(REALSXP, npts));
	SET_VECTOR_ELT(res, 5, yv = allocVector(REALSXP, npts));
	tx = REAL(xv);
	ty = REAL(yv);
	tp = INTEGER(pv);
	ti = INTEGER(idv);
	tt = INTEGER(tyv);
	if (!root) {
	    UNPROTECT(2);
	    return res;
	}
	l = root;
	while (l != R_NilValue) {
	    SEXP e = CAR(l);
	    int rec = INTEGER(VECTOR_ELT(e, 0))[0];
	    int sty = INTEGER(VECTOR_ELT(e, 1))[0];
	    int nparts = LENGTH(VECTOR_ELT(e, 3));
	    const int *parts = INTEGER(VECTOR_ELT(e, 3));
	    int j = 0, np = LENGTH(VECTOR_ELT(e, 4));
	    double *xp = REAL(VECTOR_ELT(e, 4));
	    double *yp = REAL(VECTOR_ELT(e, 5));
	    int cp = 1, nxp = (nparts > 1) ? parts[cp] : np;
	    memcpy(tx + pt, xp, 8 * np);
	    memcpy(ty + pt, yp, 8 * np);
	    for (j = 0; j < np; j++) {
		if (j == nxp) {
		    cp++;
		    nxp = (cp >= nparts) ? np : parts[cp];
		}
		tp[j] = cp;
		ti[j] = rec;
		tt[j] = sty;
	    }
	    l = CDR(l);
	}
	UNPROTECT(root ? 3 : 2);
	return res;
    }
    UNPROTECT(root ? 2 : 1);
    return root ? root : R_NilValue;
    // return R_NilValue;
}
