#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS
#include <Rinternals.h>

#include "io.h"

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

SEXP read_shp(SEXP what, SEXP format, SEXP do_close) {
    io_t *io;
    char buf[128];
    int i, n, shp_type, ftype = asInteger(format), close_c = asInteger(do_close);
    rect_t bbox;
    long flen, pos = 100;
    SEXP root = 0, tail = 0, names;
    char *lbuf;
    int lbuf_size = 65536; /* # of points in the large points buffer */
    static const char *nam[] = { "id", "type", "box", "parts", "x", "y" };

    if (TYPEOF(what) == RAWSXP)
	io = io_open_raw(what);
    else if (TYPEOF(what) == INTSXP && inherits(what, "connection"))
	io = io_open_conn(what, close_c);
    else {
	if (TYPEOF(what) != STRSXP || LENGTH(what) != 1)
	    Rf_error("source must be a file name, connection or a raw vector");
	io = io_open_file(CHAR(STRING_ELT(what, 0)), "rb");
	if (!io)
	    Rf_error("cannot open '%s'", CHAR(STRING_ELT(what, 0)));
    }
    n = io_read(io, buf, 1, 100);
    if (n < 100) {
	io_close(io);
	Rf_error("read error while reading the file header (corrupted file?)");
    }
    i = 1;
    native_endian = (*((char*)&i) == 1) ? E_LITTLE : E_BIG;
    if (int_val(buf, E_BIG) != 0x270a) {
	io_close(io);
	Rf_error("invalid file (expected type 0x%x, found 0x%x)", 0x270a, int_val(buf, E_BIG));
    }
    shp_type = int_val(buf + 32, E_LITTLE);
    bbox = rect_val(buf + 36);
    /* Rprintf("type=%d, bbox = { %g, %g, %g, %g }\n", shp_type, bbox.x1, bbox.y1, bbox.x2, bbox.y2); */
    flen = int_val(buf + 24, E_BIG);
    flen <<= 1;
    lbuf = R_alloc(lbuf_size, 16);
    names = PROTECT(allocVector(STRSXP, 6));
    for (i = 0; i < 6; i++) SET_STRING_ELT(names, i, mkChar(nam[i]));
    while (!io_eof(io) && pos < flen) {
	int rec, len, sty = 0;
	if ((n = io_read(io, buf, 1, 12)) < 8) {
	    Rf_warning("Read error at position %ld, returning result so far", pos);
	    break;
	}
	rec = int_val(buf, E_BIG);
	len = int_val(buf + 4, E_BIG);
	if (n == 12) sty = int_val(buf + 8, E_LITTLE);
	if (len < 0 || sty < 0 || sty > 256) {
	    io_close(io);
	    Rf_error("invalid chunk at %ld (type=%d, length/2=%d), aborting", pos, sty, len);
	}
	len <<= 1;
	/* printf("%d) [%d] %d bytes\n", rec, sty, len); */
	pos += 8;
	if (sty == 3 || sty == 5 || sty == 13 || sty == 15) { /* poly{gon|line}; 1x are Z-versions */
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
	    if ((n = io_read(io, buf + 12, 1, 40)) != 40) {
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
	    if (npts >= lbuf_size) {
		while (lbuf_size <= npts) lbuf_size <<= 1;
		lbuf = R_alloc(lbuf_size, 16);
	    }
	    if (io_read(io, lbuf, 4, npart) != npart) {
		Rf_warning("Read error at position %ld (id=%d, type=%d), returning result so far", pos, rec, sty);
		break;
	    }
	    SET_VECTOR_ELT(v, 3, rintv(npart, lbuf));
	    if ((n = io_read(io, lbuf, 16, npts)) != npts) {
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
	    pos += len;
	    io_seek(io, pos); /* skip Z range, Z array and optional M range + M array */
	} else if (sty == 8) { /* multipoint (MBR, # pts, points) */
	    rect_t box;
	    int npts;
	    double *vv;
	    SEXP v;
	    if (len < 36) {
		Rf_warning("Invalid record (length %d is too small for the type 8 [points] at position %d), returning results so far", len, pos);
		break;
	    }
	    if ((n = io_read(io, buf + 12, 1, 36)) != 36) {
		Rf_warning("Read error at position %ld (id=%d, type=%d), returning result so far", pos, rec, sty);
		break;
	    }
	    box = rect_val(buf + 12);
	    npts = int_val(buf + 44, E_LITTLE);
	    if (npts < 0) {
		Rf_warning("Invalid number of points %d at position %d, returning results so far", npts, pos);
		break;
	    }
	    if (npts >= lbuf_size) {
		while (lbuf_size <= npts) lbuf_size <<= 1;
		lbuf = R_alloc(lbuf_size, 16);
	    }
	    if (io_read(io, lbuf, 16, npts) != npts) {
		Rf_warning("Read error at position %ld (id=%d, type=%d), returning result so far", pos, rec, sty);
		break;
	    }
	    
	    /* Rprintf("  pts: %d\n", npts); */
	    v = allocMatrix(REALSXP, npts, 2);
	    if (!root)
		PROTECT(root = tail = list1(v));
	    else {
		SEXP nl = list1(v);
		SETCDR(tail, nl);
		tail = nl;
	    }
	    pos += len;
	    vv = REAL(v);
	    for (i = 0; i < npts; i++) {
		vv[i] = double_val(lbuf + i * 16, E_LITTLE);
		vv[i + npts] = double_val(lbuf + i * 16 + 8, E_LITTLE);
	    }
	    pos += len;
	} else {
	    /* other: 1 = point (X,Y) */
	    pos += len;
	    io_seek(io, pos); /* skip unknown records */
	}
    }
    io_close(io);
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
	{
	    SEXP bbs = install("bbox");
	    SEXP bbv = allocVector(REALSXP, 4);
	    memcpy(REAL(bbv), &bbox, sizeof(bbox));
	    setAttrib(res, bbs, bbv);
	    setAttrib(res, R_ClassSymbol, mkString("shp"));
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
		    tx += (p1 - p0);
		    ty += (p1 - p0);
		    if (p1 < npts) {
		      *(tx++) = NA_REAL;
		      *(ty++) = NA_REAL;
		    }
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
	{
	    SEXP bbs = install("bbox");
	    SEXP bbv = allocVector(REALSXP, 4);
	    memcpy(REAL(bbv), &bbox, sizeof(bbox));
	    setAttrib(res, bbs, bbv);
	    setAttrib(res, R_ClassSymbol, mkString("shp"));
	}
	UNPROTECT(root ? 3 : 2);
	return res;
    }	
    if (ftype == 3) {
	int nshp = 0, npts = 0;
	SEXP res = PROTECT(mkNamed(VECSXP,
				   (const char *[]) { "id", "type", "part", "x", "y", "" }
				   )),
	  l, xv, yv, idv, tyv, pv;
	double *tx, *ty;
	int *tp, *ti, *tt;
	
	l = root ? root : R_NilValue;
	while (l != R_NilValue) {
	    nshp++;
	    npts += LENGTH(VECTOR_ELT(CAR(l), 4));
	    l = CDR(l);
	}
	SET_VECTOR_ELT(res, 0, idv = allocVector(INTSXP, npts));
	SET_VECTOR_ELT(res, 1, tyv = allocVector(INTSXP, npts));
	SET_VECTOR_ELT(res, 2, pv = allocVector(INTSXP, npts));
	SET_VECTOR_ELT(res, 3, xv = allocVector(REALSXP, npts));
	SET_VECTOR_ELT(res, 4, yv = allocVector(REALSXP, npts));
	tx = REAL(xv);
	ty = REAL(yv);
	tp = INTEGER(pv);
	ti = INTEGER(idv);
	tt = INTEGER(tyv);
	{
	    SEXP rn = allocVector(INTSXP, 2);
	    INTEGER(rn)[0] = NA_INTEGER;
	    INTEGER(rn)[1] = -npts;
	    setAttrib(res, R_RowNamesSymbol, rn);
	    setAttrib(res, R_ClassSymbol, mkString("data.frame"));
	}	    
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
	    memcpy(tx, xp, 8 * np);
	    memcpy(ty, yp, 8 * np);
	    for (j = 0; j < np; j++) {
		if (j == nxp) {
		    cp++;
		    nxp = (cp >= nparts) ? np : parts[cp];
		}
		tp[j] = cp;
		ti[j] = rec;
		tt[j] = sty;
	    }
	    tx += np;
	    ty += np;
	    tp += np;
	    ti += np;
	    tt += np;
	    l = CDR(l);
	}
	UNPROTECT(root ? 3 : 2);
	return res;
    }
    UNPROTECT(root ? 2 : 1);
    return root ? root : R_NilValue;
}
