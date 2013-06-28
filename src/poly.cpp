#include "clipper.hpp"

using namespace ClipperLib;

#define FSCALE 2251799813685248.0

static Polygons *polys_create(int n, int *part, double *x, double *y) {
  Polygons *p = new Polygons();
  Polygon cp;
  int i, last;
  for (i = 0; i < n; i++) {
    if (i == 0)
      last = part[0];
    else if (last != part[i]) {
      p->push_back(cp);
      cp.clear();
    }
    cp.push_back(IntPoint(x[i] * FSCALE, y[i] * FSCALE));
  }
  if (cp.size())
    p->push_back(cp);
  return p;
}

#define USE_RINTERNALS 1
#include <Rinternals.h>

extern "C" SEXP C_poly_intersect(SEXP p1_part, SEXP p1_x, SEXP p1_y, SEXP p2_part, SEXP p2_x, SEXP p2_y) {
  Polygons resp;
  Polygons *p1 = polys_create(LENGTH(p1_x),
			      INTEGER(p1_part),
			      REAL(p1_x), REAL(p1_y));
  Polygons *p2 = polys_create(LENGTH(p2_x),
			      INTEGER(p2_part),
			      REAL(p2_x), REAL(p2_y));
  Clipper c;
  c.AddPolygons(*p1, ptSubject);
  c.AddPolygons(*p2, ptClip);
  c.Execute(ctIntersection, resp);
  delete p1;
  delete p2;
  int ps = resp.size(), totn = 0, i, k = 0;
  for (i = 0; i < ps; i++)
    totn += resp[i].size();
  SEXP res = PROTECT(mkNamed(VECSXP, (const char*[]) { "part", "x", "y", "" }));
  int *pp = INTEGER(SET_VECTOR_ELT(res, 0, allocVector(INTSXP, totn)));
  double *px = REAL(SET_VECTOR_ELT(res, 1, allocVector(REALSXP, totn)));
  double *py = REAL(SET_VECTOR_ELT(res, 2, allocVector(REALSXP, totn)));
  for (i = 0; i < ps; i++) {
    Polygon p = resp[i];
    int n = p.size();
    for (int j = 0; j < n; j++) {
      px[k] = ((double) p[j].X ) / FSCALE;
      py[k] = ((double) p[j].Y ) / FSCALE;
      pp[k] = i + 1;
      k++;
    }
  }
  UNPROTECT(1);
  return res;
}
