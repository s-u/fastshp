#include <stdlib.h>
#include <math.h>

/* Separate Axis Theorem polygon/polygon overlap test
   NOTE: works for convex polygons only! */

typedef struct { double x, y; } vector_t;      /* 2D vector */
typedef struct { double min, max; } range_t;   /* 1D range */
typedef struct { int n; double *x, *y; } polygon_t;

/* create a vector */
static vector_t vector(double x, double y) {
    vector_t a = {x, y};
    return a;
}

/* get a polygon vertex */
static vector_t vertex(polygon_t p, int i) {
    if (i >= p.n) i -= p.n;
    return vector(p.x[i], p.y[i]);
}

/* perpendicular (orthogonal) vector */
static vector_t perp(vector_t v) {
    vector_t a = {v.y, -v.x};
    return a;
}

/* compute direction vector (b - a) */
static vector_t direction(vector_t a, vector_t b) {
    vector_t direction = { b.x - a.x, b.y - a.y };
    return direction;
}

/* normalize vector */
static vector_t normalize(vector_t v) {
    double mag = sqrt(v.x * v.x + v.y * v.y);
    vector_t a = { v.x / mag, v.y / mag };
    return a;
}

/* dot product */
static double dot(vector_t a, vector_t b){
    return a.x  *b.x + a.y * b.y;
}

/* get the range of vertices of a projected on the axis */
static range_t project(polygon_t a, vector_t axis){
    int i;
    double min = dot(vertex(a, 0), (axis = normalize(axis)));
    double max = min;
    for (i = 0; i < a.n; i++){
	double proj = dot(vertex(a, i), axis);
	if (proj < min) min = proj;
	if (proj > max) max = proj;
    }
    {
	range_t r = { min, max };
	return r;
    }
}

/* two ranges overlap */
static int overlap(range_t a, range_t b) {
    return !(a.min > b.max || a.max < b.min);
}

/* Separate Axis Theorem test - project both polygon's
   vertices onto all axes defined by edges the polygons
   and if there is a gap between the polygons in any
   projection then they do not overlap */
static int sat(polygon_t a, polygon_t b){
    int i;
    for (i = 0; i < a.n; i++) {
	vector_t axis = perp(direction(vertex(a, i), vertex(a, i + 1)));
	range_t ap = project(a, axis), bp = project(b, axis);
	if (!overlap(ap, bp)) return 0;
    }
    for (i = 0; i < b.n; i++) {
	vector_t axis = perp(direction(vertex(b, i), vertex(b, i + 1)));
	range_t ap = project(a, axis), bp = project(b, axis);
	if (!overlap(ap, bp)) return 0;
    }
    return 1;
}

/* R API */
#include <Rinternals.h>

SEXP do_sat(SEXP sx1, SEXP sy1, SEXP sx2, SEXP sy2) {
    polygon_t p1, p2;
    p1.n = LENGTH(sx1);
    p1.x = REAL(sx1);
    p1.y = REAL(sy1);
    p2.n = LENGTH(sx2);
    p2.x = REAL(sx2);
    p2.y = REAL(sy2);
    return ScalarLogical(sat(p1, p2));
}

