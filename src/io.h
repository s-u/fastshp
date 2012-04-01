/* Simple, generic I/O abstraction that uses simple
   file-I/O-like API which maps to  arbitrary back-ends
   such as file, raw vector, etc.
   
   (C)Copyright 2012 Simon Urbanek

   Licensed under BSD license: open source, no warranty of any kind
   
   This file can be used as a header with purely static functions
   (for better inlining, default) or as header + module if
   IO_EXTERN/IO_EXTERN_IMP are defined in the header/module part
*/

#ifndef IO_H__
#define IO_H__

#include <stdio.h>
#include <string.h>

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>

typedef struct io io_t;

struct io {
    int (*read)(io_t*, void *, int size, int len);
    int (*write)(io_t*, const void *, int size, int len);
    int (*seek)(io_t*, long where);
    int (*eof)(io_t*);
    void (*close)(io_t*);
};

#define io_read(I, B, S, N) (I)->read(I, B, S, N)
#define io_write(I, B, S, N) (I)->write(I, B, S, N)
#define io_seek(I, L) (I)->seek(I, L)
#define io_eof(I) (I)->eof(I)
#define io_close(I) (I)->close(I)

#ifdef IO_EXTERN
#define IO_API
#else
#define IO_API static
#endif

IO_API io_t *io_open_file(const char *fn, const char *mode);
IO_API io_t *io_open_raw(SEXP what);

#if defined IO_EXTERN_IMP || ! defined IO_EXTERN

/* -- implementation -- */

typedef struct io_file {
    io_t io;
    FILE *f;
} io_file_t;

typedef struct io_raw {
    io_t io;
    SEXP raw;
    int pos, len;
} io_raw_t;

static int io_file_read(io_t* io, void *buf, int size, int len) {
    io_file_t *iof = (io_file_t*) io;
    if (!iof || !iof->f) return -1;
    return fread(buf, size, len, iof->f);
}

static int io_file_write(io_t* io, const void *buf, int size, int len) {
    io_file_t *iof = (io_file_t*) io;
    if (!iof || !iof->f) return -1;
    return fwrite(buf, size, len, iof->f);
}

/* >0 = from beginning, <0 = from the end  */
static int io_file_seek(io_t* io, long where) {
    io_file_t *iof = (io_file_t*) io;
    if (!iof || !iof->f) return -1;
    return fseek(iof->f, where, (where >= 0) ? SEEK_SET : SEEK_END);
}

static int io_file_eof(io_t* io) {
    io_file_t *iof = (io_file_t*) io;
    if (!iof || !iof->f) return -1;
    return feof(iof->f);
}

static void io_file_close(io_t* io) {
    io_file_t *iof = (io_file_t*) io;
    if (iof && iof->f) {
	fclose(iof->f);
	iof->f = 0;
    }
    free(io);
}

static io_t *io_open_file(const char *fn, const char *mode) {
    io_file_t *io = (io_file_t*) Calloc(1, io_file_t);
    io->f = fopen(fn, mode);
    if (!io->f) {
	Free(io);
	return 0;
    }
    io->io.read = io_file_read;
    io->io.write = io_file_write;
    io->io.seek = io_file_seek;
    io->io.eof = io_file_eof;
    io->io.close = io_file_close;
    return (io_t*) io;
}

static int io_raw_read(io_t* io, void *buf, int size, int n) {
    io_raw_t *ior = (io_raw_t*) io;
    int len = size * n;
    if (len > ior->len - ior->pos) {
	len = ior->len - ior->pos;
	len -= len % size;
    }
    if (len) {
	memcpy(buf, RAW(ior->raw) + ior->pos, len);
	ior->pos += len;
    }
    return len / size;
}

static int io_raw_write(io_t* io, const void *buf, int size, int n) {
    io_raw_t *ior = (io_raw_t*) io;
    int len = size * n;
    if (len > ior->len - ior->pos) {
	len = ior->len - ior->pos;
	len -= len % size;
    }
    if (len) {
	memcpy(RAW(ior->raw) + ior->pos, buf, len);
	ior->pos += len;
    }
    return len / size;
}

static int io_raw_seek(io_t* io, long where) {
    io_raw_t *ior = (io_raw_t*) io;
    if (where < 0)
	where = ior->len + 1 + where;
    if (where > ior->len)
	where = ior->len;
    ior->pos = where;
    return 0;
}

static int io_raw_eof(io_t* io) {
    io_raw_t *ior = (io_raw_t*) io;
    return (ior->pos >= ior->len) ? 1 : 0;
}

static void io_raw_close(io_t* io) {
    io_raw_t *ior = (io_raw_t*) io;
    if (io) {
	R_ReleaseObject(ior->raw);
	Free(io);
    }
}

static io_t *io_open_raw(SEXP what) {
    io_raw_t *io = (io_raw_t*) Calloc(1, io_raw_t);
    io->raw = what;
    R_PreserveObject(what);
    io->pos = 0;
    io->len = LENGTH(what);
    io->io.read = io_raw_read;
    io->io.write = io_raw_write;
    io->io.seek = io_raw_seek;
    io->io.eof = io_raw_eof;
    io->io.close = io_raw_close;
    return (io_t*) io;
}

#endif

#endif
