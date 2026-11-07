#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/time.h>

#define PF_BUF 2048

static const char *pf_fix(const char *p, char *buf)
{
	if (!p || !strchr(p, '\\')) return p;
	size_t n = strlen(p);
	if (n >= PF_BUF) return p;
	for (size_t i = 0; i <= n; i++) buf[i] = p[i] == '\\' ? '/' : p[i];
	return buf;
}

static int pf_open(const char *p, int flags, ...)
{
	char b[PF_BUF];
	int mode = 0;
	if (flags & O_CREAT) {
		va_list a;
		va_start(a, flags);
		mode = va_arg(a, int);
		va_end(a);
	}
	return open(pf_fix(p, b), flags, mode);
}

static int pf_openat(int fd, const char *p, int flags, ...)
{
	char b[PF_BUF];
	int mode = 0;
	if (flags & O_CREAT) {
		va_list a;
		va_start(a, flags);
		mode = va_arg(a, int);
		va_end(a);
	}
	return openat(fd, pf_fix(p, b), flags, mode);
}

static FILE *pf_fopen(const char *p, const char *m) { char b[PF_BUF]; return fopen(pf_fix(p, b), m); }
static FILE *pf_freopen(const char *p, const char *m, FILE *f) { char b[PF_BUF]; return freopen(pf_fix(p, b), m, f); }
static int pf_stat(const char *p, struct stat *s) { char b[PF_BUF]; return stat(pf_fix(p, b), s); }
static int pf_lstat(const char *p, struct stat *s) { char b[PF_BUF]; return lstat(pf_fix(p, b), s); }
static int pf_fstatat(int fd, const char *p, struct stat *s, int f) { char b[PF_BUF]; return fstatat(fd, pf_fix(p, b), s, f); }
static int pf_access(const char *p, int m) { char b[PF_BUF]; return access(pf_fix(p, b), m); }
static int pf_faccessat(int fd, const char *p, int m, int f) { char b[PF_BUF]; return faccessat(fd, pf_fix(p, b), m, f); }
static int pf_mkdir(const char *p, mode_t m) { char b[PF_BUF]; return mkdir(pf_fix(p, b), m); }
static int pf_mkdirat(int fd, const char *p, mode_t m) { char b[PF_BUF]; return mkdirat(fd, pf_fix(p, b), m); }
static int pf_rmdir(const char *p) { char b[PF_BUF]; return rmdir(pf_fix(p, b)); }
static int pf_unlink(const char *p) { char b[PF_BUF]; return unlink(pf_fix(p, b)); }
static int pf_unlinkat(int fd, const char *p, int f) { char b[PF_BUF]; return unlinkat(fd, pf_fix(p, b), f); }
static int pf_remove(const char *p) { char b[PF_BUF]; return remove(pf_fix(p, b)); }
static int pf_rename(const char *a, const char *c) { char b1[PF_BUF], b2[PF_BUF]; return rename(pf_fix(a, b1), pf_fix(c, b2)); }
static int pf_renameat(int fa, const char *a, int fc, const char *c) { char b1[PF_BUF], b2[PF_BUF]; return renameat(fa, pf_fix(a, b1), fc, pf_fix(c, b2)); }
static DIR *pf_opendir(const char *p) { char b[PF_BUF]; return opendir(pf_fix(p, b)); }
static int pf_chdir(const char *p) { char b[PF_BUF]; return chdir(pf_fix(p, b)); }
static char *pf_realpath(const char *p, char *r) { char b[PF_BUF]; return realpath(pf_fix(p, b), r); }
static int pf_truncate(const char *p, off_t l) { char b[PF_BUF]; return truncate(pf_fix(p, b), l); }
static int pf_utimes(const char *p, const struct timeval t[2]) { char b[PF_BUF]; return utimes(pf_fix(p, b), t); }
static int pf_utimensat(int fd, const char *p, const struct timespec t[2], int f) { char b[PF_BUF]; return utimensat(fd, pf_fix(p, b), t, f); }
static int pf_chmod(const char *p, mode_t m) { char b[PF_BUF]; return chmod(pf_fix(p, b), m); }

#define PF_INTERPOSE(repl, orig) { (const void *)repl, (const void *)orig }

__attribute__((used)) static struct { const void *replacement, *original; } pf_interposers[]
	__attribute__((section("__DATA,__interpose"))) = {
	PF_INTERPOSE(pf_open, open),
	PF_INTERPOSE(pf_openat, openat),
	PF_INTERPOSE(pf_fopen, fopen),
	PF_INTERPOSE(pf_freopen, freopen),
	PF_INTERPOSE(pf_stat, stat),
	PF_INTERPOSE(pf_lstat, lstat),
	PF_INTERPOSE(pf_fstatat, fstatat),
	PF_INTERPOSE(pf_access, access),
	PF_INTERPOSE(pf_faccessat, faccessat),
	PF_INTERPOSE(pf_mkdir, mkdir),
	PF_INTERPOSE(pf_mkdirat, mkdirat),
	PF_INTERPOSE(pf_rmdir, rmdir),
	PF_INTERPOSE(pf_unlink, unlink),
	PF_INTERPOSE(pf_unlinkat, unlinkat),
	PF_INTERPOSE(pf_remove, remove),
	PF_INTERPOSE(pf_rename, rename),
	PF_INTERPOSE(pf_renameat, renameat),
	PF_INTERPOSE(pf_opendir, opendir),
	PF_INTERPOSE(pf_chdir, chdir),
	PF_INTERPOSE(pf_realpath, realpath),
	PF_INTERPOSE(pf_truncate, truncate),
	PF_INTERPOSE(pf_utimes, utimes),
	PF_INTERPOSE(pf_utimensat, utimensat),
	PF_INTERPOSE(pf_chmod, chmod),
};
