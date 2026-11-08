#include <windows.h>
#include <dlfcn.h>
#include <fnmatch.h>
#include <dirent.h>
#include <sys/stat.h>
#include <mach-o/dyld.h>
#include <mach/mach_time.h>
#include <time.h>
#include <errno.h>
#include <string>
#include <vector>
#include <thread>
#include <chrono>
#include <filesystem>
#include <mutex>
#include <crt_externs.h>
#include <unordered_map>

namespace {

thread_local DWORD g_lastError = 0;
std::mutex g_moduleLock;
std::unordered_map<void *, int> g_moduleRefs;
thread_local std::string g_lastLoadError;

std::string NormalisePath(const char *path)
{
	std::string s = path ? path : "";
	for (char &c : s)
		if (c == '\\') c = '/';
	return s;
}

std::string ModulePath(const char *path)
{
	std::string s = NormalisePath(path);
	size_t n = s.size();
	if (n >= 4 && strcasecmp(s.c_str() + n - 4, ".dll") == 0)
		s = s.substr(0, n - 4) + ".dylib";
	return s;
}

struct FindState {
	DIR *dir = nullptr;
	std::string dirPath;
	std::string pattern;
};

bool NextMatch(FindState *fs, std::string &name, struct stat &st)
{
	while (dirent *e = readdir(fs->dir)) {
		if (fnmatch(fs->pattern.c_str(), e->d_name, FNM_CASEFOLD) != 0)
			continue;
		std::string full = fs->dirPath.empty() ? e->d_name : fs->dirPath + "/" + e->d_name;
		if (stat(full.c_str(), &st) != 0)
			continue;
		name = e->d_name;
		return true;
	}
	return false;
}

FindState *OpenFind(const char *pattern)
{
	std::string p = NormalisePath(pattern);
	size_t slash = p.rfind('/');
	auto *fs = new FindState;
	fs->dirPath = slash == std::string::npos ? "" : p.substr(0, slash);
	fs->pattern = slash == std::string::npos ? p : p.substr(slash + 1);
	if (fs->pattern == "*.*")
		fs->pattern = "*";
	fs->dir = opendir(fs->dirPath.empty() ? "." : fs->dirPath.c_str());
	if (!fs->dir) {
		delete fs;
		return nullptr;
	}
	return fs;
}

void CloseFind(FindState *fs)
{
	if (fs->dir) closedir(fs->dir);
	delete fs;
}

DWORD AttribsFromStat(const struct stat &st, const std::string &name)
{
	DWORD a = 0;
	if (S_ISDIR(st.st_mode)) a |= FILE_ATTRIBUTE_DIRECTORY;
	if (!(st.st_mode & S_IWUSR)) a |= FILE_ATTRIBUTE_READONLY;
	if (!name.empty() && name[0] == '.' && name != "." && name != "..") a |= FILE_ATTRIBUTE_HIDDEN;
	if (!a) a = FILE_ATTRIBUTE_NORMAL;
	return a;
}

void FillFindData(WIN32_FIND_DATAA *d, const std::string &name, const struct stat &st)
{
	memset(d, 0, sizeof(*d));
	d->dwFileAttributes = AttribsFromStat(st, name);
	uint64_t size = (uint64_t)st.st_size;
	d->nFileSizeLow = (DWORD)(size & 0xffffffff);
	d->nFileSizeHigh = (DWORD)(size >> 32);
	uint64_t ft = ((uint64_t)st.st_mtimespec.tv_sec + 11644473600ull) * 10000000ull + st.st_mtimespec.tv_nsec / 100;
	d->ftLastWriteTime.dwLowDateTime = (DWORD)ft;
	d->ftLastWriteTime.dwHighDateTime = (DWORD)(ft >> 32);
	d->ftCreationTime = d->ftLastAccessTime = d->ftLastWriteTime;
	strncpy(d->cFileName, name.c_str(), MAX_PATH - 1);
}

void FillFindData(_finddata_t *d, const std::string &name, const struct stat &st)
{
	memset(d, 0, sizeof(*d));
	DWORD a = AttribsFromStat(st, name);
	d->attrib = (a & FILE_ATTRIBUTE_DIRECTORY ? _A_SUBDIR : 0) | (a & FILE_ATTRIBUTE_READONLY ? _A_RDONLY : 0) | (a & FILE_ATTRIBUTE_HIDDEN ? _A_HIDDEN : 0);
	d->size = (uint32_t)st.st_size;
	d->time_write = st.st_mtimespec.tv_sec;
	d->time_create = st.st_birthtimespec.tv_sec;
	d->time_access = st.st_atimespec.tv_sec;
	strncpy(d->name, name.c_str(), MAX_PATH - 1);
}

uint64_t NowNanos()
{
	static mach_timebase_info_data_t tb = [] { mach_timebase_info_data_t t; mach_timebase_info(&t); return t; }();
	return mach_absolute_time() * tb.numer / tb.denom;
}

void FillSystemTime(SYSTEMTIME *st, bool local)
{
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	struct tm t;
	if (local) localtime_r(&ts.tv_sec, &t);
	else gmtime_r(&ts.tv_sec, &t);
	st->wYear = (WORD)(t.tm_year + 1900);
	st->wMonth = (WORD)(t.tm_mon + 1);
	st->wDayOfWeek = (WORD)t.tm_wday;
	st->wDay = (WORD)t.tm_mday;
	st->wHour = (WORD)t.tm_hour;
	st->wMinute = (WORD)t.tm_min;
	st->wSecond = (WORD)t.tm_sec;
	st->wMilliseconds = (WORD)(ts.tv_nsec / 1000000);
}

}

extern "C" {

const char *OCompatModuleSuffix(void) { return ".dylib"; }
const char *OCompatLastLoadError(void) { return g_lastLoadError.c_str(); }

HMODULE LoadLibraryA(const char *path)
{
	std::string p = ModulePath(path);
	void *h = dlopen(p.c_str(), RTLD_NOW | RTLD_LOCAL);
	if (!h) {
		const char *err = dlerror();
		g_lastLoadError = err ? err : "unknown error";
		g_lastError = 126;
	} else {
		std::lock_guard<std::mutex> lk(g_moduleLock);
		g_moduleRefs[h]++;
	}
	return (HMODULE)h;
}

HMODULE LoadLibraryExA(const char *path, HANDLE, DWORD)
{
	return LoadLibraryA(path);
}

WINBOOL FreeLibrary(HMODULE h)
{
	if (!h) return FALSE;
	bool last = true;
	{
		std::lock_guard<std::mutex> lk(g_moduleLock);
		auto it = g_moduleRefs.find((void *)h);
		if (it != g_moduleRefs.end()) {
			last = --it->second <= 0;
			if (last) g_moduleRefs.erase(it);
		}
	}
	if (last)
		if (auto detach = (void (*)())dlsym((void *)h, "OrbiterModuleDetach"))
			detach();
	return dlclose(h) == 0;
}

void *GetProcAddressImpl(HMODULE h, const char *name)
{
	return dlsym(h ? (void *)h : RTLD_DEFAULT, name);
}

DWORD GetModuleFileNameA(HMODULE h, char *buf, DWORD size)
{
	if (!buf || !size) return 0;
	std::string path;
	if (!h) {
		uint32_t n = 0;
		_NSGetExecutablePath(nullptr, &n);
		std::vector<char> tmp(n + 1);
		if (_NSGetExecutablePath(tmp.data(), &n) == 0) path = tmp.data();
	} else {
		for (uint32_t i = 0, n = _dyld_image_count(); i < n; i++) {
			const char *name = _dyld_get_image_name(i);
			void *hh = dlopen(name, RTLD_NOLOAD | RTLD_LAZY);
			if (hh) {
				dlclose(hh);
				if (hh == (void *)h) { path = name; break; }
			}
		}
	}
	strncpy(buf, path.c_str(), size - 1);
	buf[size - 1] = '\0';
	return (DWORD)strlen(buf);
}

HMODULE GetModuleHandleA(const char *name)
{
	if (!name) return (HMODULE)dlopen(nullptr, RTLD_NOLOAD | RTLD_LAZY);
	std::string want = ModulePath(name);
	size_t slash = want.rfind('/');
	std::string leaf = slash == std::string::npos ? want : want.substr(slash + 1);
	for (uint32_t i = 0, n = _dyld_image_count(); i < n; i++) {
		const char *img = _dyld_get_image_name(i);
		if (!img) continue;
		const char *base = strrchr(img, '/');
		base = base ? base + 1 : img;
		bool match = slash == std::string::npos ? strcasecmp(base, leaf.c_str()) == 0 : strcasecmp(img, want.c_str()) == 0;
		if (!match && slash != std::string::npos) {
			std::error_code ec;
			match = std::filesystem::equivalent(img, want, ec);
		}
		if (match) {
			void *h = dlopen(img, RTLD_NOLOAD | RTLD_LAZY);
			if (h) dlclose(h);
			return (HMODULE)h;
		}
	}
	return nullptr;
}

DWORD GetLastError(void) { return g_lastError ? g_lastError : (DWORD)errno; }
void SetLastError(DWORD err) { g_lastError = err; }

DWORD GetCurrentDirectoryA(DWORD size, char *buf)
{
	if (!getcwd(buf, size)) return 0;
	return (DWORD)strlen(buf);
}

WINBOOL SetCurrentDirectoryA(const char *path) { return chdir(NormalisePath(path).c_str()) == 0; }
WINBOOL CreateDirectoryA(const char *path, void *) { return mkdir(NormalisePath(path).c_str(), 0755) == 0; }
WINBOOL DeleteFileA(const char *path) { return unlink(NormalisePath(path).c_str()) == 0; }
WINBOOL MoveFileA(const char *src, const char *dst) { return rename(NormalisePath(src).c_str(), NormalisePath(dst).c_str()) == 0; }

WINBOOL CopyFileA(const char *src, const char *dst, WINBOOL failIfExists)
{
	std::error_code ec;
	auto opt = failIfExists ? std::filesystem::copy_options::none : std::filesystem::copy_options::overwrite_existing;
	return std::filesystem::copy_file(NormalisePath(src), NormalisePath(dst), opt, ec) && !ec;
}

DWORD GetFileAttributesA(const char *path)
{
	struct stat st;
	std::string p = NormalisePath(path);
	if (stat(p.c_str(), &st) != 0) return INVALID_FILE_ATTRIBUTES;
	size_t slash = p.rfind('/');
	return AttribsFromStat(st, slash == std::string::npos ? p : p.substr(slash + 1));
}

HANDLE FindFirstFileA(const char *pattern, WIN32_FIND_DATAA *data)
{
	FindState *fs = OpenFind(pattern);
	if (!fs) return INVALID_HANDLE_VALUE;
	std::string name;
	struct stat st;
	if (!NextMatch(fs, name, st)) {
		CloseFind(fs);
		return INVALID_HANDLE_VALUE;
	}
	FillFindData(data, name, st);
	return (HANDLE)fs;
}

WINBOOL FindNextFileA(HANDLE h, WIN32_FIND_DATAA *data)
{
	if (!h || h == INVALID_HANDLE_VALUE) return FALSE;
	std::string name;
	struct stat st;
	if (!NextMatch((FindState *)h, name, st)) return FALSE;
	FillFindData(data, name, st);
	return TRUE;
}

WINBOOL FindClose(HANDLE h)
{
	if (!h || h == INVALID_HANDLE_VALUE) return FALSE;
	CloseFind((FindState *)h);
	return TRUE;
}

intptr_t _findfirst(const char *pattern, struct _finddata_t *data)
{
	FindState *fs = OpenFind(pattern);
	if (!fs) return -1;
	std::string name;
	struct stat st;
	if (!NextMatch(fs, name, st)) {
		CloseFind(fs);
		return -1;
	}
	FillFindData(data, name, st);
	return (intptr_t)fs;
}

int _findnext(intptr_t h, struct _finddata_t *data)
{
	if (h == -1 || !h) return -1;
	std::string name;
	struct stat st;
	if (!NextMatch((FindState *)h, name, st)) return -1;
	FillFindData(data, name, st);
	return 0;
}

int _findclose(intptr_t h)
{
	if (h == -1 || !h) return -1;
	CloseFind((FindState *)h);
	return 0;
}

DWORD GetTickCount(void) { return (DWORD)(NowNanos() / 1000000ull); }
ULONGLONG GetTickCount64(void) { return NowNanos() / 1000000ull; }
DWORD timeGetTime(void) { return GetTickCount(); }
UINT timeBeginPeriod(UINT) { return 0; }
UINT timeEndPeriod(UINT) { return 0; }

WINBOOL QueryPerformanceCounter(LARGE_INTEGER *c)
{
	c->QuadPart = (LONGLONG)NowNanos();
	return TRUE;
}

WINBOOL QueryPerformanceFrequency(LARGE_INTEGER *f)
{
	f->QuadPart = 1000000000ll;
	return TRUE;
}

void Sleep(DWORD ms) { std::this_thread::sleep_for(std::chrono::milliseconds(ms)); }
void GetLocalTime(SYSTEMTIME *st) { FillSystemTime(st, true); }
void GetSystemTime(SYSTEMTIME *st) { FillSystemTime(st, false); }

void OutputDebugStringA(const char *str) { fputs(str, stderr); }

int _mkdir(const char *path) { return mkdir(NormalisePath(path).c_str(), 0755); }

char *_strlwr(char *s)
{
	for (char *p = s; *p; p++) *p = (char)tolower((unsigned char)*p);
	return s;
}

char *_strupr(char *s)
{
	for (char *p = s; *p; p++) *p = (char)toupper((unsigned char)*p);
	return s;
}

static char *FormatInt(unsigned long long v, bool neg, char *buf, int radix)
{
	char tmp[72];
	int n = 0;
	do { int d = (int)(v % radix); tmp[n++] = (char)(d < 10 ? '0' + d : 'a' + d - 10); v /= radix; } while (v);
	char *p = buf;
	if (neg) *p++ = '-';
	while (n) *p++ = tmp[--n];
	*p = '\0';
	return buf;
}

char *_itoa(int value, char *buf, int radix)
{
	bool neg = radix == 10 && value < 0;
	unsigned long long v = neg ? (unsigned long long)(-(long long)value) : (unsigned long long)(unsigned)value;
	return FormatInt(v, neg, buf, radix);
}

char *_ltoa(long value, char *buf, int radix)
{
	bool neg = radix == 10 && value < 0;
	unsigned long long v = neg ? (unsigned long long)(-(long long)value) : (unsigned long long)(unsigned long)value;
	return FormatInt(v, neg, buf, radix);
}

char *_ultoa(unsigned long value, char *buf, int radix) { return FormatInt(value, false, buf, radix); }

char *_gcvt(double value, int digits, char *buf)
{
	sprintf(buf, "%.*g", digits, value);
	return buf;
}

void _splitpath(const char *path, char *drive, char *dir, char *fname, char *ext)
{
	std::string p = path ? path : "";
	if (drive) drive[0] = '\0';
	size_t slash = p.find_last_of("/\\");
	std::string d = slash == std::string::npos ? "" : p.substr(0, slash + 1);
	std::string f = slash == std::string::npos ? p : p.substr(slash + 1);
	size_t dot = f.rfind('.');
	std::string e = dot == std::string::npos ? "" : f.substr(dot);
	if (dot != std::string::npos) f = f.substr(0, dot);
	if (dir) strcpy(dir, d.c_str());
	if (fname) strcpy(fname, f.c_str());
	if (ext) strcpy(ext, e.c_str());
}

void _makepath(char *path, const char *drive, const char *dir, const char *fname, const char *ext)
{
	std::string p;
	if (drive) p += drive;
	if (dir && *dir) {
		p += dir;
		if (p.back() != '/' && p.back() != '\\') p += '/';
	}
	if (fname) p += fname;
	if (ext && *ext) {
		if (*ext != '.') p += '.';
		p += ext;
	}
	strcpy(path, p.c_str());
}

char *_fullpath(char *abs, const char *rel, size_t size)
{
	std::error_code ec;
	std::string r = std::filesystem::absolute(NormalisePath(rel), ec).lexically_normal().string();
	if (ec) return nullptr;
	if (!abs) return strdup(r.c_str());
	if (r.size() >= size) return nullptr;
	strcpy(abs, r.c_str());
	return abs;
}

void *_aligned_malloc(size_t size, size_t align)
{
	void *p = nullptr;
	if (align < sizeof(void *)) align = sizeof(void *);
	if (posix_memalign(&p, align, size) != 0) return nullptr;
	return p;
}

void _aligned_free(void *p) { free(p); }

static const uint16_t kCp1252[32] = {
	0x20AC, 0x0081, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008D, 0x017D, 0x008F,
	0x0090, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x009D, 0x017E, 0x0178};

static bool IsUtf8Page(UINT cp) { return cp == 65001 || cp == 0 || cp == 1 || cp == 2 || cp == 3; }
static bool IsLatin1Page(UINT cp) { return cp == 28591; }

int MultiByteToWideChar(UINT cp, DWORD flags, const char *src, int srclen, wchar_t *dst, int dstlen)
{
	if (!src) return 0;
	size_t n = srclen < 0 ? strlen(src) + 1 : (size_t)srclen;
	const unsigned char *p = (const unsigned char *)src, *end = p + n;
	int count = 0;
	while (p < end) {
		uint32_t c = *p++;
		if (IsUtf8Page(cp)) {
			if (c >= 0x80) {
				int extra = c >= 0xF0 && c < 0xF8 ? 3 : c >= 0xE0 ? 2 : c >= 0xC2 ? 1 : -1;
				if (c >= 0xF8) extra = -1;
				bool bad = extra < 0;
				if (!bad) {
					c &= (0x3F >> extra);
					for (int k = 0; k < extra; k++) {
						if (p >= end || (*p & 0xC0) != 0x80) { bad = true; break; }
						c = (c << 6) | (*p++ & 0x3F);
					}
				}
				if (bad) {
					if (flags & 0x08) return 0;
					c = 0xFFFD;
				}
			}
		} else if (!IsLatin1Page(cp) && c >= 0x80 && c < 0xA0) {
			c = kCp1252[c - 0x80];
		}
		if (dst && dstlen) {
			if (count >= dstlen) return 0;
			dst[count] = (wchar_t)c;
		}
		count++;
	}
	return count;
}

int WideCharToMultiByte(UINT cp, DWORD, const wchar_t *src, int srclen, char *dst, int dstlen, const char *defchar, WINBOOL *useddef)
{
	if (useddef) *useddef = FALSE;
	if (!src) return 0;
	size_t n = srclen < 0 ? wcslen(src) + 1 : (size_t)srclen;
	int count = 0;
	auto put = [&](char ch) -> bool {
		if (dst && dstlen) {
			if (count >= dstlen) return false;
			dst[count] = ch;
		}
		count++;
		return true;
	};
	char def = defchar && *defchar ? *defchar : '?';
	for (size_t i = 0; i < n; i++) {
		uint32_t c = (uint32_t)src[i];
		bool ok;
		if (IsUtf8Page(cp)) {
			if (c < 0x80) ok = put((char)c);
			else if (c < 0x800) ok = put((char)(0xC0 | (c >> 6))) && put((char)(0x80 | (c & 0x3F)));
			else if (c < 0x10000) ok = put((char)(0xE0 | (c >> 12))) && put((char)(0x80 | ((c >> 6) & 0x3F))) && put((char)(0x80 | (c & 0x3F)));
			else ok = put((char)(0xF0 | (c >> 18))) && put((char)(0x80 | ((c >> 12) & 0x3F))) && put((char)(0x80 | ((c >> 6) & 0x3F))) && put((char)(0x80 | (c & 0x3F)));
		} else {
			int b = -1;
			if (c < 0x80 || (c >= 0xA0 && c < 0x100)) b = (int)c;
			else if (IsLatin1Page(cp) && c < 0x100) b = (int)c;
			else if (!IsLatin1Page(cp)) {
				for (int k = 0; k < 32; k++) if (kCp1252[k] == c) { b = 0x80 + k; break; }
			}
			if (b < 0) {
				b = (unsigned char)def;
				if (useddef) *useddef = TRUE;
			}
			ok = put((char)b);
		}
		if (!ok) return 0;
	}
	return count;
}

}

const char *GetCommandLineA(void)
{
	static std::string line;
	static std::once_flag once;
	std::call_once(once, [] {
		int argc = *_NSGetArgc();
		char **argv = *_NSGetArgv();
		for (int i = 0; i < argc; i++) {
			if (i) line += ' ';
			bool quote = strchr(argv[i], ' ') != nullptr;
			if (quote) line += '"';
			line += argv[i];
			if (quote) line += '"';
		}
	});
	return line.c_str();
}

namespace {

// Converts an MSVC *scanf_s format and argument list (where %s, %c and %[
// take an extra buffer size) into a standard format and pointer list.
int ScanfSecure(const char *fmt, va_list ap, std::string &out, void **ptrs, int maxPtrs)
{
	int n = 0;
	for (const char *p = fmt; *p; p++) {
		if (*p != '%') { out += *p; continue; }
		out += '%';
		p++;
		if (*p == '%') { out += '%'; continue; }
		bool suppress = false;
		if (*p == '*') { suppress = true; out += '*'; p++; }
		std::string width;
		while (isdigit((unsigned char)*p)) width += *p++;
		std::string len;
		while (*p == 'h' || *p == 'l' || *p == 'L' || *p == 'z' || *p == 'j' || *p == 't' || *p == 'q' || *p == 'I') len += *p++;
		char conv = *p;
		if (!conv) break;
		std::string spec;
		if (conv == '[') {
			spec += *p++;
			if (*p == '^') spec += *p++;
			if (*p == ']') spec += *p++;
			while (*p && *p != ']') spec += *p++;
			if (*p) spec += *p;
		} else {
			spec += conv;
		}
		if (suppress) { out += width + len + spec; continue; }
		void *ptr = va_arg(ap, void *);
		if (conv == 's' || conv == 'c' || conv == '[') {
			unsigned size = va_arg(ap, unsigned);
			if (width.empty() && conv != 'c' && size > 0) width = std::to_string(size - 1);
		}
		out += width + len + spec;
		if (n < maxPtrs) ptrs[n++] = ptr;
	}
	return n;
}

}

int sscanf_s(const char *buf, const char *fmt, ...)
{
	void *p[32] = {};
	std::string f;
	va_list ap;
	va_start(ap, fmt);
	ScanfSecure(fmt, ap, f, p, 32);
	va_end(ap);
	return sscanf(buf, f.c_str(), p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15],
		p[16], p[17], p[18], p[19], p[20], p[21], p[22], p[23], p[24], p[25], p[26], p[27], p[28], p[29], p[30], p[31]);
}

int fscanf_s(FILE *file, const char *fmt, ...)
{
	void *p[32] = {};
	std::string f;
	va_list ap;
	va_start(ap, fmt);
	ScanfSecure(fmt, ap, f, p, 32);
	va_end(ap);
	return fscanf(file, f.c_str(), p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15],
		p[16], p[17], p[18], p[19], p[20], p[21], p[22], p[23], p[24], p[25], p[26], p[27], p[28], p[29], p[30], p[31]);
}
