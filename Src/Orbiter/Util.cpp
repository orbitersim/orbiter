// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Util.h"
#include <shlobj.h>

LONGLONG NameToId (const char *name)
{
	LONGLONG id = 0;
	const char *c;
	char *cid = (char*)&id;
	int i = 0;
	for (c = name; *c; c++, i++) cid[i%8] += toupper (*c);
	return id;
}

DWORDLONG Str2Crc (const char *str)
{
	DWORDLONG crc = 0;
	for (const char *c = str; *c; c++)
		crc += (DWORDLONG)*c;
	return crc;
}

double rand1()
{
	static double irmax = 1.0/(double)RAND_MAX;
	return (double)rand()*irmax;
}

char *uscram (const char *str)
{
	static char cbuf[4096];
	char *c;
	int k;
	BYTE key = str[0];
	for (k = 1, c = cbuf; k < 4096 && (str[k] || str[k+1]); k++)
		*c++ = (k&1 ? str[k]+key : str[k]-key);
	*c = '\0';
	return cbuf;
}

bool MakePath (const char *fname)
{
	char cbuf[256];
	int i, len = strlen(fname);
	for (i = len; i > 0; i--)
		if (fname[i-1] == '\\') break;
	if (!i) return false;
	if (fname[0] != '\\' && fname[1] != ':') {
		GetCurrentDirectory (256, cbuf);
		len = strlen(cbuf);
		cbuf[len++] = '\\';
	} else len = 0;
	strncpy_s (cbuf+len, 256-len, fname, i);
	int res = SHCreateDirectoryEx (NULL, cbuf, NULL);
	return res == ERROR_SUCCESS;
}

bool iequal(const std::string& s1, const std::string& s2)
{
	unsigned int len = s1.size();
	if (s2.size() != len)
		return false;
	for (unsigned int i = 0; i < len; i++) {
		if (tolower(s1[i]) != tolower(s2[i]))
			return false;
	}
	return true;
}

static bool need_timer_setup = true;
static LARGE_INTEGER fine_counter_freq; // high-precision tick frequency
static LARGE_INTEGER hi_start;

void tic()
{
	if (need_timer_setup) {
		QueryPerformanceFrequency (&fine_counter_freq);
		need_timer_setup = false;
	}
	QueryPerformanceCounter (&hi_start);
}

double toc()
{
	LARGE_INTEGER hi_end;
	QueryPerformanceCounter (&hi_end);
	_int64 diff = hi_end.QuadPart-hi_start.QuadPart;
	_int64 freq = fine_counter_freq.QuadPart;
	return (double)diff/(double)freq;
	return 0;
}

RECT GetClientPos (HWND hWnd, HWND hChild)
{
	RECT r;
	POINT p;
	GetWindowRect (hChild, &r);
	p.x = r.left, p.y = r.top; ScreenToClient (hWnd, &p);
	r.left = p.x, r.top = p.y;
	p.x = r.right, p.y = r.bottom; ScreenToClient (hWnd, &p);
	r.right = p.x, r.bottom = p.y;
	return r;
}

void SetClientPos (HWND hWnd, HWND hChild, RECT &r)
{
	MoveWindow (hChild, r.left, r.top, r.right-r.left, r.bottom-r.top, true);
}

