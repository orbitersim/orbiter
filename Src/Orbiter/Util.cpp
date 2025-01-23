// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Util.h"
#include <shlobj.h>
#include <sstream>
#include <iomanip>
#include <unordered_map>
#include <algorithm>

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


// ------------------------------------------------------------------------------
// Floating point output stream formatter
// ------------------------------------------------------------------------------

FltFormatter::FltFormatter (int precision, double value)
	: precision(precision)
	, value(value)
{
}

std::ostream& operator<< (std::ostream& os, const FltFormatter& v)
{
	std::stringstream ss;
	ss << std::setprecision(v.precision) << std::fixed << v.value;
	std::string str;
	ss.str().swap(str);
	str.resize(str.find_last_not_of("0") + 1);
//	if (str[str.length() - 1] == '.') { str.resize(str.length() - 1); } // results in "1"   instead of "1."
	if (str[str.length() - 1] == '.') { str.push_back('0'); }           // results in "1.0" instead of "1."
	os << str;
	return os;
}

FltFormat::FltFormat (int precision /* = 6 */)
	: precision(precision)
{
}

FltFormatter FltFormat::operator() (double value) const {
	return FltFormatter(precision, value);
}

// Extended CSS colors
static const std::unordered_map<std::string, DWORD> colornames {
	{"aliceblue",           0xfff8f0},
	{"antiquewhite",        0xd7ebfa},
	{"aqua",                0xffff00},
	{"aquamarine",          0xd4ff7f},
	{"azure",               0xfffff0},
	{"beige",               0xdcf5f5},
	{"bisque",              0xc4e4ff},
	{"black",               0x000000},
	{"blanchedalmond",      0xcdebff},
	{"blue",                0xff0000},
	{"blueviolet",          0xe22b8a},
	{"brown",               0x2a2aa5},
	{"burlywood",           0x87b8de},
	{"cadetblue",           0xa09e5f},
	{"chartreuse",          0x00ff7f},
	{"chocolate",           0x1e69d2},
	{"coral",               0x507fff},
	{"cornflowerblue",      0xed9564},
	{"cornsilk",            0xdcf8ff},
	{"crimson",             0x3c14dc},
	{"cyan",                0xffff00},
	{"darkblue",            0x8b0000},
	{"darkcyan",            0x8b8b00},
	{"darkgoldenrod",       0x0b86b8},
	{"darkgray",            0xa9a9a9},
	{"darkgreen",           0x006400},
	{"darkgrey",            0xa9a9a9},
	{"darkkhaki",           0x6bb7bd},
	{"darkmagenta",         0x8b008b},
	{"darkolivegreen",      0x2f6b55},
	{"darkorange",          0x008cff},
	{"darkorchid",          0xcc3299},
	{"darkred",             0x00008b},
	{"darksalmon",          0x7a96e9},
	{"darkseagreen",        0x8fbc8f},
	{"darkslateblue",       0x8b3d48},
	{"darkslategray",       0x4f4f2f},
	{"darkslategrey",       0x4f4f2f},
	{"darkturquoise",       0xd1ce00},
	{"darkviolet",          0xd30094},
	{"deeppink",            0x9314ff},
	{"deepskyblue",         0xffbf00},
	{"dimgray",             0x696969},
	{"dimgrey",             0x696969},
	{"dodgerblue",          0xff901e},
	{"firebrick",           0x2222b2},
	{"floralwhite",         0xf0faff},
	{"forestgreen",         0x228b22},
	{"fuchsia",             0xff00ff},
	{"gainsboro",           0xdcdcdc},
	{"ghostwhite",          0xfff8f8},
	{"gold",                0x00d7ff},
	{"goldenrod",           0x20a5da},
	{"gray",                0x808080},
	{"green",               0x008000},
	{"greenyellow",         0x2fffad},
	{"grey",                0x808080},
	{"honeydew",            0xf0fff0},
	{"hotpink",             0xb469ff},
	{"indianred",           0x5c5ccd},
	{"indigo",              0x82004b},
	{"ivory",               0xf0ffff},
	{"khaki",               0x8ce6f0},
	{"lavender",            0xfae6e6},
	{"lavenderblush",       0xf5f0ff},
	{"lawngreen",           0x00fc7c},
	{"lemonchiffon",        0xcdfaff},
	{"lightblue",           0xe6d8ad},
	{"lightcoral",          0x8080f0},
	{"lightcyan",           0xffffe0},
	{"lightgoldenrodyellow",0xd2fafa},
	{"lightgray",           0xd3d3d3},
	{"lightgreen",          0x90ee90},
	{"lightgrey",           0xd3d3d3},
	{"lightpink",           0xc1b6ff},
	{"lightsalmon",         0x7aa0ff},
	{"lightseagreen",       0xaab220},
	{"lightskyblue",        0xface87},
	{"lightslategray",      0x998877},
	{"lightslategrey",      0x998877},
	{"lightsteelblue",      0xdec4b0},
	{"lightyellow",         0xe0ffff},
	{"lime",                0x00ff00},
	{"limegreen",           0x32cd32},
	{"linen",               0xe6f0fa},
	{"magenta",             0xff00ff},
	{"maroon",              0x000080},
	{"mediumaquamarine",    0xaacd66},
	{"mediumblue",          0xcd0000},
	{"mediumorchid",        0xd355ba},
	{"mediumpurple",        0xdb7093},
	{"mediumseagreen",      0x71b33c},
	{"mediumslateblue",     0xee687b},
	{"mediumspringgreen",   0x9afa00},
	{"mediumturquoise",     0xccd148},
	{"mediumvioletred",     0x8515c7},
	{"midnightblue",        0x701919},
	{"mintcream",           0xfafff5},
	{"mistyrose",           0xe1e4ff},
	{"moccasin",            0xb5e4ff},
	{"navajowhite",         0xaddeff},
	{"navy",                0x800000},
	{"oldlace",             0xe6f5fd},
	{"olive",               0x008080},
	{"olivedrab",           0x238e6b},
	{"orange",              0x00a5ff},
	{"orangered",           0x0045ff},
	{"orchid",              0xd670da},
	{"palegoldenrod",       0xaae8ee},
	{"palegreen",           0x98fb98},
	{"paleturquoise",       0xeeeeaf},
	{"palevioletred",       0x9370db},
	{"papayawhip",          0xd5efff},
	{"peachpuff",           0xb9daff},
	{"peru",                0x3f85cd},
	{"pink",                0xcbc0ff},
	{"plum",                0xdda0dd},
	{"powderblue",          0xe6e0b0},
	{"purple",              0x800080},
	{"red",                 0x0000ff},
	{"rosybrown",           0x8f8fbc},
	{"royalblue",           0xe16941},
	{"saddlebrown",         0x13458b},
	{"salmon",              0x7280fa},
	{"sandybrown",          0x60a4f4},
	{"seagreen",            0x578b2e},
	{"seashell",            0xeef5ff},
	{"sienna",              0x2d52a0},
	{"silver",              0xc0c0c0},
	{"skyblue",             0xebce87},
	{"slateblue",           0xcd5a6a},
	{"slategray",           0x908070},
	{"slategrey",           0x908070},
	{"snow",                0xfafaff},
	{"springgreen",         0x7fff00},
	{"steelblue",           0xb48246},
	{"tan",                 0x8cb4d2},
	{"teal",                0x808000},
	{"thistle",             0xd8bfd8},
	{"tomato",              0x4763ff},
	{"turquoise",           0xd0e040},
	{"violet",              0xee82ee},
	{"wheat",               0xb3def5},
	{"white",               0xffffff},
	{"whitesmoke",          0xf5f5f5},
	{"yellow",              0x00ffff},
	{"yellowgreen",         0x32cd9a},
};


// Convert a CSS color string to a DWORD (in 0xbbggrr format)
DWORD GetCSSColor(const char *col)
{
	if(col[0]=='#') {
		DWORD rgb = strtoul(&col[1], NULL, 16);
		DWORD bgr = ((rgb & 0xff0000) >> 16) | (rgb & 0x00ff00) |((rgb & 0x0000ff) << 16);
		return bgr;
	}

	// Force lowercase
	std::string lccol = col;
	std::transform(lccol.begin(), lccol.end(), lccol.begin(), [](unsigned char c){ return std::tolower(c); });

	if (auto search = colornames.find(lccol); search != colornames.end())
        return search->second;

	// Color not found, default to fuchsia
	return 0xFF00FF;
}
