#pragma once
#include <windows.h>
#include <stdarg.h>

#define STRSAFE_E_INSUFFICIENT_BUFFER ((HRESULT)0x8007007A)

inline HRESULT StringCchVPrintfA(char *dst, size_t n, const char *fmt, va_list ap)
{
	if (!dst || !n) return E_INVALIDARG;
	int r = vsnprintf(dst, n, fmt, ap);
	return (r < 0 || (size_t)r >= n) ? STRSAFE_E_INSUFFICIENT_BUFFER : S_OK;
}

inline HRESULT StringCchPrintfA(char *dst, size_t n, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	HRESULT hr = StringCchVPrintfA(dst, n, fmt, ap);
	va_end(ap);
	return hr;
}

inline HRESULT StringCchCopyA(char *dst, size_t n, const char *src)
{
	return strcpy_s(dst, n, src) == 0 ? S_OK : STRSAFE_E_INSUFFICIENT_BUFFER;
}

inline HRESULT StringCchCatA(char *dst, size_t n, const char *src)
{
	return strcat_s(dst, n, src) == 0 ? S_OK : STRSAFE_E_INSUFFICIENT_BUFFER;
}

#define StringCchPrintf StringCchPrintfA
#define StringCchVPrintf StringCchVPrintfA
#define StringCchCopy StringCchCopyA
#define StringCchCat StringCchCatA
