// ==============================================================
// printf-style formatting into a std::string that is always sized to fit.
// XRSound's log messages include file paths and sound descriptions of unbounded length,
// so formatting them into a fixed-size char buffer silently truncated the end of the message
// (usually the file name, which is the most useful part).
//
// NOTE: this is for XRSound.dll only.  No function in the static XRSound.lib that third-party vessels link with (XRSound.cpp, XRSoundImpl.cpp)
// may call it: that code deliberately avoids the C runtime and std::string.  XRSoundEngine.h, which XRSoundImpl.h includes, includes this header
// only when XRSOUND_DLL_BUILD is defined, so the lib never sees it.
//
// Copyright (c) 2026 Douglas Beachy and contributors
// Licensed under the MIT license
// ==============================================================

#pragma once

#include <cstdarg>
#include <cstdio>
#include <string>

// Formats the arguments printf-style and returns the complete result, however long it is.
// Call it through the FormatString macro below, not directly.
// As with snprintf, MSVC invokes its invalid-parameter handler for an invalid format, so the "could not be formatted" result below only covers
// a vsnprintf failure that returns a negative value instead.
inline std::string FormatStringUnchecked(const char *pFormat, ...)
{
    va_list args;
    va_start(args, pFormat);

    va_list argsCopy;
    va_copy(argsCopy, args);    // vsnprintf consumes the va_list, and we need two passes: one to measure and one to format
    const int length = vsnprintf(nullptr, 0, pFormat, argsCopy);
    va_end(argsCopy);

    std::string result;
    if (length < 0)
    {
        result = "<message could not be formatted> format string: ";
        result += (pFormat ? pFormat : "<null>");
    }
    else if (length > 0)
    {
        // vsnprintf always writes a terminating null, so the buffer is one char larger than the text; the extra char is dropped afterwards
        result.resize(static_cast<size_t>(length) + 1);
        vsnprintf(result.data(), result.size(), pFormat, args);
        result.resize(static_cast<size_t>(length));
    }

    va_end(args);
    return result;
}

// MSVC checks printf format strings against their arguments (warnings C4477 and C4313) only for the printf family itself, not for a
// variadic function like FormatStringUnchecked.  The unevaluated snprintf inside sizeof is never called, but it keeps that compile-time
// check for every FormatString call, as the snprintf calls this replaced had.
#define FormatString(...) (static_cast<void>(sizeof(snprintf(nullptr, 0, __VA_ARGS__))), FormatStringUnchecked(__VA_ARGS__))
