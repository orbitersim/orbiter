// debug.cpp
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include <windows.h>
#include "debug.h"
#include <stdio.h>

#ifdef _DEBUG

#define MODNAME "MusicLines"

static int dbgLevel;
FILE *fTraceFile = NULL;

void TraceSetup()
{
    dbgLevel = GetProfileInt(MODNAME, "Debug", 0);    
    Trace(0, "Debug level is %d", dbgLevel);

    char szTraceFile[MAX_PATH];
    GetProfileString(MODNAME, "TraceFile", "", szTraceFile, sizeof(szTraceFile));
    if (*szTraceFile)
    {
        fTraceFile = fopen(szTraceFile, "w");
        if (fTraceFile == NULL)
        {
            Trace(-1, "Could not open trace file \"%s\".", szTraceFile);
        }
    }
}

void _TraceMsg(char *msg, HRESULT hr)
{
    LPSTR lpMsgBuf;
    char foo[256];

    // DDraw codes aren't in FormatMessage, plus they're in decimal
    //
    if (HIWORD(hr) == 0x8876)
    {
        wsprintf(foo, "%s: DDraw error %u\n", msg, LOWORD(hr));
    }
    else
    {
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL,
                      hr,
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                      (LPTSTR)&lpMsgBuf,    
                      0,    
                      NULL);

        wsprintf(foo, "%s: [%08lX] %s", msg, hr, lpMsgBuf);
        LocalFree(lpMsgBuf);
    }

    if (fTraceFile)
    {
        fputs(foo, fTraceFile);
        fflush(fTraceFile);
    }
    else
    {
        OutputDebugString(foo);
    }
}

void _Trace(int level, char *msg, ...)
{
    char buffer[512];

    va_list args;
    va_start(args, msg);
    vsprintf(buffer, msg, args);
    va_end(args);

    if (fTraceFile)
    {
        fprintf(fTraceFile, MODNAME ": %s\n", buffer);
        fflush(fTraceFile);
    }
    else
    {
        OutputDebugString(MODNAME ": ");
        OutputDebugString(buffer);
        OutputDebugString("\n");
    }
}

#endif // _DEBUG
