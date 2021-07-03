// debug.cpp
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifdef DBG
#define _DEBUG
#endif

#ifndef _Debug_
#define _Debug_

#ifdef _DEBUG
#define TraceMsg(msg, hr) _TraceMsg(msg, hr)
#define Trace _Trace

extern void TraceSetup();
extern void _TraceMsg(char *msg, HRESULT hr);
extern void _Trace(int level, char *msg, ...);

#else

#define TraceMsg(msg, hr)
#define Trace

#endif // _DEBUG

#endif // _Debug_
