// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#include "Log.h"
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <windows.h>
#include <OrbiterAPI.h>
#include <queue>
#include <string>

FILE *d3d9client_log = NULL;

#define LOG_MAX_LINES 100000
#define ERRBUF 8000
#define OPRBUF 512
#define TIMEBUF 63

char ErrBuf[ERRBUF+1];
char OprBuf[OPRBUF+1];
char TimeBuf[TIMEBUF+1];

time_t ltime;
int uEnableLog = 1;		// This value is controlling log opeation 
int iEnableLog = 0;
int EnableLogStack[16];
int iLine = 0;

__int64 qpcFrq = 0;
__int64 qpcRef = 0; 
__int64 qpcStart = 0;

std::queue<std::string> D3D9DebugQueue;

CRITICAL_SECTION LogCrit;

//-------------------------------------------------------------------------------------------
//
void D3D9DebugLog(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args);
	va_end(args);

	D3D9DebugQueue.push(std::string(ErrBuf));
}




//-------------------------------------------------------------------------------------------
//
void D3D9InitLog(char *file)
{
	if (fopen_s(&d3d9client_log,file,"w+")) { d3d9client_log=NULL; } // Failed
	else {
		InitializeCriticalSectionAndSpinCount(&LogCrit, 256); 
		fprintf_s(d3d9client_log,"<html><head><title>D3D9Client Log</title></head><body bgcolor=0x000000 text=0xFFFFFF>"); 
		fprintf_s(d3d9client_log,"<center><h2>D3D9Client Log</h2><br>");
		fprintf_s(d3d9client_log,"</center><hr><br><br>"); 
	}
}

//-------------------------------------------------------------------------------------------
//
void D3D9CloseLog()
{
	if (d3d9client_log) {
		fprintf(d3d9client_log,"</body></html>");
		fclose(d3d9client_log);
		d3d9client_log = NULL;
		DeleteCriticalSection(&LogCrit);
	}
}

//-------------------------------------------------------------------------------------------
//
double D3D9GetTime()
{
	__int64 qpcCurrent;
	QueryPerformanceCounter((LARGE_INTEGER*)&qpcCurrent);
	return double(qpcCurrent) * 1e6 / double(qpcFrq);
}

//-------------------------------------------------------------------------------------------
//
void D3D9SetTime(D3D9Time &inout, double ref)
{
	__int64 qpcCurrent;
	QueryPerformanceCounter((LARGE_INTEGER*)&qpcCurrent);
	double time = double(qpcCurrent) * 1e6 / double(qpcFrq);
	inout.time += (time - ref);
	inout.count += 1.0;
}

//-------------------------------------------------------------------------------------------
//
char *my_ctime()
{
	__int64 qpcCurrent;
	QueryPerformanceCounter((LARGE_INTEGER*)&qpcCurrent);
	double time = double(qpcCurrent-qpcRef) * 1e3 / double(qpcFrq);
	double start = double(qpcCurrent-qpcStart) / double(qpcFrq);
	sprintf_s(OprBuf,OPRBUF,"%d: %.1fs %05.2fms", iLine, start, time); iLine++;
	qpcRef = qpcCurrent;
	return OprBuf;
}

//-------------------------------------------------------------------------------------------
//
void LogTrace(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>3) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%lX)</font><font color=DarkGrey>", my_ctime(), th);
		
		va_list args; 
		va_start(args, format); 
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	}
	LeaveCriticalSection(&LogCrit);
}

//-------------------------------------------------------------------------------------------
//
void LogAlw(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>0) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%lX)</font><font color=Olive> ", my_ctime(), th);
		
		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
	
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	}
	LeaveCriticalSection(&LogCrit);
}

//-------------------------------------------------------------------------------------------
//
void LogOapi(const char *format, ...)
{

	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>0) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%lX)</font><font color=Olive> ", my_ctime(), th);
		
		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
	
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	
		int len = strlen(ErrBuf) + 16;
		char *text = new char[len];
		sprintf_s(text, len, "D3D9: %s", ErrBuf);
		oapiWriteLog(text);
		delete[] text;
	}
	LeaveCriticalSection(&LogCrit);
}

// ---------------------------------------------------
//
void LogErr(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>0) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%lX)</font><font color=Red>[ERROR] ", my_ctime(), th);

		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
		
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);

		int len = strlen(ErrBuf) + 16;
		char *text = new char[len];
		sprintf_s(text, len, "D3D9: ERROR: %s", ErrBuf);
		oapiWriteLog(text);
		delete[] text;
	}
	LeaveCriticalSection(&LogCrit);
}	

// ---------------------------------------------------
//
void LogBlu(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>1) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%lX)</font><font color=#1E90FF> ", my_ctime(), th);

		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
	
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	}
	LeaveCriticalSection(&LogCrit);
}	

// ---------------------------------------------------
//
void LogWrn(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>1) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%lX)</font><font color=Yellow>[WARNING] ", my_ctime(), th);

		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 

		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	}
	LeaveCriticalSection(&LogCrit);
}	

// ---------------------------------------------------
//
void LogOk(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	EnterCriticalSection(&LogCrit);
	if (uEnableLog>2) {
		DWORD th = GetCurrentThreadId();
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%lX)</font><font color=#00FF00> ", my_ctime(), th);

		va_list args; 
		va_start(args, format); 
		
		_vsnprintf_s(ErrBuf, ERRBUF, ERRBUF, format, args); 
	
		va_end(args);

		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
	}
	LeaveCriticalSection(&LogCrit);
}	