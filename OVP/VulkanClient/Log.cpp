// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012-2026 Jarmo Nikkanen
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
#include <mutex>
#include <chrono>
#include <csignal>
#include <cstdarg>
#include <unistd.h>
#include <QMessageBox>
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Client.h"
#include "OrbiterResource.h"

FILE *d3d9client_log = NULL;

#define LOG_MAX_LINES 100000
#define ERRBUF 8000
#define OPRBUF 512
#define TIMEBUF 63

extern class D3D9Client* g_client;

char ErrBuf[ERRBUF+1];
char OprBuf[OPRBUF+1];
char TimeBuf[TIMEBUF+1];

time_t ltime;
int uEnableLog = 1;     // This value is controlling log opeation ( Config->DebugLvl )
int iEnableLog = 0;     // Index into EnableLogStack
int EnableLogStack[16];
int iLine = 0;          // Line number counter (iLine <= LOG_MAX_LINES)

int64_t qpcFrq = 0;     // Performance counter frequency
int64_t qpcRef = 0;     // Performance counter reference value (for "delta t")
int64_t qpcStart = 0;   // Performance counter start value ("zero")

std::queue<std::string> D3D9DebugQueue;

std::recursive_mutex LogCrit;

// QueryPerformanceCounter counterpart: steady_clock ticks in nanoseconds
static int64_t Ticks() { return std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::steady_clock::now().time_since_epoch()).count(); }


//-------------------------------------------------------------------------------------------
//
void MissingRuntimeError()
{
	QMessageBox::warning(NULL, "VulkanClient Initialization Failed",
		"The Vulkan runtime may be missing. See /Doc/D3D9Client.pdf for more information");
}

//-------------------------------------------------------------------------------------------
//
void FailedDeviceError()
{
	QMessageBox::warning(NULL, "VulkanClient Initialization Failed",
		"Vulkan device failed. The graphics card needs Vulkan 1.4 with shader objects (see Orbiter.log)"); // the DX12 wrapper hint doesn't apply
}

//-------------------------------------------------------------------------------------------
//
void RuntimeError(const char* File, const char* Fnc, UINT Line)
{
	if (Config->DebugLvl == 0) return;
	char buf[256];
	snprintf(buf, 256, "[%s] [%s] Line: %u See Orbiter.log for details.", File, Fnc, Line);
	QMessageBox box(QMessageBox::NoIcon, "Critical Error:", buf, QMessageBox::Ok);
	oapiExecOwned(&box, g_client ? g_client->GetWindow() : NULL); // MessageBoxA (render window, MB_OK)
	raise(SIGTRAP); // DebugBreak
}

//-------------------------------------------------------------------------------------------
//
/*
int PrintModules(DWORD pAdr)
{
	HMODULE hMods[1024];
	HANDLE hProcess;
	DWORD cbNeeded;
	unsigned int i;

	// Get a handle to the process.

	hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetProcessId(GetCurrentProcess()));

	if (NULL == hProcess) return 1;

	if (EnumProcessModules(hProcess, hMods, sizeof(hMods), &cbNeeded)) {
		for (i = 0; i < (cbNeeded / sizeof(HMODULE)); i++) {
			char szModName[MAX_PATH];
			if (GetModuleFileNameExA(hProcess, hMods[i], szModName, sizeof(szModName))) {
				MODULEINFO mi;
				GetModuleInformation(hProcess, hMods[i], &mi, sizeof(MODULEINFO));
				DWORD Base = (DWORD)mi.lpBaseOfDll;
				if (pAdr > Base && pAdr < (Base + mi.SizeOfImage)) LogErr("%s EntryPoint=0x%8.8X, Base=0x%8.8X, Size=%u", szModName, mi.EntryPoint, mi.lpBaseOfDll, mi.SizeOfImage);
				else										 LogOk("%s EntryPoint=0x%8.8X, Base=0x%8.8X, Size=%u", szModName, mi.EntryPoint, mi.lpBaseOfDll, mi.SizeOfImage);
			}
		}
	}
	CloseHandle(hProcess);
	return 0;
}
*/


//-------------------------------------------------------------------------------------------
// Log OAPISURFACE_xxx attributes
void LogAttribs(DWORD attrib, DWORD w, DWORD h, const char *origin)
{
	char buf[512];
	snprintf(buf, 512, "%s (%d,%d)[0x%X]: ", origin, w, h, attrib);
	if (attrib&OAPISURFACE_TEXTURE)		 strcat(buf, "OAPISURFACE_TEXTURE ");
	if (attrib&OAPISURFACE_RENDERTARGET) strcat(buf, "OAPISURFACE_RENDERTARGET ");
	if (attrib&OAPISURFACE_GDI)			 strcat(buf, "OAPISURFACE_GDI ");
	if (attrib&OAPISURFACE_SKETCHPAD)	 strcat(buf, "OAPISURFACE_SKETCHPAD ");
	if (attrib&OAPISURFACE_MIPMAPS)		 strcat(buf, "OAPISURFACE_MIPMAPS ");
	if (attrib&OAPISURFACE_NOMIPMAPS)	 strcat(buf, "OAPISURFACE_NOMIPMAPS ");
	if (attrib&OAPISURFACE_ALPHA)		 strcat(buf, "OAPISURFACE_ALPHA ");
	if (attrib&OAPISURFACE_NOALPHA)		 strcat(buf, "OAPISURFACE_NOALPHA ");
	if (attrib&OAPISURFACE_UNCOMPRESS)	 strcat(buf, "OAPISURFACE_UNCOMPRESS ");
	if (attrib&OAPISURFACE_SYSMEM)		 strcat(buf, "OAPISURFACE_SYSMEM ");
	LogDbg("BlueViolet", buf);
}

//-------------------------------------------------------------------------------------------
//
void D3D9DebugLog(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	vsnprintf(ErrBuf, ERRBUF, format, args);
	va_end(args);

	D3D9DebugQueue.push(std::string(ErrBuf));
}

//-------------------------------------------------------------------------------------------
//
void D3D9DebugLogVec(const char* lbl, oapi::FVECTOR4 &v)
{
	snprintf(ErrBuf, ERRBUF, "%s = [%f, %f, %f, %f]", lbl, v.x, v.y, v.z, v.w);
	D3D9DebugQueue.push(std::string(ErrBuf));
}

//-------------------------------------------------------------------------------------------
//
void D3D9InitLog(const char *file)
{
	qpcFrq = 1000000000; // QueryPerformanceFrequency: nanosecond ticks
	qpcStart = Ticks();

	if (!(d3d9client_log = fopen(oapiResolvePath(file).c_str(),"w+"))) { d3d9client_log=NULL; } // Failed
	else {
		qpcRef = Ticks();
		// InitializeCriticalSectionAndSpinCount left out: the std::recursive_mutex needs no setup
		fprintf(d3d9client_log,"<!DOCTYPE html><html><head><title>VulkanClient Log</title></head><body bgcolor=black text=white>");
		fprintf(d3d9client_log,"<center><h2>VulkanClient Log</h2><br>");
		fprintf(d3d9client_log,"</center><hr><br><br>");
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
	}
}

//-------------------------------------------------------------------------------------------
//
double D3D9GetTime()
{
	int64_t qpcCurrent;
	qpcCurrent = Ticks();
	return double(qpcCurrent) * 1e6 / double(qpcFrq);
}

//-------------------------------------------------------------------------------------------
//
void D3D9SetTime(D3D9Time &inout, double ref)
{
	int64_t qpcCurrent;
	qpcCurrent = Ticks();
	double time = double(qpcCurrent) * 1e6 / double(qpcFrq);
	inout.time += (time - ref);
	inout.count += 1.0;
	inout.peak = std::max((time - ref), inout.peak);
}

//-------------------------------------------------------------------------------------------
//
char *my_ctime()
{
	int64_t qpcCurrent;
	qpcCurrent = Ticks();
	double time = double(qpcCurrent-qpcRef) * 1e3 / double(qpcFrq);
	double start = double(qpcCurrent-qpcStart) / double(qpcFrq);
	snprintf(OprBuf,OPRBUF,"%d: %.1fs %05.2fms", iLine++, start, time);
	qpcRef = qpcCurrent;
	return OprBuf;
}

//-------------------------------------------------------------------------------------------
//
void escape_ErrBuf () {
	std::string buf(ErrBuf);
	size_t n = 0;
	n += replace_all(buf, "&", "&amp;");
	n += replace_all(buf, "<", "&lt;");
	n += replace_all(buf, ">", "&gt;");
	if (n) {
		snprintf(ErrBuf, sizeof(ErrBuf), "%s", buf.c_str());
	}
}

//-------------------------------------------------------------------------------------------
//
void LogTrace(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>3) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=DarkGrey> ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}
}

//-------------------------------------------------------------------------------------------
//
void LogAlw(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>0) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=Olive> ", my_ctime(), th);

		va_list args;
		va_start(args, format);

		vsnprintf(ErrBuf, ERRBUF, format, args);

		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}
}

//-------------------------------------------------------------------------------------------
//
void LogDbg(const char *color, const char *format, ...)
{
	if (d3d9client_log == NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>2) {
		LogCrit.lock();

		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=%s> ", my_ctime(), th, color);

		va_list args;
		va_start(args, format);

		vsnprintf(ErrBuf, ERRBUF, format, args);

		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf, d3d9client_log);
		fputs("</font><br>\n", d3d9client_log);
		fflush(d3d9client_log);

		LogCrit.unlock();
	}
}

//-------------------------------------------------------------------------------------------
//
void LogClr(const char *color, const char *format, ...)
{
	if (d3d9client_log == NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>1) {
		LogCrit.lock();

		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=%s> ", my_ctime(), th, color);

		va_list args;
		va_start(args, format);

		vsnprintf(ErrBuf, ERRBUF, format, args);

		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf, d3d9client_log);
		fputs("</font><br>\n", d3d9client_log);
		fflush(d3d9client_log);

		LogCrit.unlock();
	}
}

//-------------------------------------------------------------------------------------------
//
void LogOapi(const char *format, ...)
{

	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>0) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=Olive> ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		oapiWriteLogV("D3D9: %s", ErrBuf);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}
}

// ---------------------------------------------------
//
void LogErr(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>0) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%X)</font><font color=Red> [ERROR] ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		oapiWriteLogV("D3D9ERROR: %s", ErrBuf);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}
}

// ---------------------------------------------------
//
void LogBlu(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>1) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%X)</font><font color=#1E90FF> ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}
}

// ---------------------------------------------------
//
void LogWrn(const char *format, ...)
{
	if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>1) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%X)</font><font color=Yellow> [WARNING] ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		oapiWriteLogV("D3D9Info: %s", ErrBuf);
		LogCrit.unlock();
	}
}

// ---------------------------------------------------
//
void LogBreak(const char* format, ...)
{
	if (d3d9client_log == NULL) return;
	if (iLine > LOG_MAX_LINES) return;
	if (uEnableLog > 1) {

		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log, "<font color=Gray>(%s)(0x%X)</font><font color=Yellow> [WARNING] ", my_ctime(), th);

		va_list args;
		va_start(args, format);
		vsnprintf(ErrBuf, ERRBUF, format, args);
		va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf, d3d9client_log);
		fputs("</font><br>\n", d3d9client_log);
		fflush(d3d9client_log);
		oapiWriteLogV("D3D9Debug: %s", ErrBuf);
		LogCrit.unlock();

		if (Config->DebugBreak) raise(SIGTRAP); // DebugBreak
	}
}

// ---------------------------------------------------
//
void LogOk(const char *format, ...)
{
	/*if (d3d9client_log==NULL) return;
	if (iLine>LOG_MAX_LINES) return;
	if (uEnableLog>2) {
		LogCrit.lock();
		DWORD th = (DWORD)gettid(); // GetCurrentThreadId
		fprintf(d3d9client_log,"<font color=Gray>(%s)(0x%X)</font><font color=#00FF00> ", my_ctime(), th);

		va_list args;
		va_start(args, format);
        vsnprintf(ErrBuf, ERRBUF, format, args);
        va_end(args);

		escape_ErrBuf();
		fputs(ErrBuf,d3d9client_log);
		fputs("</font><br>\n",d3d9client_log);
		fflush(d3d9client_log);
		LogCrit.unlock();
	}*/
}

