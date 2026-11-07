// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define __LOG_CPP

#include <string.h>
#include <fstream>
#include <Windows.h>
#include <mach-o/dyld.h>
#include "Log.h"
#include "Orbiter.h"

using namespace std;

extern char DBG_MSG[256];
extern TimeData td;

static char logname[256] = "Orbiter.log";
static char logs[256] = "";
static bool finelog = false;
static DWORD t0 = 0;

static LogOutFunc logOut = 0;

void InitLog (const char *logfile, bool append)
{
	strcpy (logname, logfile);
	ofstream ofs (logname, append ? ios::app : ios::out);
	ofs << "**** " << logname << endl;
	t0 = timeGetTime();
}

void SetLogOutFunc(LogOutFunc func)
{
	logOut = func;
}

void SetLogVerbosity (bool verbose)
{
	finelog = verbose;
}

void LogOutFormat (const char *msg, ...)
{
	va_list ap;
	va_start (ap, msg);
	LogOutVA(msg, ap);
	va_end (ap);
}

void LogOutVA(const char *format, va_list ap)
{
	FILE *f = fopen(logname, "a+t");
	if (f) {
		fprintf(f, "%010.3f: ", (timeGetTime() - t0) * 1e-3);
		vfprintf(f, format, ap);
		fputc('\n', f);
		fclose(f);
	}
	if (logOut) {
		vsnprintf(logs, 255, format, ap);
		(*logOut)(logs);
	}
}

void LogOutFineFormat (const char *msg, ...)
{
	if (finelog) {
		va_list ap;
		va_start (ap, msg);
		FILE *f = fopen (logname, "a+t");
		if (f) {
			fprintf (f, "%010.3f: ", (timeGetTime() - t0) * 1e-3);
			vfprintf (f, msg, ap);
			fputc ('\n', f);
			fclose (f);
		}
		if (logOut) {
			vsnprintf (logs, 255, msg, ap);
			(*logOut)(logs);
		}
		va_end (ap);
	}
}

void LogOut ()
{
	LogOut (logs);
}

void LogOut_ErrorFormat (const char *func, const char *file, int line, const char *msg, ...)
{
	va_list ap;
	va_start (ap, msg);
	LogOut_ErrorVA(func, file, line, msg, ap);
	va_end(ap);
}

void LogOut_Error_Start()
{
	LogOut("============================ ERROR: ===========================");
}

void LogOut_Error_End()
{
	LogOut("===============================================================");
}

void LogOut_Warning_Start()
{
	LogOut("--------------------------- WARNING: --------------------------");
}

void LogOut_Warning_End()
{
	LogOut("---------------------------------------------------------------");
}

void LogOut_Obsolete_Start()
{
	LogOut("-------------------------- OBSOLETE: --------------------------");
}

void LogOut_Location(const char* func, const char* file, int line)
{
	LogOut("[%s | %s | %d]", func, file, line);

}

void LogOut_ErrorVA(const char *func, const char *file, int line, const char *msg, va_list ap)
{
	LogOut_Error_Start();
	LogOutVA(msg, ap);
	LogOut_Location(func, file, line);
	LogOut_Error_End();
}

void LogOut_WarningVA(const char* func, const char* file, int line, const char* msg, va_list ap)
{
	LogOut_Warning_Start();
	LogOutVA(msg, ap);
	LogOut_Location(func, file, line);
	LogOut_Warning_End();
}

void LogOut_LastError (const char *func, const char *file, int line)
{
	DWORD err = GetLastError();
	const char *errString = strerror ((int)err);
	LogOut_Error (func, file, line, errString ? errString : "Unknown error");
}

void LogOut_WarningFormat(const char* func, const char* file, int line, const char* msg, ...)
{
	va_list ap;
	va_start(ap, msg);
	LogOut_WarningVA(func, file, line, msg, ap);
	va_end(ap);
}

void LogOut_Obsolete(const char* func, const char* msg)
{
	LogOut_Obsolete_Start();
	LogOut("Obsolete API function used: %s", func);
	if (msg)
		LogOut(msg);
	else {
		LogOut("At least one active module is accessing an obsolete interface function.");
		LogOut("Addons which rely on obsolete functions may not be compatible with");
		LogOut("future versions of Orbiter.");
	}
	LogOut_Warning_End();
}


void PrintModules()
{
	for (uint32_t i = 0, n = _dyld_image_count(); i < n; i++) {
		const char *name = _dyld_get_image_name(i);
		if (!name || !strncmp(name, "/System/", 8) || !strncmp(name, "/usr/lib/", 9)) continue;
		LogOut("Module linked [%s]", name);
	}
}



void tracenew (char *fname, int line)
{
#define TESTALLOC 2
#if TESTALLOC == 1
	ofstream ofs("tracenew.txt", ios::app);
	sprintf (DBG_MSG, "T=%f, %s: %d", SimT, fname, line);
	ofs << DBG_MSG << endl;
	ofs.close();
#elif TESTALLOC == 2
	sprintf (DBG_MSG, "T=%f, %s: %d", td.SimT0, fname, line);
#else
	MessageBeep (-1);
#endif
}


// =======================================================================
// Profiler methods
// =======================================================================

static double prof_sum = 0.0;
static uint32_t prof_count = 0;
static std::chrono::time_point<std::chrono::steady_clock> prof_t0;

void StartProf ()
{
	prof_t0 = std::chrono::steady_clock::now();
}

double EndProf (DWORD *count)
{
	auto t1 = std::chrono::steady_clock::now();
	std::chrono::duration<double> time_delta = t1 - prof_t0;

	double dt = time_delta.count();
	if (dt > 0.0) {
		prof_sum += dt;
		prof_count++;
	}
	if (count) *count = prof_count;
	return prof_sum/(double)prof_count;
}

