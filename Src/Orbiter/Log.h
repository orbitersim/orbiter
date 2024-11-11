// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LOG_H
#define __LOG_H

#include <stdio.h>
#include <windows.h>

// comment the following line to suppress log file output
#define GENERATE_LOG

typedef void (*LogOutFunc)(const char* msg);

// The following routines are for message output into a log file
void InitLog (const char *logfile, bool append);   // Set log file name and clear if exists
void SetLogVerbosity (bool verbose);
void SetLogOutFunc(LogOutFunc func); // clone log output to a function
void LogOut (const char *msg, ...);   // Write a message to the log file
void LogOutVA(const char *format, va_list ap);
void LogOutFine (const char *msg, ...);   // Write a message to the log file if fine-grain output enabled
void LogOut ();                       // Write current message to log file
void LogOut_Error (const char *func, const char *file, int line, const char *msg, ...);  // Write error message to log file
void LogOut_ErrorVA(const char *func, const char *file, int line, const char *msg, va_list ap);
void LogOut_Warning(const char* func, const char* file, int line, const char* msg, ...);  // Write general warning to log file
void LogOut_WarningVA(const char* func, const char* file, int line, const char* msg, va_list ap);
void LogOut_Obsolete(const char* func, const char* msg = 0);      // Write obsolete-function warning to log file
void LogOut_LastError (const char *func, const char *file, int line);             // Write formatted string from GetLastError
void LogOut_DDErr (HRESULT hr, const char *func, const char *file, int line);     // Write DirectDraw error to log file
void LogOut_DIErr (HRESULT hr, const char *func, const char *file, int line);     // Write DirectInput error to log file

// Message formatting components
void LogOut_Error_Start();
void LogOut_Error_End();
void LogOut_Warning_Start();
void LogOut_Warning_End();
void LogOut_Obsolete_Start();
void LogOut_Location(const char* func, const char* file, int line);

#ifdef GENERATE_LOG
#define INITLOG(x,app) InitLog(x,app)
#define LOGOUT(msg,...) LogOut(msg, ##__VA_ARGS__)
#define LOGOUT_FINE(msg,...) LogOutFine(msg, ##__VA_ARGS__)
#define LOGOUT_ERR(msg, ...) LogOut_Error(__FUNCTION__,__FILE__,__LINE__, msg, ##__VA_ARGS__)
#define LOGOUT_LASTERR() LogOut_LastError(__FUNCTION__,__FILE__,__LINE__);
#define LOGOUT_WARN(msg,...) LogOut_Warning(__FUNCTION__, __FILE__, __LINE__, msg, ##__VA_ARGS__)
#define LOGOUT_ERR_FILENOTFOUND(file) LogOut_Error(__FUNCTION__, __FILE__, __LINE__, "File not found: %s", file)
#define LOGOUT_ERR_FILENOTFOUND_MSG(file,msg,...) { \
	LogOut_Error_Start(); \
	LogOut("File not found: %s", file); \
	LogOut(msg, __VA_ARGS__); \
	LogOut_Location(__FUNCTION__,__FILE__,__LINE__); \
	LogOut_Error_End(); \
}
#define LOGOUT_DDERR(hr) LogOut_DDErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_DIERR(hr) LogOut_DIErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_DPERR(hr) LogOut_DPErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_DDERR_ONCE(hr) {static bool bout=true; if(bout) {LogOut_DDErr(hr,__FUNCTION__,__FILE__,__LINE__);bout=false;}}
#define LOGOUT_OBSOLETE {static bool bout=true; if(bout) {LogOut_Obsolete(__FUNCTION__);bout=false;}}
#else
#define INITLOG(x,app)
#define LOGOUT(msg,...)
#define LOGOUT_FINE(msg,...)
#define LOGOUT_ERR(msg)
#define LOGOUT_LASTERR()
#define LOGOUT_WARN(msg,...)
#define LOGOUT_ERR_FILENOTFOUND(file)
#define LOGOUT_DDERR(hr)
#define LOGOUT_DIERR(hr)
#define LOGOUT_DPERR(hr)
#define LOGOUT_OBSOLETE
#endif

// General assertion test. If fatal==true, terminates application
#define ASSERT(test,fatal,msg,...) { \
	if (!(test)) { \
		LogOut_Error_Start(); \
		LogOut("Assertion failure:"); \
		LogOut(msg, ##__VA_ARGS__); \
		LogOut_Location(__FUNCTION__, __FILE__, __LINE__); \
		LogOut_Error_End(); \
		if(fatal) { \
			LogOut(">>> TERMINATING <<<"); \
			DebugBreak(); \
			exit(1); \
		} \
	} \
}

// Debug build-only assertions. Assume debug assertion failures are always fatal
#ifdef _DEBUG
#define dASSERT(test,msg,...) { \
	ASSERT(test, true, msg, ##__VA_ARGS__); \
}
#define dVERIFY(test,msg,...) { \
	ASSERT(!FAILED(test), true, msg, ##__VA_ARGS__); \
}
#define dCHECK(test,msg,...) { \
	ASSERT(test, true, msg, ##__VA_ARGS__); \
}
#else
#define dASSERT(test,msg,...) (test)
#define dVERIFY(test,msg,...) (test)
#define dCHECK(test,msg,...)
#endif

#define CHECKCWD(cwd,name) { char c[512]; _getcwd(c,512); if(strcmp(c,cwd)) { _chdir(cwd); sprintf (c,"CWD modified by module %s - Fixing.",name); LOGOUT_WARN(c); } }

#ifndef __LOG_CPP
extern char logs[256];
#endif

// trace memory allocation
//#define TRACE_MEMALLOC
#ifdef TRACE_MEMALLOC
void tracenew(char *fname, int line);
#define TRACENEW tracenew(__FILE__, __LINE__);
#else
#define TRACENEW
#endif

// =======================================================================
// Profiler methods

void StartProf ();
// put this at the beginning of the section you want to profile

double EndProf (DWORD *count = 0);
// put this at the end of the section to be profiled.
// Return value is the runtime (in microseconds) of the profiled section,
// averaged over all occurrences. If count is initialised, it receives the
// number of occurrecnes.

#endif // !__LOG_H