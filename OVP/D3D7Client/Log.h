// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Log.h
// Log file output routines
// ==============================================================

#ifndef __LOG_H
#define __LOG_H

#include <stdio.h>

#define LOGOUT(msg,...) oapiWriteLogV(msg,__VA_ARGS__)
#define LOGOUT_ERR(msg, ...) LogOut_Error(__FUNCTION__,__FILE__,__LINE__, msg, __VA_ARGS__)
#define LOGOUT_WARN(msg,...) LogOut_Warning(msg,__VA_ARGS__)
#define LOGOUT_ERR_FILENOTFOUND(file,msg) { if (msg) sprintf (logs, "%s\n>>> File not found: %s", msg, file); else sprintf(logs,"File not found: %s", file); LOGOUT_ERR(logs); }
#define LOGOUT_DDERR(hr) LogOut_DDErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_DIERR(hr) LogOut_DIErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_DPERR(hr) LogOut_DPErr(hr,__FUNCTION__,__FILE__,__LINE__)
#define LOGOUT_OBSOLETE {static bool bout=true; if(bout) {LogOut_Obsolete(__FUNCTION__);bout=false;}}

#define LOG_DD_ERRORS
#ifdef LOG_DD_ERRORS
#define DDLOG(func) LogOut_DDErr((func),__FUNCTION__,__FILE__,__LINE__)
#else
#define DDLOG(func) (func)
#endif

// The following routines are for message output into a log file
void LogOut (const char *msg, ...);   // Write a message to the log file
void LogOut ();                       // Write current message to log file
void LogOut_Error (const char *func, const char *file, int line, const char *msg, ...);  // Write error message to log file
HRESULT LogOut_DDErr (HRESULT hr, const char *func, const char *file, int line);     // Write DirectDraw error to log file
void LogOut_DIErr (HRESULT hr, const char *func, const char *file, int line);     // Write DirectInput error to log file
void LogOut_DPErr (HRESULT hr, const char *func, const char *file, int line);     // Write DirectPlay error to log file
void LogOut_Obsolete (char *func, char *msg = 0);      // Write obsolete-function warning to log file
void LogOut_Warning (const char *msg, ...);            // Write general warning to log file

#ifdef _DEBUG
#define dVERIFY(test) { if (FAILED(test)) { LogOut_Error (__FUNCTION__, __FILE__, __LINE__, "Assertion failure"); exit(1); }}
#else
#define dVERIFY(test) (test)
#endif

#define ASSERT(test) { if (!(test)) { LogOut_Error (__FUNCTION__, __FILE__, __LINE__, "Assertion failure"); exit(1); }}

#define CHECKCWD(cwd,name) { char c[512]; _getcwd(c,512); if(strcmp(c,cwd)) { _chdir(cwd); sprintf (c,"CWD modified by module %s - Fixing.",name); LogOut_Warning(c); } }

#ifndef __LOG_CPP
extern char logs[256];
#endif

#endif // !__LOG_H