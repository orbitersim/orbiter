// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __MEMSTAT_H
#define __MEMSTAT_H

#ifndef __linux__
#include <windows.h>
#include <psapi.h>

typedef BOOL (CALLBACK *Proc_GetProcessMemoryInfo)(HANDLE,PPROCESS_MEMORY_COUNTERS,DWORD);
#else // __linux__
// windows.h/psapi.h left out: the working set is read from /proc/self/statm
#endif // __linux__

class MemStat {
public:
    MemStat ();
    ~MemStat ();

    long HeapUsage ();

private:
#ifndef __linux__
    static HMODULE hLib;
	static bool bLib;
    HANDLE hProc;
	Proc_GetProcessMemoryInfo pGetProcessMemoryInfo;
#else // __linux__
    // hLib/bLib/hProc/pGetProcessMemoryInfo left out: /proc needs no library or process handle
#endif // __linux__
    bool active;
};

#endif // !__MEMSTAT_H
