// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __MEMSTAT_H
#define __MEMSTAT_H

#include <windows.h>
#include <psapi.h>

typedef BOOL (CALLBACK *Proc_GetProcessMemoryInfo)(HANDLE,PPROCESS_MEMORY_COUNTERS,DWORD);

class MemStat {
public:
    MemStat ();
    ~MemStat ();

    long HeapUsage ();

private:
    static HMODULE hLib;
	static bool bLib;
    HANDLE hProc;
	Proc_GetProcessMemoryInfo pGetProcessMemoryInfo;
    bool active;
};

#endif // !__MEMSTAT_H