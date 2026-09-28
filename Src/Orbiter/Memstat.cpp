// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Memstat.h"
#ifdef __linux__
#include <stdio.h>
#include <unistd.h>
#endif // __linux__

#ifndef __linux__
bool MemStat::bLib = false;
HMODULE MemStat::hLib = 0;
#else // __linux__
// Psapi.dll load left out: GetProcessMemoryInfo's WorkingSetSize is the resident set, field 2 of /proc/self/statm
#endif // __linux__

MemStat::MemStat ()
{
#ifndef __linux__
	if (!bLib) {
		hLib = LoadLibrary ("Psapi.dll");
		bLib = true;
	}
    hProc = GetCurrentProcess();
    active = (hLib != NULL && hProc != NULL);
	if (active) {
		pGetProcessMemoryInfo = (Proc_GetProcessMemoryInfo)GetProcAddress (hLib, "GetProcessMemoryInfo");
	} else {
		pGetProcessMemoryInfo = 0;
	}
#else // __linux__
	FILE *f = fopen ("/proc/self/statm", "r");
	active = (f != NULL);
	if (f) fclose (f);
#endif // __linux__
}

MemStat::~MemStat ()
{
#ifndef __linux__
    if (hProc) CloseHandle (hProc);
#endif // !__linux__
}

long MemStat::HeapUsage ()
{
#ifndef __linux__
	if (pGetProcessMemoryInfo) {
	    PROCESS_MEMORY_COUNTERS pmc;
		pGetProcessMemoryInfo (hProc, &pmc, sizeof(pmc));
		return (long)pmc.WorkingSetSize;
#else // __linux__
	if (active) {
		long size = 0, resident = 0;
		FILE *f = fopen ("/proc/self/statm", "r");
		if (!f) return 0;
		int n = fscanf (f, "%ld %ld", &size, &resident);
		fclose (f);
		return (n == 2 ? resident * sysconf (_SC_PAGESIZE) : 0);
#endif // __linux__
	} else return 0;
}
