// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Memstat.h"

bool MemStat::bLib = false;
HMODULE MemStat::hLib = 0;

MemStat::MemStat ()
{
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
}

MemStat::~MemStat ()
{
    if (hProc) CloseHandle (hProc);
}

long MemStat::HeapUsage ()
{
	if (pGetProcessMemoryInfo) {
	    PROCESS_MEMORY_COUNTERS pmc;
		pGetProcessMemoryInfo (hProc, &pmc, sizeof(pmc));
		return (long)pmc.WorkingSetSize;
	} else return 0;
}