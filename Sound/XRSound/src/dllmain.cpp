// dllmain.cpp : Defines the entry point for the DLL application.
#include "stdafx.h"
#include <crtdbg.h>

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	    case DLL_PROCESS_ATTACH:
#ifdef _DEBUG
            // NOTE: _CRTDBG_CHECK_ALWAYS_DF is too slow
            _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF |
                _CRTDBG_CHECK_CRT_DF |
                _CRTDBG_LEAK_CHECK_DF);
#endif
            break;

	    case DLL_THREAD_ATTACH:
	    case DLL_THREAD_DETACH:
	    case DLL_PROCESS_DETACH:
		    break;
	}
	return TRUE;
}
