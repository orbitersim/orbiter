#include "framework.h"
#include "xrsound.h"

// dllmain.cpp : Defines the entry point for the DLL application.
BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                     )
{
    static XRSound *pXRSoundInstance;

    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
        // so we link with XRSound.lib (for Release builds) or XRSoundD.lib (for Debug builds)
        pXRSoundInstance = XRSound::CreateInstance("XRSound-lib-linktest");
        break;

    case DLL_THREAD_ATTACH:
        break;

    case DLL_THREAD_DETACH:
        break;

    case DLL_PROCESS_DETACH:
        delete pXRSoundInstance;
        break;
    }
    return TRUE;
}
