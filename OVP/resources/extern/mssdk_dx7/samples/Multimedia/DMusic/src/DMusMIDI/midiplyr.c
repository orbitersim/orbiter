/*****************************************************************************
*
*  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
*  ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED
*  TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR
*  A PARTICULAR PURPOSE.
*
*  Copyright (C) 1993-1999 Microsoft Corporation. All Rights Reserved.
*
******************************************************************************
*
* MIDIPlyr.C
*
* Initialization code for the polymessage MIDI playback app.
*
*****************************************************************************/

#include <windows.h>
#include <windowsx.h>


#include <mmsystem.h>
#include <commdlg.h>
#include <commctrl.h>
#include <ctype.h>
#include <dmusicc.h>
#include <dmusici.h>
#include <string.h>
#include "debug.h"

#include "MIDIPlyr.H"

PUBLIC  char BCODE      gszMWndClass[]      = "MIDIPlyrMWnd";
PUBLIC  char BCODE      gszTWndClass[]      = "MIDIPlyrTWnd";
PUBLIC  HINSTANCE       ghInst              = NULL;
PUBLIC  MIDIPERF        MIDIPerf;
PUBLIC  char            gszUntitled[80]     = "";
PUBLIC  char            gszAppLongName[80] = "";
PUBLIC  char            gszAppTitleMask[80] = "";
PUBLIC  char            grgszTimeFormats[N_TIME_FORMATS][CB_TIME_FORMATS] = {0};
PUBLIC  RECT            grcTWnd             = {0, 0, 0, 0};
PUBLIC  int             gnTimeFormat        = 0;
HWND    ghWnd;
HANDLE  ghEvent;


PRIVATE HWND FNLOCAL InitApp(HINSTANCE hInst, int nCmdShow);
PRIVATE BOOL FNLOCAL InitDirectMusic(VOID);
PRIVATE VOID FNLOCAL UnInitDirectMusic(VOID);
DWORD WINAPI ThreadProc(void* pv);

/*****************************************************************************
*
* WinMain
*
* Called by C startup code.
*
* HANDLE hInst              - Instance handle of this instance
* HANDLE hPrevInst          - Instance handle of previous instance or
*                             NULL if we are the first instance
* LPSTR lpstrCmdLine        - Any command line arguments
* int nCmdShow              - Code for ShowWindow which tells us what state
*                             to initially show the main application window.
*
* Initialize application if first instance.
* Initialize instance.
* Stay in main message processing loop until exit.
*
*****************************************************************************/
int WINAPI WinMain(
    HINSTANCE               hInst,
    HINSTANCE               hPrevInst,
    LPSTR                   lpstrCmdLine,
    int                     nCmdShow)
{
    MSG         msg;
    HWND        hWnd;

    CoInitialize(NULL); // TCB

    if (!InitDirectMusic ())
   {
        DPF(1,"Unable to initialize DirectMusic.");
        UnInitDirectMusic();
        return 0;
   }

    hWnd = InitApp(hInst, nCmdShow);

   if (hWnd == NULL)
   {
        DPF(1,"Unable to initialize DirectMusic MIDI Player.");
        UnInitDirectMusic();
        return 0;
   }

   ghWnd = hWnd;

   while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    UnInitDirectMusic();

    return (msg.wParam);
}

/*****************************************************************************
*
* InitApp
*
* Called for one-time initialization if we are the first app instance.
*
* HANDLE hInst              - Instance handle of this instance
*
* int nCmdShow              - Code for ShowWindow which tells us what state
*                             to initially show the main application window.
*
* Returns TRUE on success.
*
* Register the window class for the main window and the time window.
* Initialize debug library.
* Save the instance handle.
* Load global resource strings.
* Create the main window.
* Add time formats to the options menu.
* Show the main window.
*
*****************************************************************************/
HWND FNLOCAL InitApp(HINSTANCE hInst, int nCmdShow)
{
    WNDCLASS    wc;
    int         idx;
    DWORD       dwError;
    HWND        hWnd;

    InitCommonControls();

    /* Don't specify CS_HREDRAW or CS_VREDRAW if you're going to use the
    ** commctrl status or toolbar -- invalidate the (remaining) client
    ** area yourself if you want this behavior. This will allow the child
    ** control redraws to be much more efficient.
    */
    wc.style =          0;
    wc.lpfnWndProc =    MWnd_WndProc;
    wc.cbClsExtra =     0;
    wc.cbWndExtra =     0;
    wc.hInstance =      hInst;
    wc.hIcon =          LoadIcon(hInst, MAKEINTRESOURCE(ID_ICON));
    wc.hCursor =        LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground =  (HBRUSH)(COLOR_BTNFACE+1);
    wc.lpszMenuName =   MAKEINTRESOURCE(ID_MENU);
    wc.lpszClassName =  (LPCSTR)gszMWndClass;

    RegisterClass(&wc);

    wc.style =          CS_HREDRAW|CS_VREDRAW;
    wc.lpfnWndProc =    TWnd_WndProc;
    wc.cbClsExtra =     0;
    wc.cbWndExtra =     0;
    wc.hInstance =      hInst;
    wc.hIcon =          LoadIcon(hInst, MAKEINTRESOURCE(ID_ICON));
    wc.hCursor =        LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground =  (HBRUSH)(COLOR_WINDOW+1);
    wc.lpszMenuName =   NULL;
    wc.lpszClassName =  (LPCSTR)gszTWndClass;

    RegisterClass(&wc);

    DbgInitialize(TRUE);

    ghInst = hInst;

    LoadString(hInst, IDS_APPTITLEMASK, gszAppTitleMask, sizeof(gszAppTitleMask));
    LoadString(hInst, IDS_APPNAME,      gszAppLongName,  sizeof(gszAppLongName));
    LoadString(hInst, IDS_UNTITLED,     gszUntitled,     sizeof(gszUntitled));

    for (idx = 0; idx < N_TIME_FORMATS; idx++)
    {
        *grgszTimeFormats[idx] = '\0';
        LoadString(hInst,
                   IDS_TF_FIRST+idx,
                   grgszTimeFormats[idx],
                   sizeof(grgszTimeFormats[idx]));
    }

     hWnd = CreateWindow(gszMWndClass,
                        NULL,
                        WS_OVERLAPPEDWINDOW|WS_CLIPCHILDREN,
                        CW_USEDEFAULT,CW_USEDEFAULT,
                        CW_USEDEFAULT,CW_USEDEFAULT,
                        HWND_DESKTOP,
                        NULL,
                        hInst,
                        NULL);

    if (hWnd == (HWND)NULL)
    {
        dwError=GetLastError();
        return NULL;
    }

    ShowWindow(hWnd, nCmdShow);

    return hWnd;

}

BOOL FNLOCAL InitDirectMusic()
{
    BOOL    fAuto = TRUE;
    HANDLE  hThread;
    DWORD   dwThreadID;

    DPF(0, "Initializing DirectMusic");

    // DirectMusic initialization
    if (SUCCEEDED(CoInitialize(NULL)))
    {
        // DirectMusic initialization: Create the performance object
        if (FAILED(CoCreateInstance(&CLSID_DirectMusicPerformance,
                                    NULL,
                                    CLSCTX_INPROC,
                                    &IID_IDirectMusicPerformance,
                                    (void**)&MIDIPerf.pDMusPerformance)))
        {
            DPF (1,"Unable to create a performance object.");
            MIDIPerf.pDMusPerformance = NULL;
            return FALSE;
        }

        // DirectMusic initialization: Set DirectMusic interface
        // pointer to NULL so that Init will return the pointer 
        // for the DirectMusic objects interface that it creates
        MIDIPerf.pDirectMusic = NULL;

        // DirectMusic initialization: Initialize the performance
        // This application doesn't use DSound, so pass NULL for 
        // IDirectSound pointer.
        if (FAILED(MIDIPerf.pDMusPerformance->lpVtbl->Init(MIDIPerf.pDMusPerformance,
                                                            &MIDIPerf.pDirectMusic,
                                                            NULL,
                                                            NULL)))
        {
            DPF (1,"Unable to initialize Performance.");
            return FALSE;
        }

        ghEvent = CreateEvent(NULL, FALSE, FALSE, NULL);

        if (ghEvent == NULL)
        {
            DPF(0, "CreateEvent failed.");
            return FALSE;
        }

        hThread = CreateThread(NULL, 0, ThreadProc, NULL, 0, &dwThreadID);

        if (hThread == NULL)
        {
            DPF(0, "CreateThread failed.");
            return FALSE;
        }

        CloseHandle(hThread);

        if (FAILED(MIDIPerf.pDMusPerformance->lpVtbl->SetNotificationHandle(
                                                        MIDIPerf.pDMusPerformance,
                                                        ghEvent,
                                                        0)))
        {
            DPF(0, "Failed to set notification handle for performance.");
            return FALSE;
        }

        // Request segment notifications
        if (FAILED(MIDIPerf.pDMusPerformance->lpVtbl->AddNotificationType(
            MIDIPerf.pDMusPerformance,
            &GUID_NOTIFICATION_SEGMENT)))
        {
            DPF (0,"Unable add segment notifications to the performance.");
            return FALSE;
        }


        // DirectMusic initialization: Create the loader object    
        if (FAILED(CoCreateInstance(&CLSID_DirectMusicLoader,
                                    NULL,
                                    CLSCTX_INPROC,
                                    &IID_IDirectMusicLoader,
                                    (void**)&MIDIPerf.pDMusLoader)))
        {
            DPF (0,"Unable to create a loader object.");
            MIDIPerf.pDMusLoader = NULL;    
            return FALSE;
        }
    }  // Else terminate the application
    else
    {
            DPF (1,"CoInitialize failed.");
            return FALSE;
    }

    MIDIPerf.fDownloaded = FALSE;
    // Initialization successful
    return TRUE;
}

VOID FNLOCAL UnInitDirectMusic()
{
    if (MIDIPerf.pDMusSegmentState)
    {
        MIDIPerf.pDMusSegmentState->lpVtbl->Release(MIDIPerf.pDMusSegmentState);
    }

    if (MIDIPerf.pDMusSegment)
    {
        MIDIPerf.pDMusPerformance->lpVtbl->Stop(MIDIPerf.pDMusPerformance,
                                                NULL,
                                                NULL,
                                                0,
                                                0);

        if (MIDIPerf.fDownloaded)
        {
            MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                    &GUID_Unload,
                                                    0xFFFFFFFF,
                                                    0,
                                                    0,
                                                    (void*)MIDIPerf.pDMusPerformance);
        }

        MIDIPerf.pDMusSegment->lpVtbl->Release(MIDIPerf.pDMusSegment);
    }

    if (MIDIPerf.pDMusPerformance)
    {
        MIDIPerf.pDMusPerformance->lpVtbl->CloseDown(MIDIPerf.pDMusPerformance);
        MIDIPerf.pDMusPerformance->lpVtbl->Release(MIDIPerf.pDMusPerformance);
        MIDIPerf.pDMusPerformance = NULL;
    }

    if (MIDIPerf.pDMusPort)
    {
        MIDIPerf.pDMusPort->lpVtbl->Release(MIDIPerf.pDMusPort);
    }

    if (MIDIPerf.pDMusLoader)
    {
        MIDIPerf.pDMusLoader->lpVtbl->Release(MIDIPerf.pDMusLoader);    
        MIDIPerf.pDMusLoader = NULL;
    }
    
    if (MIDIPerf.pDirectMusic)
    {
        MIDIPerf.pDirectMusic->lpVtbl->Release(MIDIPerf.pDirectMusic);
        MIDIPerf.pDirectMusic = NULL;
    }

    CloseHandle(ghEvent);

    CoUninitialize();
}


DWORD WINAPI ThreadProc(void* pv)
{
    DWORD    dwReturn;

    while (TRUE)
    {
        dwReturn = WaitForSingleObject(ghEvent,INFINITE);

        if (dwReturn != WAIT_OBJECT_0)
        {
            DPF(0, "WaitForSingleObject failed.");
            return 1;
        }

        PostMessage(ghWnd, WM_GETPMSG, 0, 0);
    }
    

    return 0;
}
