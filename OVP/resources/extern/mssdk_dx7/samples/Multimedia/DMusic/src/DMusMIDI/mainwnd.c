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
* MainWnd.C
*
* Message handlers and UI for the main window and associated controls
*
*****************************************************************************/

#include <windows.h>
#include <shellapi.h>
#include <windowsx.h>

#include <mmsystem.h>
#include <commdlg.h>
#include <commctrl.h>
#include <ctype.h>

#include <dmusici.h>
#include <dmksctrl.h>

#include "debug.h"

#include "MIDIPlyr.H"

#define BITMAP_COUNT    6           /* Number of buttons in bitmap */
#define MAX_FILEPATH    256

HWND           ghWndToolbar                    = NULL;
HWND           ghWndStatus                     = NULL;
HWND           ghWndTime                       = NULL;
char           gszAppTitle[80]                 = "";
int            gnSB_TFPaneSize                 = 0;
char           gszOpenName[MAX_FILEPATH]       = "";
char           gszOpenTitle[MAX_FILEPATH]      = "";
char BCODE     gszFilter[]                     =
    "MIDI File (*.MID;*.RMI)\0*.MID;*.RMI\0"
    "All Files (*.*)\0*.*\0";

char BCODE     gszDefExtension[]               = "MID";
BOOL           gbAutoPlay                      = TRUE;
UINT           guDevice                        = 0;

TBBUTTON gatbButton[] =
{
    {0, -1,             TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0,  0, -1},
    {0, IDM_OPEN,       TBSTATE_ENABLED, TBSTYLE_BUTTON, 0, 0,  0, -1},
    {0, -1,             TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0,  0, -1},
    {2, IDM_PLAY,       0,               TBSTYLE_BUTTON, 0, 0,  0, -1},
    {3, IDM_STOP,       0,               TBSTYLE_BUTTON, 0, 0,  0, -1},
    {4, IDM_PAUSE,      0,               TBSTYLE_BUTTON, 0, 0,  0, -1},
};

#define BUTTON_COUNT (sizeof(gatbButton)/sizeof(gatbButton[0]))

VOID InitToolbar(HWND hWnd);
VOID InitStatusBar(HWND hWnd);
VOID ResizeStatusBar(HWND hWnd);
VOID SyncUI(HWND hWnd);
VOID SetOneAction(HWND hWnd, int nMenuID, BOOL fEnable);
VOID AttemptFileOpen(HWND hWnd);
BOOL PrerollAndWait(HWND hWnd);
BOOL CreateAndAddPort(HMENU hMenu);
VOID ReleasePort(VOID);
VOID PlaySegment(HWND hWnd);
VOID PauseSegment(HWND hWnd);
VOID StopSegment(VOID);
VOID ToggleReverb(HWND hWnd);
VOID GMReset(VOID);

BOOL MWnd_OnCreate(HWND hWnd, CREATESTRUCT FAR* lpCreateStruct);
VOID MWnd_OnGetMinMaxInfo(HWND hWnd, MINMAXINFO FAR* lpMinMaxInfo);
VOID MWnd_OnSize(HWND hWnd, UINT state, int cx, int cy);
VOID MWnd_OnPaint(HWND hWnd);
VOID MWnd_OnDropFiles(HWND hWnd, HANDLE hDrop);
VOID MWnd_OnFileOpen(HWND hWnd);
VOID MWnd_OnCommandToggleChild(HWND hWnd, UINT id);
VOID MWnd_OnCommand(HWND hWnd, int id, HWND hWndCtl, UINT codeNotify);
VOID MWnd_OnDestroy(HWND hWnd);

/*****************************************************************************
*
* InitToolbar
*
* Called to create the toolbar
*
* HWND hWnd                 - Application window which toolbar is a child of
*
* Create and show the toolbar window.
*
*****************************************************************************/
VOID InitToolbar(HWND hWnd)
{
    ghWndToolbar = CreateToolbarEx(hWnd,
                                   WS_CHILD|WS_CLIPSIBLINGS|WS_CLIPCHILDREN,
                                   IDC_TOOLBAR,
                                   BITMAP_COUNT,
                                   ghInst,
                                   IDB_TOOLBAR,
                                   gatbButton,
                                   BUTTON_COUNT,
                                   0,  0,
                                   0,  0,
                                   sizeof(TBBUTTON));

    if (ghWndToolbar)
        ShowWindow(ghWndToolbar, SW_RESTORE);
}

/*****************************************************************************
*
* InitStatusBar
*
* Called to create the status bar
*
* HWND hWnd                 - Application window which status bar is a child of
*
* Create and show the status window.
*
*****************************************************************************/
VOID InitStatusBar(HWND hWnd)
{
    HWND                    hWndDesktop;
    HFONT                   hFontStat;
    HDC                     hDC;
    UINT                    idx;
    SIZE                    size;

    ghWndStatus = CreateStatusWindow(WS_CHILD|SBARS_SIZEGRIP,
                                     "",
                                     hWnd,
                                     IDC_STATBAR);

    if (ghWndStatus)
    {
        hWndDesktop = GetDesktopWindow();
        hFontStat = (HFONT)SendMessage(ghWndStatus, WM_GETFONT, 0, 0L);
        hDC = GetDC(hWndDesktop);

        if (hFontStat != (HFONT)NULL && hDC != (HDC)NULL)
        {
            hFontStat = (HFONT)SelectObject(hDC, hFontStat);

            gnSB_TFPaneSize = 0;
            for (idx = 0; idx < N_TIME_FORMATS; idx++)
            {
                GetTextExtentPoint(hDC,
                                   grgszTimeFormats[idx],
                                   lstrlen(grgszTimeFormats[idx]),
                                   &size);

                gnSB_TFPaneSize = max(gnSB_TFPaneSize, size.cx);
            }

            SelectObject(hDC, hFontStat);

            gnSB_TFPaneSize *= 2;
        }

        if (hDC != (HDC)NULL)
            ReleaseDC(hWndDesktop, hDC);

        ResizeStatusBar(hWnd);
        
        FORWARD_WM_COMMAND(hWnd, gnTimeFormat, 0, 0, SendMessage);
        ShowWindow(ghWndStatus, SW_RESTORE);
    }
}

/*****************************************************************************
*
* ResizeStatusBar
*
* Force the status bar to resize to fit in the main window
*
* HWND hWnd                 - Application window which status bar is a child of
*
* Figure out the pane sizes and send a message to the status bar to set them.
*
*****************************************************************************/
VOID  ResizeStatusBar(HWND hWnd)
{
    RECT                    rc;
    int                     rnPaneEdge[SB_N_PANES];

    GetClientRect(hWnd, &rc);

    /* SB_SETPARTS expects:
    **  wParam == Number of panes in status bar.
    **  lParam == Pointer to an array of int's indicating the right-hand
    **            coordinate of each pane.
    */
    rnPaneEdge[SB_PANE_STATE] = rc.right - gnSB_TFPaneSize;
    rnPaneEdge[SB_PANE_TFMT]  = -1;

    SendMessage(ghWndStatus,
                SB_SETPARTS,
                SB_N_PANES,
                (DWORD)(LPINT)(rnPaneEdge));
}

/*****************************************************************************
*
* SyncUI
*
* Bring all UI elements into sync with the state of the performance
*
* HWND hWnd                 - Application main window 
*
* Build a flag word of the actions which are allowed from the current state.
* Set the menu items and toolbar buttons for each action appropriately.
* Show the current state as a string in the status bar.
* Depress the pause button if the performance is paused.
* Cause the time window to update.
*
*****************************************************************************/
#define SUI_F_CANPLAY       0x0001
#define SUI_F_CANPAUSE      0x0002
#define SUI_F_CANSTOP       0x0004
#define SUI_F_CANSELDEVICE  0x0008

VOID  SyncUI(HWND hWnd)
{
    WORD                    wActionFlags;
    UINT                    uState;
    char                    szState[40];
    BOOL                    fPress;
    
    // Initialize so that port can be selected before the first file is opened.
    wActionFlags = SUI_F_CANSELDEVICE;
    
    uState = MIDIPerf.uState;
    switch (uState)
    {
        case SEG_S_OPENED:
            wActionFlags = SUI_F_CANPLAY|SUI_F_CANSELDEVICE;
            break;

        case SEG_S_PAUSED:
        case SEG_S_PLAYING:
            wActionFlags = SUI_F_CANPAUSE|SUI_F_CANSTOP;
            break;
    }
    
    fPress = (MIDIPerf.uState == SEG_S_PAUSED);
    SendMessage(ghWndToolbar,
                TB_PRESSBUTTON,
                IDM_PAUSE,
                fPress);

    SetOneAction(hWnd, IDM_PLAY,   wActionFlags & SUI_F_CANPLAY);
    SetOneAction(hWnd, IDM_PAUSE,  wActionFlags & SUI_F_CANPAUSE);
    SetOneAction(hWnd, IDM_STOP,   wActionFlags & SUI_F_CANSTOP);

    EnableMenuItem(GetMenu(hWnd),
                   POS_PLAYTHRU,
                   MF_BYPOSITION|((wActionFlags & SUI_F_CANSELDEVICE) ? MF_ENABLED : MF_GRAYED));

    DrawMenuBar(hWnd);

    szState[0] = '\0';
    LoadString(ghInst, IDS_STATES + uState, szState, sizeof(szState));
    SendMessage(ghWndStatus, SB_SETTEXT, SB_PANE_STATE, (LPARAM)(LPSTR)szState);

    InvalidateRect(ghWndTime, NULL, TRUE);
}

/*****************************************************************************
*
* SetOneAction
*
* Update the state of one action in both the toolbar and action menu
*
* HWND hWnd                 - Application main window
* int nMenuID               - Menu ID of action
* BOOL fEnable              - Enable or disable this action
*
*****************************************************************************/
VOID SetOneAction(
    HWND                hWnd,
    int                 nMenuID,
    BOOL                fEnable)
{
    EnableMenuItem(GetMenu(hWnd),
                    nMenuID,
                    MF_BYCOMMAND|(fEnable ? MF_ENABLED : MF_GRAYED));

    SendMessage(ghWndToolbar,
                TB_ENABLEBUTTON,
                nMenuID,
                (DWORD)fEnable);
}

/*****************************************************************************
*
* AttemptFileOpen
*
* Try to load the given file into a segment.
*
* HWND hWnd                 - Application main window
*
* Stop and close the current file.
* Open the new file.
* Preroll the new file.
* Set the title test for the main window.
* Call SyncUI to update available actions.
*
*****************************************************************************/
VOID AttemptFileOpen(HWND hWnd)
{
    PSTR                pStrFile = gszUntitled;
    DMUS_OBJECTDESC     dmodFile;
    IDirectMusicObject  *pObject;

    // Stop playing the current segment and release it
    
    if (MIDIPerf.pDMusSegment)
    {
        MIDIPerf.pDMusPerformance->lpVtbl->Stop(MIDIPerf.pDMusPerformance, 
                                                NULL,
                                                NULL,
                                                0,
                                                0);

        if (MIDIPerf.pDMusSegmentState)
        {
            MIDIPerf.pDMusSegmentState->lpVtbl->Release(MIDIPerf.pDMusSegmentState);
            MIDIPerf.pDMusSegmentState = NULL;
        }

         if (MIDIPerf.fDownloaded)
         {
            MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                    &GUID_Unload, 
                                                    0xFFFFFFFF,
                                                    0,
                                                    0,
                                                    (void*)MIDIPerf.pDMusPerformance); 

            MIDIPerf.fDownloaded = FALSE;
         }

        // Start points for a segment are cached, so I am calling ReleaseObject
        // in case we are reloading the same file after previously pausing it
       if(SUCCEEDED(MIDIPerf.pDMusSegment->lpVtbl->QueryInterface(
            MIDIPerf.pDMusSegment,
            &IID_IDirectMusicObject, 
            (void**)&pObject)))
        {

            MIDIPerf.pDMusLoader->lpVtbl->ReleaseObject(MIDIPerf.pDMusLoader,
                                                        pObject);
        }

        MIDIPerf.pDMusSegment->lpVtbl->Release(MIDIPerf.pDMusSegment);

        MIDIPerf.pDMusSegment = NULL;

        GMReset();

        MIDIPerf.rtOffset = 0;
        MIDIPerf.mtOffset = 0;

        // Reset the performance state
        MIDIPerf.uState = 0;

        SyncUI(hWnd);
    }

    MIDIPerf.fSegIgnoreBSFGM = FALSE;
    dmodFile.dwSize = sizeof(dmodFile);
    dmodFile.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME | DMUS_OBJ_FULLPATH;
    dmodFile.guidClass = CLSID_DirectMusicSegment;

    MultiByteToWideChar(CP_ACP,
                        0,
                        gszOpenName,
                        -1,
                        dmodFile.wszFileName,
                        sizeof(dmodFile.wszFileName)/sizeof(dmodFile.wszFileName[0]));

    if (FAILED(MIDIPerf.pDMusLoader->lpVtbl->GetObject(MIDIPerf.pDMusLoader,
                                                        &dmodFile,
                                                        &IID_IDirectMusicSegment,
                                                        (void**) &MIDIPerf.pDMusSegment)))
    {
        DPF(0,"DMUSIC MIDI PLAYER: GetObject failed.");
        return;
    }

    pStrFile = gszOpenTitle;

    wsprintf(gszAppTitle, gszAppTitleMask, (LPSTR)pStrFile);
    SetWindowText(hWnd, gszAppTitle);

    MIDIPerf.uState = SEG_S_OPENED;

    SyncUI(hWnd);
}

BOOL CreateAndAddPort(HMENU hMenu)
{
    DMUS_PORTPARAMS     portParams;
    MENUITEMINFO        menuItemInfo;
    HRESULT             hResult;
    DMUS_PORTCAPS       *pPortCaps;
    DWORD               dwMenuFlags;

    //Get DMUS_PORTCAPS pointer for checked port
    menuItemInfo.cbSize = sizeof(MENUITEMINFO);
    menuItemInfo.fMask = MIIM_DATA;

    //Add error checking!! TCB ***
    GetMenuItemInfo(hMenu, 
        MIDIPerf.iMenuPos, 
        FALSE, 
        &menuItemInfo);

    ZeroMemory(&portParams, sizeof(portParams));

    portParams.dwSize = sizeof(portParams);

    if (MIDIPerf.fReverbOn)
    {
        portParams.dwEffectFlags = DMUS_EFFECT_REVERB;
    }
    else
    {
        portParams.dwEffectFlags = 0;
    }

    portParams.dwChannelGroups = 1;
    portParams.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS | 
        DMUS_PORTPARAMS_EFFECTS;

    pPortCaps = (DMUS_PORTCAPS *)menuItemInfo.dwItemData;
    
    hResult = MIDIPerf.pDirectMusic->lpVtbl->CreatePort(MIDIPerf.pDirectMusic,
                                                        &pPortCaps->guidPort,
                                                        &portParams,
                                                        &MIDIPerf.pDMusPort,
                                                        NULL);

    if (FAILED(hResult))
    {
        DPF(1, "Unable to create the requested DirectMusic port.");
        return FALSE;
    }

     if(FAILED(MIDIPerf.pDMusPort->lpVtbl->Activate(MIDIPerf.pDMusPort, TRUE)))
    {
        DPF(0, "Failed to activate the port");
        return FALSE;
    }

    hResult = MIDIPerf.pDMusPerformance->lpVtbl->AddPort(MIDIPerf.pDMusPerformance,
                                                        MIDIPerf.pDMusPort);

    if (FAILED(hResult))
    {
        DPF(1, "Unable to add the requested port to the performance.");
        return FALSE;
    }

    hResult = MIDIPerf.pDMusPerformance->lpVtbl->AssignPChannelBlock(
        MIDIPerf.pDMusPerformance,
        0,
        MIDIPerf.pDMusPort,
        1);

    if (FAILED(hResult))
    {
        DPF(1,"Unable to add the requested DirectMusic port to the performance.");
        return FALSE;
    }

    MIDIPerf.pPortCaps = pPortCaps;

    if (MIDIPerf.pPortCaps->dwEffectFlags & DMUS_EFFECT_REVERB)
        dwMenuFlags = MF_BYCOMMAND | MF_ENABLED;
    else
        dwMenuFlags = MF_BYCOMMAND | MF_GRAYED | MF_DISABLED;

    EnableMenuItem(hMenu,IDM_REVERB, dwMenuFlags);

    return TRUE;
}

VOID ReleasePort(VOID)
{

    if (MIDIPerf.fDownloaded)
    {
        MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                &GUID_Unload,
                                                0xFFFFFFFF,
                                                0,
                                                0,
                                                (void*)MIDIPerf.pDMusPerformance);

        MIDIPerf.fDownloaded = FALSE;
    }

    MIDIPerf.pDMusPerformance->lpVtbl->RemovePort(MIDIPerf.pDMusPerformance,
                                                MIDIPerf.pDMusPort);
                        
    MIDIPerf.pDMusPort->lpVtbl->Release(MIDIPerf.pDMusPort);

    MIDIPerf.pDMusPort = NULL;
}

VOID PlaySegment(HWND hWnd)
{
    HRESULT     hResult;
    
    if (MIDIPerf.pDMusPort == NULL)
        return;

    if (MIDIPerf.pPortCaps->dwFlags & DMUS_PC_DLS)
    {
        if (!MIDIPerf.fSegIgnoreBSFGM)
        {
            hResult = MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                            &GUID_StandardMIDIFile,
                                                            0xFFFFFFFF,
                                                            0,
                                                            0,
                                                            NULL);

            if (hResult)
            {
                DPF(0,
                    "SetParam on StandardMIDI File failed: %08lx",
                    hResult);
            }
            else
                MIDIPerf.fSegIgnoreBSFGM = TRUE;
        }

        if (MIDIPerf.fDownloaded)
        {
            MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                    &GUID_Unload,
                                                    0xFFFFFFFF,
                                                    0,
                                                    0,
                                                    (void*)MIDIPerf.pDMusPerformance);

            MIDIPerf.fDownloaded=FALSE;
        }
    }
    else
    {
        if (MIDIPerf.fSegIgnoreBSFGM)
        {
            AttemptFileOpen(hWnd);

            // Open file failed
            if (!(MIDIPerf.uState == SEG_S_OPENED))
                return;
        }
    }

    if (MIDIPerf.pPortCaps->dwFlags & DMUS_PC_DLS &&  !(MIDIPerf.fDownloaded))
    {
        if (FAILED(MIDIPerf.pDMusSegment->lpVtbl->SetParam(MIDIPerf.pDMusSegment,
                                                            &GUID_Download, 
                                                            0xFFFFFFFF,
                                                            0,
                                                            0,
                                                            (void*)MIDIPerf.pDMusPerformance)))
        {
            DPF(0,"DMUSIC MIDI PLAYER: Download on segment failed.");
            return;
        }
        MIDIPerf.fDownloaded = TRUE;
    }

   if (FAILED(MIDIPerf.pDMusPerformance->lpVtbl->PlaySegment(MIDIPerf.pDMusPerformance,
                                                            MIDIPerf.pDMusSegment,
                                                            DMUS_SEGF_BEAT,
                                                            0,
                                                            &MIDIPerf.pDMusSegmentState)))
    {
        DPF(0, "PlaySegment failed.");
        return;
    }

    // Find out the performance time when segment began playing and
    // convert to reference time. This way if the tempo changes later
    // in the file, we will still have the correct reference start time
    MIDIPerf.pDMusSegmentState->lpVtbl->GetStartTime(MIDIPerf.pDMusSegmentState,
                                                    &MIDIPerf.mtStart);                                                
 
    MIDIPerf.pDMusPerformance->lpVtbl->MusicToReferenceTime(MIDIPerf.pDMusPerformance,
                    MIDIPerf.mtStart,
                    &MIDIPerf.rtStart);

    MIDIPerf.uState = SEG_S_PLAYING;

    SyncUI(hWnd);
}

VOID PauseSegment(HWND hWnd)
{
    MUSIC_TIME          mtNow;
    REFERENCE_TIME      rtNow;
                
    MIDIPerf.pDMusPerformance->lpVtbl->Stop(MIDIPerf.pDMusPerformance,
                                            NULL,
                                            NULL,
                                            0,
                                            0);

    //Find out the current performance time so that we can figure out 
    //where we stopped in the segment.
    MIDIPerf.pDMusPerformance->lpVtbl->GetTime(MIDIPerf.pDMusPerformance,
                                                &rtNow,
                                                &mtNow);

    // Caculate the offset into the segment in music time (ticks)
    // and reference time (milliseconds) and add to previous offset in 
    // cause there has been more than one pause in this segment playback
    MIDIPerf.mtOffset = (mtNow - MIDIPerf.mtStart) + MIDIPerf.mtOffset; 
    MIDIPerf.rtOffset = (rtNow - MIDIPerf.rtStart) + MIDIPerf.rtOffset;

    // Set restart point
    MIDIPerf.pDMusSegment->lpVtbl->SetStartPoint(MIDIPerf.pDMusSegment,
                                                MIDIPerf.mtOffset);
 
    MIDIPerf.pDMusSegmentState->lpVtbl->Release(MIDIPerf.pDMusSegmentState);

    MIDIPerf.pDMusSegmentState = NULL;

    MIDIPerf.uState = SEG_S_PAUSED;
    SyncUI(hWnd);
}

VOID StopSegment(VOID)
{

    MIDIPerf.pDMusPerformance->lpVtbl->Stop(MIDIPerf.pDMusPerformance,
                                            NULL,
                                            NULL,
                                            0,
                                            0);

    // Start segment from beginning in case this has been previously paused.
    MIDIPerf.pDMusSegment->lpVtbl->SetStartPoint(MIDIPerf.pDMusSegment, 0);

    if (MIDIPerf.pDMusSegmentState)
    {
        MIDIPerf.pDMusSegmentState->lpVtbl->Release(MIDIPerf.pDMusSegmentState);

        MIDIPerf.pDMusSegmentState = NULL;
    }

    MIDIPerf.rtOffset = 0;
    MIDIPerf.mtOffset = 0;

    MIDIPerf.uState = SEG_S_OPENED;
}

/*****************************************************************************
*
* MWnd_OnCreate
*
* Handle WM_CREATE message to main application window.
*
* HWND hWnd                 - Window handle
* CREATESTRUCT FAR* lpCreateStruct
*                           - Pointer to creation parameters for the window.
*
* Returns TRUE on success. Returning FALSE will cause the window to be
* destroyed and the application will exit.
*
* Set the default time format.
* Create the tool and status bars.
* Create the time window as a child of the main app window and show it.
* Set the main window's title to show no document ('Untitled').
* Accept drag/drop files.
* Call SyncUI to update the enable status of the toolbar and menu items.
*
*****************************************************************************/
BOOL MWnd_OnCreate(
    HWND                    hWnd,
    CREATESTRUCT FAR*       lpCreateStruct)
{
    HMENU           hMenu;
    HMENU           hMenuOptions;
    HMENU           hMenuPlayThru;
    UINT            idx,iDev;
    RECT            rc;
    DMUS_PORTCAPS   *pPortCaps;
    MENUITEMINFO    menuItemInfo;
    HRESULT         hResult = S_OK;
    GUID            guidDefaultPort;
    BOOL            fDefPortChecked = FALSE;
    char            szPortDescription[DMUS_MAX_DESCRIPTION*2];
    char            szMenuName[80];

    gnTimeFormat = IDS_TF_FIRST;

    InitToolbar(hWnd);
    InitStatusBar(hWnd);

    hMenu = GetMenu(hWnd);
    hMenuOptions = GetSubMenu(hMenu, POS_OPTIONS);

    AppendMenu(hMenuOptions, MF_SEPARATOR, 0, NULL);

    for (idx = 0; idx < N_TIME_FORMATS; idx++)
    {
        AppendMenu(hMenuOptions,
                   MF_ENABLED|MF_STRING,
                   IDS_TF_FIRST + idx,
                   grgszTimeFormats[idx]);
    }


    //Enum ports and save PortCaps ptr in listbox Dword

    // allocate memory for one DMUS_PORTCAPS
    // Call EnumPort with 0 
    // if return value equals S_OK then append menu with new port, put ptr to Port caps structure
    // in listbox dword and increment ptr
    // if return value equals S_FALSE, release port caps structure and exit loop


    hMenuPlayThru = CreateMenu();

    MIDIPerf.pDirectMusic->lpVtbl->GetDefaultPort(MIDIPerf.pDirectMusic,
                                                    &guidDefaultPort);

    idx = 0;
    iDev = 0;

    while (hResult == S_OK)
    {
        if ((pPortCaps = LocalAlloc(LPTR, sizeof(DMUS_PORTCAPS))) == NULL)
        {
            DPF (1,"Unable to allocate memory for port caps structure.");
            return FALSE;
        }

        pPortCaps->dwSize = sizeof(DMUS_PORTCAPS);

        hResult = MIDIPerf.pDirectMusic->lpVtbl->EnumPort(MIDIPerf.pDirectMusic,
                                                            iDev,
                                                            pPortCaps);

        // Don't add input ports. Need to check to see if port is output
        if (hResult == S_OK && pPortCaps->dwClass == DMUS_PC_OUTPUTCLASS)
        {
            WideCharToMultiByte(CP_ACP,
                                0,
                                pPortCaps->wszDescription,
                                -1,
                                szPortDescription,
                                sizeof(szPortDescription)/sizeof(szPortDescription[0]),
                                0,
                                0);

            AppendMenu(hMenuPlayThru,
                        MF_ENABLED|MF_STRING,
                        IDM_DEVICES + idx,
                        szPortDescription);

            // Add port caps ptr to menu item
            menuItemInfo.cbSize = sizeof(MENUITEMINFO);
            menuItemInfo.dwItemData = (DWORD)pPortCaps;
            menuItemInfo.fMask = MIIM_DATA;
            SetMenuItemInfo(hMenuPlayThru,
                            IDM_DEVICES + idx,
                            FALSE,
                            (LPMENUITEMINFO)&menuItemInfo);

            if (IsEqualGUID(&pPortCaps->guidPort,
                &guidDefaultPort))
            {
                CheckMenuItem(hMenuPlayThru,
                    IDM_DEVICES + idx,
                    MF_BYCOMMAND|MF_CHECKED);

                //Save the menu value of the port that is checked
                MIDIPerf.iMenuPos = IDM_DEVICES + idx;

                fDefPortChecked = TRUE;
            }

            idx++;
        }
        else if ((hResult == S_FALSE) |
            (pPortCaps->dwClass == DMUS_PC_INPUTCLASS))
        {
            LocalFree((HLOCAL)pPortCaps);
        }
        else
        {
            DPF (1,"EnumPort failed. Return code: %08lx", hResult);
            return FALSE;
        }
        iDev++;
    }

    LoadString(ghInst,
                IDS_PLAYTHRU,
                szMenuName,
                sizeof(szMenuName));

    // Add Play Thru menu to menu bar
    AppendMenu(hMenu,
        MF_ENABLED|MF_STRING|MF_POPUP,
        (UINT)hMenuPlayThru,
        (LPCTSTR)szMenuName);

    CheckMenuItem(hMenu, IDM_REVERB, MF_BYCOMMAND|MF_CHECKED);
    MIDIPerf.fReverbOn = TRUE;
    CheckMenuItem(hMenu, IDM_TOOLBAR, MF_BYCOMMAND|MF_CHECKED);
    CheckMenuItem(hMenu, IDM_STATUS, MF_BYCOMMAND|MF_CHECKED);
    CheckMenuItem(hMenu, IDM_AUTOPLAY, MF_BYCOMMAND|MF_CHECKED);
    CheckMenuItem(hMenu, gnTimeFormat, MF_BYCOMMAND|MF_CHECKED);

    if (!fDefPortChecked)
    {
        CheckMenuItem(hMenu,
            IDM_DEVICES,
            MF_BYCOMMAND|MF_CHECKED);
        //Save the menu value of the port that is checked
        MIDIPerf.iMenuPos = IDM_DEVICES;
    }

    if (!(CreateAndAddPort(hMenuPlayThru)))
    {
        MessageBox(hWnd,
                    "CreateAndAddPort failed!",
                    "DMusMIDI Error",
                    MB_ICONERROR | MB_OK);

        return FALSE;
    }

    GetClientRect(hWnd, &rc);

    ghWndTime = CreateWindow(gszTWndClass,
                            NULL,
                            WS_CHILD,
                            rc.left, rc.top,
                            rc.right-rc.left, rc.bottom-rc.top,
                            hWnd,
                            NULL,
                            lpCreateStruct->hInstance,
                            NULL);

    ShowWindow(ghWndTime, SW_RESTORE);

    wsprintf(gszAppTitle, gszAppTitleMask, (LPSTR)gszUntitled);
    SetWindowText(hWnd, gszAppTitle);

    DragAcceptFiles(hWnd, TRUE);

    SyncUI(hWnd);

    return TRUE;
}

/*****************************************************************************
*
* MWnd_OnGetMinMaxSize
*
* Handle WM_GETMINMAXSIZE message to main application window.
*
* HWND hWnd                 - Window handle
* MINMAXINFO FAR* lpMinMaxInfo
*                           - Pointer to min/max tracking information
*
* This message is sent to a window before resize tracking begins. The
* lpMinMaxInfo structure contains the minimum and maximum x and y values
* the window can be resized to.
*
* We don't allow resizing small enough to cause the status bar and toolbar
* to overlap so they don't try to draw over each other. 
*
*****************************************************************************/
VOID MWnd_OnGetMinMaxInfo(HWND hWnd, MINMAXINFO FAR* lpMinMaxInfo)
{
    RECT                    rc;

    GetWindowRect(hWnd, &rc);

    /* Don't allow resizing small enough to cause the status bar and toolbar
    to overlap so they don't try to draw over each other. 
    */
    lpMinMaxInfo->ptMinTrackSize.x = 200;
    lpMinMaxInfo->ptMinTrackSize.y = 150;
}

/*****************************************************************************
*
* MWnd_OnSize
*
* Handle WM_SIZE message to main application window.
*
* HWND hWnd                 - Window handle
* UINT state                - Some SIZE_xxx code indicating what type of
*                             size operation this is.
* int  cx, cy               - New x and y size of the window's client area.
*
* Get the new client area.
* Adjust the client area for the toolbar and status bar if they exist.
* Make sure the client area isn't a negative height and adjust if it is.
* Resize the time window to fit in our new client area.
* Forward the WM_SIZE to the time window so it can resize its font.
*
*****************************************************************************/
VOID MWnd_OnSize(
    HWND                    hWnd,
    UINT                    state,
    int                     cx,
    int                     cy)
{
    RECT                    rc;
    RECT                    rcClient;

    GetClientRect(hWnd, &rcClient);
    if (ghWndToolbar != NULL)
    {
        /* Cause the toolbar to be aware of the size change
        */
        FORWARD_WM_SIZE(ghWndToolbar, SIZE_RESTORED, 0, 0, SendMessage);
        
        GetWindowRect(ghWndToolbar, &rc);
        rcClient.top += (rc.bottom - rc.top);
    }

    if (ghWndStatus != NULL)
    {
        ResizeStatusBar(hWnd);
        
        /* Cause the status bar to be aware of the size change
        */
        FORWARD_WM_SIZE(ghWndStatus, SIZE_RESTORED, 0, 0, SendMessage);
        
        GetWindowRect(ghWndStatus, &rc);
        rcClient.bottom -= (rc.bottom - rc.top);
    }

    /* Do we need to resize entire window so the tool/status bars
    ** don't overlap? (The only case where this can happen is
    ** on creation of one of the two -- we set minimum tracking so
    ** a user can't manually resize the window to cause this
    ** condition).
    */
    if (rcClient.bottom < rcClient.top)
    {
        GetWindowRect(hWnd, &rc);
        SetWindowPos(hWnd,
                     (HWND)NULL,
                     0, 0,
                     rc.right - rc.left + 1,
                     rc.bottom - rc.top + 1 - rcClient.top - rcClient.bottom,
                     SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOZORDER);
    }

    SetWindowPos(ghWndTime,
                 (HWND)NULL,
                 rcClient.left,
                 rcClient.top,
                 rcClient.right - rcClient.left,
                 rcClient.bottom - rcClient.top,
                 SWP_NOACTIVATE|SWP_NOZORDER);

    FORWARD_WM_SIZE(ghWndTime, SIZE_RESTORED, 0, 0, SendMessage);
}

/*****************************************************************************
*
* MWnd_OnPaint
*
* Handle WM_PAINT message to main application window.
*
* HWND hWnd                 - Window handle
*
* Just do a BeginPaint/EndPaint pair so USER will mark the area
*   as valid. All the real work of painting the time is done
*   by the WM_PAINT handler for the time window.
*
*****************************************************************************/
VOID MWnd_OnPaint(HWND hWnd)
{
    PAINTSTRUCT     ps;
    HDC             hDC;

    hDC = BeginPaint(hWnd, &ps);
    EndPaint(hWnd, &ps);
}

/*****************************************************************************
*
* MWnd_OnDropFiles
*
* Handle WM_DROPFILES message to main application window.
*
* HWND hWnd                 - Window handle
* HANDLE hDrop               - Handle to dropped file information
*
* Get the 0th filename and free the drop handle.
* Extract the file title from the full pathname.
* Open the file.
* If we opened successfully, start playback by forwarding a WM_COMMAND
*   of IDM_PLAY to the main window.
*
*****************************************************************************/
VOID MWnd_OnDropFiles(HWND hWnd, HANDLE hDrop)
{
    PSTR                    pStr;
    
    // For multiple selections, we only accept the first file
    DragQueryFile(hDrop, 0, gszOpenName, sizeof(gszOpenName));
    DragFinish(hDrop);

    /* We don't get OpenTitle like we do from GetOpenFileName; need to
    ** figure this out for ourselves */

    pStr = gszOpenName + lstrlen(gszOpenName) - 1;

    while (pStr >= gszOpenName && *pStr != '/' && *pStr != '\\' && *pStr != ':')
        pStr--;

    pStr++;
    lstrcpy(gszOpenTitle, pStr);

    AttemptFileOpen(hWnd);

    if (gbAutoPlay && MIDIPerf.uState == SEG_S_OPENED)
        FORWARD_WM_COMMAND(hWnd, IDM_PLAY, (HWND)NULL, 0, SendMessage);
}


/*****************************************************************************
*
* MWnd_OnFileOpen
*
* Handle WM_COMMAND/IDM_OPEN message to main application window.
*
* HWND hWnd                 - Window handle
*
* Fill in the OPENFILENAME struct and call GetOpenFileName.
* If not canceled, try to open the file.
*
*****************************************************************************/
VOID MWnd_OnFileOpen(HWND hWnd)
{
    static CHAR strPath[512] = "";

    OPENFILENAME            ofn;
    *gszOpenName = '\0';

    ofn.lStructSize         = sizeof(OPENFILENAME);
    ofn.hwndOwner           = hWnd;
    ofn.lpstrFilter         = gszFilter;
    ofn.lpstrCustomFilter   = (LPSTR)NULL;
    ofn.nMaxCustFilter      = 0L;
    ofn.nFilterIndex        = 1L;
    ofn.lpstrFile           = gszOpenName;
    ofn.nMaxFile            = MAX_FILEPATH;
    ofn.lpstrFileTitle      = gszOpenTitle;
    ofn.nMaxFileTitle       = MAX_FILEPATH;
    ofn.lpstrTitle          = (LPSTR)NULL;
    ofn.lpstrInitialDir     = (LPSTR)NULL;
    ofn.Flags               = OFN_HIDEREADONLY|OFN_FILEMUSTEXIST;
    ofn.nFileOffset         = 0;
    ofn.nFileExtension      = 0;
    ofn.lpstrDefExt         = gszDefExtension;

    // Get the initial path from the registry
    if( strPath[0] == '\0' )
    {
        DWORD type, size = 512;
        HKEY  key;

        // Open the appropriate registry key
        LONG result = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
                                    TEXT("Software\\Microsoft\\DirectX"),
                                    0, KEY_READ, &key );
        if( ERROR_SUCCESS == result )
        {
            result = RegQueryValueEx( key, TEXT("DXSDK Samples Path"), NULL,
                                      &type, (BYTE*)strPath, &size );
            RegCloseKey( key );

            if( ERROR_SUCCESS == result )
            {
                strcat( strPath, TEXT("\\DMusic\\Media\\") );
                ofn.lpstrInitialDir = strPath;
            }
        }
    }

    if (!GetOpenFileName(&ofn))
        return;

    AttemptFileOpen(hWnd);
}

/*****************************************************************************
*
* ToggleReverb
*
* Handle WM_COMMAND/IDM_REVERB message to main application window.
*
* HWND hWnd                 - Window handle
*
* Turn reverb on or off based on status of menu item. Set global variable
* MIDIPerf.fReverb
*
*****************************************************************************/

VOID ToggleReverb(HWND hWnd)
{
    HMENU        hMenu;
    UINT         uState;
    IKsControl   *pControl;
    KSPROPERTY   ksp;
    ULONG        cb;
    DWORD        dwEffects = 0;
    HRESULT      hr;


    hMenu = GetMenu(hWnd);
    uState = GetMenuState(hMenu, IDM_REVERB, MF_BYCOMMAND);

    hr = MIDIPerf.pDMusPort->lpVtbl->QueryInterface(MIDIPerf.pDMusPort,
                                                    &IID_IKsControl,
                                                    (void**)&pControl);

    if (SUCCEEDED(hr))
    {
        ZeroMemory(&ksp, sizeof(ksp));

        // If currently checked, need to turn Reverb Off
        if (uState & MF_CHECKED)
        {
            DPF(0,"Turning REVERB Off");
            dwEffects = 0;
        }
        else // Turn Reverb On
        {
            DPF(0,"Turning REVERB On");
            dwEffects = DMUS_EFFECT_REVERB;
        }
        
#ifdef _MSC_EXTENSIONS
        ksp.Set        = GUID_DMUS_PROP_Effects;
        ksp.Id         = 0;
        ksp.Flags      = KSPROPERTY_TYPE_SET;
#else
        ksp.Data.Set   = GUID_DMUS_PROP_Effects;
        ksp.Data.Id    = 0;
        ksp.Data.Flags = KSPROPERTY_TYPE_SET;
#endif
                            
        pControl->lpVtbl->KsProperty(pControl,
                                    &ksp,
                                    sizeof(ksp),
                                    (LPVOID)&dwEffects,
                                    sizeof(dwEffects),
                                    &cb);

        pControl->lpVtbl->Release(pControl);

        // Toggle the checked state
        uState ^= MF_CHECKED;
        uState &= MF_CHECKED;
        CheckMenuItem(hMenu, IDM_REVERB, MF_BYCOMMAND|uState);

        SyncUI(hWnd);
    }
}

VOID GMReset(VOID)
{
    DMUS_SYSEX_PMSG     *pGMReset;
    BYTE                abGMReset[] = {0xF0,0x7E,0x7F,0x09,0x01,0xF7};
    DWORD               dwLen;

    dwLen = sizeof(abGMReset)/sizeof(abGMReset[0]);

    if(SUCCEEDED( MIDIPerf.pDMusPerformance->lpVtbl->AllocPMsg(MIDIPerf.pDMusPerformance,
                                                            sizeof(DMUS_SYSEX_PMSG) + dwLen,
                                                            (DMUS_PMSG**)&pGMReset )))
    {
        ZeroMemory(pGMReset, sizeof(pGMReset));
        pGMReset->dwSize = sizeof(DMUS_SYSEX_PMSG);
        pGMReset->dwPChannel = 0;
        pGMReset->dwVirtualTrackID = 0;
        pGMReset->dwType = DMUS_PMSGT_SYSEX ;
        pGMReset->dwVoiceID = 0;
        pGMReset->dwGroupID = 0xFFFFFFFF;

        pGMReset->dwLen = dwLen;
        memcpy(pGMReset->abData, abGMReset, dwLen);

        if (SUCCEEDED(MIDIPerf.pDMusPerformance->lpVtbl->GetTime(MIDIPerf.pDMusPerformance,
                                                                NULL,
                                                                &pGMReset->mtTime)))
        {
            pGMReset->dwFlags = DMUS_PMSGF_MUSICTIME | DMUS_PMSGF_TOOL_IMMEDIATE;
        }

            MIDIPerf.pDMusPerformance->lpVtbl->SendPMsg(
                MIDIPerf.pDMusPerformance, 
                (DMUS_PMSG*)pGMReset);
    }
}


/*****************************************************************************
*
* MWnd_OnCommandToggleChild
*
* Handle WM_COMMAND message of toggle tool or status bar to main application
* window.
*
* HWND hWnd                 - Window handle
* UINT id                   - Control id of menu selection; either
*                             IDM_TOOLBAR or IDM_STATUS
*
* Get the current menu item check state.
* Destroy or create the child as needed.
* Send a WM_SIZE to the main window so client area will be recalculated.
* Toggle the menu item check state.
*
*****************************************************************************/
VOID MWnd_OnCommandToggleChild(HWND hWnd, UINT id)
{
    HMENU   hMenu;
    UINT    uState;
    HWND*   phWnd;

    phWnd = (id == IDM_TOOLBAR) ? &ghWndToolbar : &ghWndStatus;

    hMenu = GetMenu(hWnd);
    uState = GetMenuState(hMenu, id, MF_BYCOMMAND);
    if (uState & MF_CHECKED)
    {
        DestroyWindow(*phWnd);
        *phWnd = NULL;
    }
    else
    {
        if (id == IDM_TOOLBAR)
            InitToolbar(hWnd);
        else
            InitStatusBar(hWnd);
    }

    SendMessage(hWnd, WM_SIZE, 0, 0L);

    uState ^= MF_CHECKED;
    uState &= MF_CHECKED;
    CheckMenuItem(hMenu, id, MF_BYCOMMAND|uState);

    SyncUI(hWnd);
}

/*****************************************************************************
*
* MWnd_OnCommand
*
* Handle WM_COMMAND message to main application window.
*
* HWND hWnd                 - Window handle
* int id                    - id of control or menu causing WM_COMMAND
* HWND hwndCtl              - Window handle of child control, if any
* UINT codeNotify           - Notification code if this message is from a
*                             control.
*
* For a press of the toolbar buttons or their menu equivalents, just load
* a resource string and display it in the status bar.
*
* For an exit request, send ourselves a WM_CLOSE message.
*
*****************************************************************************/
VOID MWnd_OnCommand(
    HWND    hWnd,
    int     id,
    HWND    hWndCtl,
    UINT    codeNotify)
{
    HMENU                   hMenu;
    int                     nIdxFormat;
    LPSTR                    lpstr;

    if (id >= IDS_TF_FIRST && id <= IDS_TF_LAST)
    {
        if (NULL != ghWndStatus)
        {
            nIdxFormat = id - IDS_TF_FIRST;

            lpstr = (LPSTR)(grgszTimeFormats[nIdxFormat]);

            SendMessage(ghWndStatus,
                        SB_SETTEXT,
                        SB_PANE_TFMT,
                        (LPARAM)lpstr);

        }

        hMenu = GetMenu(hWnd);

        CheckMenuItem(hMenu, gnTimeFormat, MF_UNCHECKED|MF_BYCOMMAND);
        CheckMenuItem(hMenu, id, MF_CHECKED|MF_BYCOMMAND);

        gnTimeFormat = id;

        // Force time window to update font and repaint entire time string

        if(ghWndTime)
            // for when WM_COMMAND is called before WM_CREATE
            FORWARD_WM_SIZE(ghWndTime, SIZE_RESTORED, 0, 0, SendMessage);
    }
    else if (id >= IDM_DEVMIN && id <= IDM_DEVMAX)
    {
        hMenu = GetMenu(hWnd);

        // If new port checked
        if (id != MIDIPerf.iMenuPos)
        {
            CheckMenuItem(hMenu, MIDIPerf.iMenuPos, MF_UNCHECKED|MF_BYCOMMAND);

            if (MIDIPerf.pDMusSegment)
                StopSegment();

            if (MIDIPerf.pDMusPort)
            {
                ReleasePort();
            }

            //Save menu position of new port
            MIDIPerf.iMenuPos = id;

            if (!(CreateAndAddPort(hMenu)))
            {
                MessageBox(hWnd,
                    "CreateAndAddPort failed!", 
                    "DMusMIDI Error", 
                    MB_ICONERROR | MB_OK);
            }

            CheckMenuItem(hMenu, id, MF_CHECKED|MF_BYCOMMAND);
        }
    }
    else switch(id)
    {
        case IDM_OPEN:
            MWnd_OnFileOpen(hWnd);
            break;
        
        case IDM_REVERB:
            ToggleReverb(hWnd);
            break;

        case IDM_TOOLBAR:
        case IDM_STATUS:
            MWnd_OnCommandToggleChild(hWnd, id);
            break;

        case IDM_AUTOPLAY:
            gbAutoPlay = !gbAutoPlay;
            CheckMenuItem(GetMenu(hWnd),
                          IDM_AUTOPLAY,
                          MF_BYCOMMAND|(gbAutoPlay ? MF_CHECKED : MF_UNCHECKED));
            break;

        case IDM_PLAY:
            FORWARD_WM_COMMAND(ghWndTime, IDM_PLAY, 0, 0, SendMessage);
        
            if (MIDIPerf.uState != SEG_S_OPENED)
            {
                DPF(1, 
                "IDM_PLAY: State was %u when IDM_PLAY was received", 
                MIDIPerf.uState);
            }
            else
            {
                DPF(4, "Calling PlaySegment.");
                PlaySegment(hWnd);
            }
            break;

        case IDM_STOP:
            FORWARD_WM_COMMAND(ghWndTime, IDM_STOP, 0, 0, SendMessage);

            StopSegment();
            SyncUI(hWnd);

            break;

        case IDM_PAUSE:
            if (MIDIPerf.uState == SEG_S_PAUSED)
            {
                if(SUCCEEDED(MIDIPerf.pDMusPerformance->lpVtbl->PlaySegment(
                                                        MIDIPerf.pDMusPerformance,
                                                        MIDIPerf.pDMusSegment,
                                                        DMUS_SEGF_BEAT,
                                                        0,
                                                        &MIDIPerf.pDMusSegmentState)))
                {
                    // Find out the performance time when segment began playing and
                    // convert to reference time. This way if the tempo changes later
                    // in the file, we will still have the correct reference start time
                    MIDIPerf.pDMusSegmentState->lpVtbl->GetStartTime(
                                        MIDIPerf.pDMusSegmentState,
                                        &MIDIPerf.mtStart);                                                

                    MIDIPerf.pDMusPerformance->lpVtbl->MusicToReferenceTime(
                        MIDIPerf.pDMusPerformance,
                        MIDIPerf.mtStart,
                        &MIDIPerf.rtStart);
                    
                    MIDIPerf.uState = SEG_S_PLAYING;
                    SyncUI(hWnd);
                }
            }
            else if (MIDIPerf.uState == SEG_S_PLAYING)
            {
                PauseSegment(hWnd);
            }
            else
            {
                DPF(1, 
                    "IDM_PAUSE: State was %u when IDM_PAUSE was received", 
                    MIDIPerf.uState);
            }
            break;

        case IDM_EXIT:
            SendMessage(hWnd, WM_CLOSE, 0, 0L);
            break;
    }
}

/*****************************************************************************
*
* MWnd_OnDestroy
*
* Handle WM_DESTROY message to main application window.
*
* HWND hWnd                 - Window handle
*
* Our main application window has been closed. PostQuitMessage so the main
* message loop will exit and the app can terminate.
*
*****************************************************************************/
VOID MWnd_OnDestroy(HWND hWnd)
{
    int             nIdx;
    HMENU           hMenu;
    HMENU           hMenuPlayThru;
    MENUITEMINFO    menuItemInfo;

    // Free the port caps structure stored in the menu item info structure
    hMenu = GetMenu(hWnd);
    hMenuPlayThru = GetSubMenu(hMenu, POS_PLAYTHRU);
    nIdx = GetMenuItemCount(hMenuPlayThru);

    while (nIdx > 0)
    {
        nIdx--; // are menu item position 0 based?? TCB

        if (GetMenuItemInfo(hMenuPlayThru,
                            IDM_DEVICES + nIdx,
                            FALSE,
                            (LPMENUITEMINFO)&menuItemInfo))
        {
            LocalFree((HLOCAL)menuItemInfo.dwItemData);
        }
    }

    PostQuitMessage(0);
}

/*****************************************************************************
*
* MWnd_WndProc
*
* Window procedure for main application window.
*
* HWND hWnd                 - Window handle
* UINT msg                  - Message code
* WPARAM wParam             - Message specific parameter
* LPARAM lParam             - Message specific parameter
*
* Dispatch messages we care about to the appropriate handler, else just
* call DefWindowProc.
*
*****************************************************************************/
LRESULT CALLBACK MWnd_WndProc(
    HWND                    hWnd,
    UINT                    msg,
    WPARAM                  wParam,
    LPARAM                  lParam)
{
    DMUS_NOTIFICATION_PMSG    *pperfmsg;

    switch(msg)
    {
        HANDLE_MSG(hWnd, WM_CREATE,         MWnd_OnCreate);
        HANDLE_MSG(hWnd, WM_GETMINMAXINFO,  MWnd_OnGetMinMaxInfo);
        HANDLE_MSG(hWnd, WM_SIZE,           MWnd_OnSize);
        HANDLE_MSG(hWnd, WM_PAINT,          MWnd_OnPaint);
        HANDLE_MSG(hWnd, WM_DROPFILES,      MWnd_OnDropFiles);
        HANDLE_MSG(hWnd, WM_COMMAND,        MWnd_OnCommand);
        HANDLE_MSG(hWnd, WM_DESTROY,        MWnd_OnDestroy);

        case WM_GETPMSG:
            if (MIDIPerf.pDMusPerformance && MIDIPerf.pDMusSegment)
            {
                // Get the notification PMsg
                if ((MIDIPerf.pDMusPerformance->lpVtbl->GetNotificationPMsg(
                                                    MIDIPerf.pDMusPerformance,
                                                    &pperfmsg)== S_OK))
                {
                    if (IsEqualGUID(&pperfmsg->guidNotificationType, &GUID_NOTIFICATION_SEGMENT))
                    {
                        if (pperfmsg->dwNotificationOption == DMUS_NOTIFICATION_SEGEND)
                        {
                            // Segment has stopped. Forward STOP command to the main window
                            FORWARD_WM_COMMAND(hWnd, IDM_STOP, 0, 0, SendMessage);
                        }
                    }

                    MIDIPerf.pDMusPerformance->lpVtbl->FreePMsg(
                        MIDIPerf.pDMusPerformance,
                        (DMUS_PMSG *)pperfmsg);
                }
            }
            break;

        default:
            return DefWindowProc(hWnd, msg, wParam, lParam);
    }

    return 0;
}



