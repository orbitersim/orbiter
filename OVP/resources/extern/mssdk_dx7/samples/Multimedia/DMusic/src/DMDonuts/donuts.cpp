/*==========================================================================
*
*  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
*
*  File:       donuts.cpp
*  Content:    Shoot-em-up game
*
*
***************************************************************************/
#define INITGUID
#define USE_DSOUND // hardwire the ability to use DSound

#include "donuts.h"
/////////// DMUSIC BEGIN ///////////
#include <oleauto.h>
#include <dmusicc.h>
#include <dmusici.h>
#define _SOFTWARE_SYNTH_ // hardwire the synth configuration
/////////// DMUSIC END /////////////


LPDIRECTDRAWSURFACE     lpFrontBuffer;
LPDIRECTDRAWSURFACE     lpBackBuffer;
LPDIRECTDRAWSURFACE     lpDonut;
LPDIRECTDRAWSURFACE     lpPyramid;
LPDIRECTDRAWSURFACE     lpCube;
LPDIRECTDRAWSURFACE     lpSphere;
LPDIRECTDRAWSURFACE     lpShip;
LPDIRECTDRAWSURFACE     lpNum;
LPDIRECTDRAW            lpDD;
LPDIRECTDRAWPALETTE     lpArtPalette;
LPDIRECTDRAWPALETTE     lpSplashPalette;
BOOL                    bSoundEnabled = FALSE;
BOOL                    bPlayIdle = FALSE;
BOOL                    bPlayBuzz = FALSE;
BOOL                    bPlayRev = FALSE;
BOOL                    lastThrust = FALSE;
BOOL                    lastShield = FALSE;
int                     showDelay = 0;
HWND                    hWndMain;
HACCEL                  hAccel;
HINSTANCE               hInst;
BOOL                    bShowFrameCount=TRUE;
BOOL                    bIsActive;
BOOL                    bMouseVisible;
DWORD                   dwFrameCount;
DWORD                   dwFrameTime;
DWORD                   dwFrames;
DWORD                   dwFramesLast;
BOOL                    bUseEmulation;
BOOL                    bTest=FALSE;
BOOL                    bStress=FALSE;
DWORD                   dwTransType;
RGBQUAD                 SPalette[256];
DWORD                   lastTickCount;
int                     score;
int                     ProgramState;
int                     level;
int                     restCount;
DWORD                   dwFillColor;
BOOL                    bSpecialEffects = FALSE;
int                     iForceErase = 0;
DWORD                   ShowLevelCount = 3000;
DWORD                   ScreenX;
DWORD                   ScreenY;
DWORD                   ScreenBpp;
BOOL                    bWantSound = TRUE;  //global hack to turn off sound

/////////// DMUSIC BEGIN ///////////
// Boolean for extra play option
BOOL                    bMusicEnabled = TRUE;

// Various constants
const   GUID guidZero =     {0};
#define MOTIF_BOUNCE        0
#define MOTIF_DEATH         1
#define MOTIF_SHIELD        2
#define MOTIF_BLOWUP        4
#define MOTIF_BLOWUPLITE    3
#define NUM_MOTIFS          5
#define NUM_STYLES          2
#define SEGMENT_1           0
#define SEGMENT_2           1
#define SEGMENT_TRANS_1     2
#define SEGMENT_TRANS_2     3
#define NUM_SEGMENTS        4
#define NUM_CHORDMAP        4
#define BLOWUPS_PER_BEAT    2
#define MEASURE_LENGTH      DMUS_PPQ * 4 // parts/quarter * quarters/measure (assumes 4/4)

// Global Interfaces
IDirectMusicStyle*          gapStyle[NUM_STYLES];
IDirectMusicChordMap*       gapChordMap[NUM_STYLES] [NUM_CHORDMAP];
IDirectMusicSegment*        gapMotif[NUM_STYLES] [NUM_MOTIFS];
IDirectMusicSegment*        gapSegment[NUM_SEGMENTS] = {NULL, NULL, NULL, NULL};
IDirectMusicComposer*       gpComposer = NULL;
IDirectMusicPerformance*    gpPerformance = NULL;
IDirectMusic*               gpDirectMusic = NULL;
IDirectMusicLoader*         gpLoader = NULL;
IDirectMusicSegment*        gpIntroTemplate = NULL;
IDirectMusicSegment*        gpGameTemplate = NULL;
IDirectMusicSegment*        gapShieldSegment[NUM_STYLES] = {NULL, NULL};
IDirectMusicSegment*        gapDefaultSegment[NUM_STYLES] = {NULL, NULL};
IDirectMusicBand*           gapShieldBand[NUM_STYLES] = {NULL, NULL};
IDirectMusicBand*           gapDefaultBand[NUM_STYLES] = {NULL, NULL};

// Global variables
BOOL                        bAnyHits = FALSE;
int                         gnCurrentStyle = 0;
int                         gnCurrentChordMap = 0;
int                         gnLastStyle = 0;
int                         gnLastChordMap = 0;

BOOL                        gbShieldsOn = FALSE;
static int                  snLastTempo;
static int                  snSubLevel;
static int                  snMaxBlowUps = BLOWUPS_PER_BEAT;

void CleanUpDMusic();

/////////// DMUSIC END /////////////

int getint(char**p, int def);

#ifdef DEBUG
char                    DebugBuf[256];
BOOL                    bHELBlt = FALSE;
#endif

DBLNODE                 DL;             // Display List

#ifdef USE_DSOUND
LPDIRECTSOUND           lpDS              = NULL;
HSNDOBJ                 hsoBeginLevel     = NULL;
HSNDOBJ                 hsoEngineIdle     = NULL;
HSNDOBJ                 hsoEngineRev      = NULL;
HSNDOBJ                 hsoSkidToStop     = NULL;
HSNDOBJ                 hsoShieldBuzz     = NULL;
HSNDOBJ                 hsoShipExplode    = NULL;
HSNDOBJ                 hsoFireBullet     = NULL;
HSNDOBJ                 hsoShipBounce     = NULL;
HSNDOBJ                 hsoDonutExplode   = NULL;
HSNDOBJ                 hsoPyramidExplode = NULL;
HSNDOBJ                 hsoCubeExplode    = NULL;
HSNDOBJ                 hsoSphereExplode  = NULL;
#endif


/////////// DMUSIC BEGIN /////////////
void ComposeNewSegments()
{
    // Set up new segments
    snSubLevel = 1;
    snLastTempo = 0;
    gnLastStyle = gnCurrentStyle;
    gnLastChordMap = gnCurrentChordMap;
    gnCurrentStyle = level % NUM_STYLES;
    gnCurrentChordMap = ((level - 1) / 2) % NUM_CHORDMAP;
    HRESULT hr = S_OK;
    MUSIC_TIME mtSegmentLength;

    if (gapSegment[SEGMENT_1])
    {
        gapSegment[SEGMENT_1]->Release();
    }

    hr = gpComposer->ComposeSegmentFromTemplate(
        gapStyle[gnCurrentStyle],
        gpIntroTemplate, 0, 
        gapChordMap[gnCurrentStyle][gnCurrentChordMap],
        &gapSegment[SEGMENT_1]
        );

    if (!SUCCEEDED(hr))
    {
        CleanupAndExit("Segment 1 composition failed");
    }

    gapSegment[SEGMENT_1]->GetLength(&mtSegmentLength);
    gapSegment[SEGMENT_1]->SetLength(mtSegmentLength - MEASURE_LENGTH);
    gapSegment[SEGMENT_1]->SetRepeats(999);

    if (gapSegment[SEGMENT_2])
    {
        gapSegment[SEGMENT_2]->Release();
    }

    hr = gpComposer->ComposeSegmentFromTemplate(
        gapStyle[gnCurrentStyle],
        gpGameTemplate, 0, 
        gapChordMap[gnCurrentStyle][gnCurrentChordMap],
        &gapSegment[SEGMENT_2]
        );

    if (!SUCCEEDED(hr))
    {
        CleanupAndExit("Segment 2 composition failed");
    }

    gapSegment[SEGMENT_2]->GetLength(&mtSegmentLength);
    gapSegment[SEGMENT_2]->SetLength(mtSegmentLength - MEASURE_LENGTH);
    gapSegment[SEGMENT_2]->SetRepeats(999);

    bAnyHits = FALSE;
}


BOOL TransitionDone() // only called if music is enabled...
{
    if (gapSegment[SEGMENT_TRANS_2]) // NULL on startup
    {
        return (gpPerformance->IsPlaying(gapSegment[SEGMENT_1], NULL) == S_OK);
    }
    else
    {
        return ( ( GetTickCount() - restCount ) > ShowLevelCount );
    }
}
/////////// DMUSIC END /////////////

void setup_game(void)
{
    restCount = GetTickCount();
    initLevel( ++level );
    /////////////// DMUSIC BEGIN /////////////////
    ComposeNewSegments();
    gpPerformance->PlaySegment(
        gapSegment[SEGMENT_1], 0, 0, NULL);
    gpPerformance->PlaySegment(
        gapDefaultSegment[gnCurrentStyle], DMUS_SEGF_SECONDARY, 0, NULL);
    /////////////// DMUSIC END /////////////////
                // set the palette
    lpFrontBuffer->SetPalette( lpArtPalette ); 
}

/*
*
*  AppPause
*
*/
void AppPause(void)
{
    lpDD->FlipToGDISurface();
    DrawMenuBar(hWndMain);
    RedrawWindow(hWndMain, NULL, NULL, RDW_FRAME);
    
}

/*
*
*  AppUnpause
*
*  Reset the various time counters so the donuts don't suddenly
*  jump halfways across the screen and so the frame rate remains accurate.
*
*/
void AppUnpause(void)
{
    iForceErase = 2;
    lastTickCount = dwFrameTime = timeGetTime();
}

/*
*  CheckOneMenuItem
*
*  Helper function that checks a single item in a menu.
*/
void CheckOneMenuItem(HMENU hmenu, UINT idc, BOOL fCheck)
{
    CheckMenuItem(hmenu, idc,
        fCheck ? (MF_BYCOMMAND | MF_CHECKED)
        : (MF_BYCOMMAND | MF_UNCHECKED));
}

/*
*  CheckMenuItems
*
*  Sync the menu checkmarks with our internal variables
*/
void CheckMenuItems(HWND hwnd)
{
    HMENU hmenu = GetMenu(hwnd);
    
    CheckOneMenuItem(hmenu, IDC_TRAILS, bSpecialEffects);
#ifdef USE_DSOUND
    CheckOneMenuItem(hmenu, IDC_AUDIO, bWantSound && bSoundEnabled);
#endif
    CheckOneMenuItem(hmenu, IDC_FRAMERATE, bShowFrameCount);
    
    
}

/*
* MainWndproc
*
* Callback for all Windows messages
*/
long FAR PASCAL MainWndproc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
    PAINTSTRUCT ps;
    HDC         hdc;
    UINT        cmd;
    
    switch( message )
    {
    case WM_ACTIVATEAPP:
        bIsActive = (BOOL) wParam;
        if( bIsActive )
        {
            bMouseVisible = FALSE;
            lastTickCount = GetTickCount();
            bSpecialEffects = FALSE;
            // we are active, need to reacquire the keyboard
            ReacquireInput();
        }
        else
        {
            bMouseVisible = TRUE;
            // DirectInput automatically unacquires for us in FOREGROUND mode
        }
        break;
        
    case WM_CREATE:
        break;
        
    case WM_SETCURSOR:
        if( !bMouseVisible )
        {
            SetCursor(NULL);
        }
        else
        {
            SetCursor(LoadCursor( NULL, IDC_ARROW ));
        }
        return TRUE;
        
    case WM_COMMAND:
        cmd = GET_WM_COMMAND_ID(wParam, lParam);
        
        if (cmd >= IDC_DEVICES && cmd < IDC_DEVICES + 100) {
            PickInputDevice(cmd - IDC_DEVICES);
        } 
        else 
        {
            switch (cmd) 
            {           
                case IDC_FRAMERATE:
                    bShowFrameCount = !bShowFrameCount;
                    if( bShowFrameCount )
                    {
                        dwFrameCount = 0;
                        dwFrameTime = timeGetTime();
                    }
                    break;
                    ////////// DMUSIC BEGIN /////////
                case IDC_MUSIC:
                    if (bMusicEnabled)
                    {
                        gpPerformance->Stop( NULL, NULL, 0, 0 );
                    }
                    else
                    {
                        short nSegment = (snSubLevel == 1) ? SEGMENT_1 : SEGMENT_2;
                        gpPerformance->PlaySegment(gapSegment[nSegment], 0, 0, NULL);
                    }
                    bMusicEnabled = !bMusicEnabled;
                    break;
                    ////////// DMUSIC END /////////
                case IDC_STARTGAME:
                    if( ProgramState == PS_SPLASH )
                    {
                        ProgramState = PS_BEGINREST;
                        setup_game();
                    }
                    break;
        
                case IDC_QUIT:
                    PostMessage(hWnd, WM_CLOSE, 0, 0);
                    return 0;
        
                case IDC_AUDIO:
#ifdef USE_DSOUND
                    if(bWantSound)
                    {
                        if( bSoundEnabled )
                        {
                            DestroySound();
                        }
                        else
                        {
                            InitializeSound();
                        }
                    }
#endif
                    break;
        
                case IDC_TRAILS:
                    bSpecialEffects = !bSpecialEffects;
                    break;
            }
        }
        break;
        
    case WM_INITMENU:
        CheckMenuItems(hWndMain);
        break;
        
    case WM_ERASEBKGND:
        return 1;
        
    case WM_PAINT:
        hdc = BeginPaint( hWnd, &ps );
        EndPaint( hWnd, &ps );
        return 1;
        
    case WM_DESTROY:
        CleanupInput();
        DestroyGame();
        PostQuitMessage( 0 );
        break;
        
    case WM_ENTERMENULOOP:
        AppPause();
        break;
        
    case WM_EXITMENULOOP:
        AppUnpause();
        break;
        
    default:
        break;
    }

    return DefWindowProc(hWnd, message, wParam, lParam);
    
} /* MainWndproc */

// initApplication
// Do that Windows initialization stuff...

static BOOL initApplication( HINSTANCE hInstance, int nCmdShow )
{
    WNDCLASS    wc;
    BOOL        rc;
    
    wc.style = CS_DBLCLKS;
    wc.lpfnWndProc = MainWndproc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon( hInstance, MAKEINTRESOURCE(DONUTS_ICON));
    wc.hCursor = LoadCursor( NULL, IDC_ARROW );
    wc.hbrBackground = (HBRUSH)GetStockObject( BLACK_BRUSH );
    wc.lpszMenuName =  MAKEINTRESOURCE(DONUTS_MENU);
    wc.lpszClassName = "DonutsClass";
    rc = RegisterClass( &wc );
    if( !rc )
    {
        return FALSE;
    }
    
    hAccel = LoadAccelerators(hInstance, MAKEINTRESOURCE(DONUTS_ACCEL));
    if ( !hAccel )
    {
        return FALSE;
    }
    
    hWndMain = CreateWindowEx(0,  // WS_EX_TOPMOST,
        "DonutsClass",
        "Donuts",
        WS_VISIBLE | // so we don't have to call ShowWindow
        WS_POPUP |   // non-app window
        WS_CAPTION | // so our menu doesn't look ultra-goofy
        WS_SYSMENU,  // so we get an icon in the tray
        0,
        0,
        GetSystemMetrics(SM_CXSCREEN),
        GetSystemMetrics(SM_CYSCREEN),
        NULL,
        NULL,
        hInstance,
        NULL );
    
    if( !hWndMain )
    {
        return FALSE;
    }
    
    UpdateWindow( hWndMain );
    
#ifdef USE_DSOUND
    /*
    *  If sound is globally disabled, then disable the sound menu.
    */
    if (!bWantSound) {
        EnableMenuItem(GetMenu(hWndMain), IDC_AUDIO,
            MF_BYCOMMAND | MF_DISABLED | MF_GRAYED);
    }
#endif
    
    return TRUE;
    
} /* initApplication */

  /*
  * WinMain
*/
int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
                   int nCmdShow )
{
    MSG     msg;
    
    // save off application instance
    hInst = hInstance;
    
    while( lpCmdLine[0] == '-' )
    {
        lpCmdLine++;
        
        switch (*lpCmdLine++)
        {
        case 'e':
            bUseEmulation = TRUE;
            break;
        case 't':
            bTest = TRUE;
            break;
        case 'S':
            bWantSound = FALSE;
            break;
        case 'x':
            bStress= TRUE;
            bTest = TRUE;
            break;
        }
        while( IS_SPACE(*lpCmdLine) )
        {
            lpCmdLine++;
        }
    }
    
    ScreenX = getint(&lpCmdLine, 640);
    ScreenY = getint(&lpCmdLine, 480);
    ScreenBpp = getint(&lpCmdLine, 8);
    
    if( !initApplication(hInstance, nCmdShow) )
    {
        OutputDebugString( "initApplication() failed!\n" );
        return FALSE;
    }
    
    if( !InitializeGame() )
    {
        OutputDebugString( "InitializeGame() failed!\n" );
        DestroyWindow( hWndMain );
        return FALSE;
    }
    
    dwFrameTime = timeGetTime();
    
#ifdef USE_DSOUND
    DestroySound();
#endif
    
    while( 1 )
    {
        if( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
        {
            if( !GetMessage( &msg, NULL, 0, 0 ) )
            {
                ////////// DMUSIC BEGIN /////////
                CleanUpDMusic();
                ////////// DMUSIC END /////////
                return msg.wParam;
            }
            if ( !TranslateAccelerator( hWndMain, hAccel, &msg ) ) {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
        else if ( bIsActive )
        {
            UpdateFrame();
        }
        else
        {
            WaitMessage();
        }
        /////////////////// DMUSIC BEGIN //////////////////
        if( gpPerformance )
        {
            DMUS_NOTIFICATION_PMSG* pEvent;
            while ( S_OK == gpPerformance->GetNotificationPMsg(&pEvent))
            {
                gpPerformance->FreePMsg((DMUS_PMSG*)pEvent);
                snMaxBlowUps = BLOWUPS_PER_BEAT;
            }
        }
        /////////////////// DMUSIC END //////////////////
    }
    
} /* WinMain */


void DestroyGame( void )
{
#ifdef USE_DSOUND
    if (lpDS)
    {
        lpDS->Release(); 
        lpDS = NULL;
    }
#endif
}

BOOL InitializeGame( void )
{
    DDCAPS          ddcaps;
    HRESULT         ddrval;
    DDSURFACEDESC   ddsd;
    DDSCAPS         ddscaps;
    WCHAR         wszSearchPath[MAX_PATH];
#ifdef NT_HACK
    DDSURFACEDESC DDSurfDesc;
#endif
    
#ifdef USE_DSOUND
    if(bWantSound)
    {
        InitializeSound();
    }
#endif
    //////////////// DMUSIC BEGIN //////////////////
    CoInitialize(NULL);
    
    if( !SUCCEEDED(::CoCreateInstance(
        CLSID_DirectMusicComposer,
        NULL,
        CLSCTX_INPROC, 
        IID_IDirectMusicComposer,
        (void**)&gpComposer
        )))
    {
        return CleanupAndExit("Couldn't create a composer object");
    }
    
    if( !SUCCEEDED( CoCreateInstance( CLSID_DirectMusicPerformance, 
        NULL, 
        CLSCTX_INPROC, 
        IID_IDirectMusicPerformance,
        (void**)&gpPerformance )))
    {
        return CleanupAndExit("Couldn't create a performance object");
    }
    
#ifdef USE_DSOUND
    if( !SUCCEEDED(gpPerformance->Init(&gpDirectMusic, lpDS, NULL)))
    {
        return CleanupAndExit("Couldn't initialize the performance");
    }
#else
    if( !SUCCEEDED(gpPerformance->Init(&gpDirectMusic, NULL, hWndMain)))
    {
        return CleanupAndExit("Couldn't initialize the performance");
    }
#endif
    
    IDirectMusicPort* pPort = NULL;
    DMUS_PORTPARAMS dmos;
    DMUS_PORTCAPS dmpc;
    GUID guidSynthGUID;
    HRESULT hr = S_OK;
    
    if( !SUCCEEDED(gpDirectMusic->GetDefaultPort(&guidSynthGUID)))
    {
        return CleanupAndExit("Could't GetDefaultPort on IDirectMusic");
    }
    
    ZeroMemory(&dmos, sizeof(dmos));
    dmos.dwSize = sizeof(DMUS_PORTPARAMS);
    dmos.dwChannelGroups = 1;
    dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS;
    
    if( !SUCCEEDED(gpDirectMusic->CreatePort(guidSynthGUID,
        &dmos,
        &pPort,
        NULL)))
    {
        return CleanupAndExit("Couldn't CreatePort on IDirectMusic");
    }
    
    ZeroMemory(&dmpc, sizeof(dmpc));
    dmpc.dwSize = sizeof(DMUS_PORTCAPS);
    
    if( !SUCCEEDED(pPort->GetCaps(&dmpc)))
    {
        if (pPort) pPort->Release();
        return CleanupAndExit("Couldn't GetCaps on IDirectMusicPort");
    }
    
#ifdef _SOFTWARE_SYNTH_
    if ((dmpc.dwClass != DMUS_PC_OUTPUTCLASS) || !(dmpc.dwFlags & DMUS_PC_DLS))
    {
        pPort->Release();
        pPort = NULL;
    }
    
    if (!pPort)
    {
        for(DWORD index = 0; hr == S_OK; index++)
        {
            ZeroMemory(&dmpc, sizeof(dmpc));
            dmpc.dwSize = sizeof(DMUS_PORTCAPS);
            
            hr = gpDirectMusic->EnumPort(index, &dmpc);
            if(hr == S_OK)
            {
                if ( (dmpc.dwClass == DMUS_PC_OUTPUTCLASS) && 
                    (dmpc.dwFlags & DMUS_PC_DLS) )
                {
                    CopyMemory(&guidSynthGUID, &dmpc.guidPort, sizeof(GUID));
                    
                    ZeroMemory(&dmos, sizeof(dmos));
                    dmos.dwSize = sizeof(DMUS_PORTPARAMS);
                    dmos.dwChannelGroups = 1;
                    dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS;
                    
                    hr = gpDirectMusic->CreatePort(guidSynthGUID, &dmos, &pPort, NULL);
                    break;
                }
            }
        }
        if (hr != S_OK)
        {
            if (pPort) pPort->Release();
            return CleanupAndExit("Couldn't initialize the Synth port");
        }
    }
#else
    if( 0 != _wcsicmp( dmpc.wszDescription, L"MIDI Mapper" ) )
    {
        pPort->Release();
        pPort = NULL;
    }
    
    if (!pPort)
    {
        for(DWORD index = 0; hr == S_OK; index++)
        {
            ZeroMemory(&dmpc, sizeof(dmpc));
            dmpc.dwSize = sizeof(DMUS_PORTCAPS);
            
            hr = gpDirectMusic->EnumPort(index, &dmpc);
            if(hr == S_OK)
            {
                if( 0 == _wcsicmp( dmpc.wszDescription, L"MIDI Mapper" ) )
                {
                    CopyMemory(&guidSynthGUID, &dmpc.guidPort, sizeof(GUID));
                    
                    ZeroMemory(&dmos, sizeof(dmos));
                    dmos.dwSize = sizeof(DMUS_PORTPARAMS);
                    dmos.dwChannelGroups = 1;
                    dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS;
                    
                    hr = gpDirectMusic->CreatePort(guidSynthGUID, &dmos, &pPort, NULL);
                    break;
                }
            }
        }
        if (hr != S_OK)
        {
            if (pPort) pPort->Release();
            return CleanupAndExit("Couldn't initialize the MIDI Mapper port");
        }
    }
#endif
    
    pPort->Activate(TRUE);
    gpPerformance->AddPort(pPort );
    gpPerformance->AssignPChannelBlock(0, pPort, 1);
    if (pPort) pPort->Release();
    if (!SUCCEEDED(::CoCreateInstance(
        CLSID_DirectMusicLoader,
        NULL,
        CLSCTX_INPROC, 
        IID_IDirectMusicLoader,
        (void**)&gpLoader
        )))
    {
        return CleanupAndExit("Couldn't create a loader object");
    }
    
    hr = E_FAIL;
    
    if (GetSearchPath(wszSearchPath))
    {
        hr = gpLoader->SetSearchDirectory(GUID_DirectMusicAllTypes, wszSearchPath, FALSE);
    }
    
    if (FAILED(hr))
    {
        hr = gpLoader->SetSearchDirectory(GUID_DirectMusicAllTypes, L".", FALSE);
    }
    
    if (FAILED(hr))
    {
        return CleanupAndExit("Couldn't set the search diretory for the DirectMusic loader");
    }
    
    
    gpLoader->EnableCache(GUID_DirectMusicAllTypes, TRUE);
    DMUS_OBJECTDESC ObjectDescript;
    
    ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
    ObjectDescript.guidClass = CLSID_DirectMusicStyle;
    
    wcscpy(ObjectDescript.wszName, L"Donuts");
    ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_NAME;
    if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicStyle, (void**)&gapStyle[1])))
    {
        wcscpy(ObjectDescript.wszFileName, L"Donuts.sty");
        ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
        if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicStyle, (void**)&gapStyle[1])))
        {
            return CleanupAndExit("Couldn't load style object 1");
        }
    }
    
    wcscpy(ObjectDescript.wszName, L"Donutz");
    ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_NAME;
    if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicStyle, (void**)&gapStyle[0])))
    {
        wcscpy(ObjectDescript.wszFileName, L"Donutz.sty");
        ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
        if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicStyle, (void**)&gapStyle[0])))
        {
            return CleanupAndExit("Couldn't load style object 2");
        }
    }
    
    ObjectDescript.guidClass = CLSID_DirectMusicSegment;
    ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
    
    wcscpy(ObjectDescript.wszFileName, L"donuts1.tpl");
    if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicSegment, (void**)&gpIntroTemplate)))
    {
        return CleanupAndExit("Couldn't load template object 1");
    }
    
    wcscpy(ObjectDescript.wszFileName, L"donuts2.tpl");
    if (!SUCCEEDED(gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicSegment, (void**)&gpGameTemplate)))
    {
        return CleanupAndExit("Couldn't load template object 2");
    }
    
    hr = S_OK;
    WCHAR awszMotifs[NUM_MOTIFS][64];
    wcscpy(awszMotifs[MOTIF_BOUNCE], L"Bounce");
    wcscpy(awszMotifs[MOTIF_SHIELD], L"Shields");
    wcscpy(awszMotifs[MOTIF_DEATH], L"Death");
    wcscpy(awszMotifs[MOTIF_BLOWUPLITE], L"Blowuplite");
    wcscpy(awszMotifs[MOTIF_BLOWUP], L"Blowup");
    
    hr = gapStyle[0]->GetMotif(awszMotifs[MOTIF_BOUNCE], &(gapMotif[0][MOTIF_BOUNCE]));
    if (hr == S_OK)
        hr = gapStyle[0]->GetMotif(awszMotifs[MOTIF_SHIELD], &(gapMotif[0][MOTIF_SHIELD]));
    if (hr == S_OK)
        hr = gapStyle[0]->GetMotif(awszMotifs[MOTIF_DEATH], &(gapMotif[0][MOTIF_DEATH]));
    if (hr == S_OK)
        hr = gapStyle[0]->GetMotif(awszMotifs[MOTIF_BLOWUPLITE], &(gapMotif[0][MOTIF_BLOWUPLITE]));
    if (hr == S_OK)
        hr = gapStyle[0]->GetMotif(awszMotifs[MOTIF_BLOWUP], &(gapMotif[0][MOTIF_BLOWUP]));
    
    if (hr == S_OK)
        hr = gapStyle[1]->GetMotif(awszMotifs[MOTIF_BOUNCE], &(gapMotif[1][MOTIF_BOUNCE]));
    if (hr == S_OK)
        hr = gapStyle[1]->GetMotif(awszMotifs[MOTIF_SHIELD], &(gapMotif[1][MOTIF_SHIELD]));
    if (hr == S_OK)
        hr = gapStyle[1]->GetMotif(awszMotifs[MOTIF_DEATH], &(gapMotif[1][MOTIF_DEATH]));
    if (hr == S_OK)
        hr = gapStyle[1]->GetMotif(awszMotifs[MOTIF_BLOWUPLITE], &(gapMotif[1][MOTIF_BLOWUPLITE]));
    if (hr == S_OK)
        hr = gapStyle[1]->GetMotif(awszMotifs[MOTIF_BLOWUP], &(gapMotif[1][MOTIF_BLOWUP]));
    
    if (hr != S_OK)
    {
        return CleanupAndExit("Couldn't find a Motif");
    }
    
    WCHAR awszChordMap[NUM_STYLES][NUM_CHORDMAP][64];
    wcscpy(awszChordMap[0][0], L"minaeo.cdm");
    wcscpy(awszChordMap[0][1], L"minfunc.cdm");
    wcscpy(awszChordMap[0][2], L"mipedpt.cdm");
    wcscpy(awszChordMap[0][3], L"tension.cdm");
    wcscpy(awszChordMap[1][0], L"dianoble.cdm");
    wcscpy(awszChordMap[1][1], L"minpedpt.cdm");
    wcscpy(awszChordMap[1][2], L"mippjazz.cdm");
    wcscpy(awszChordMap[1][3], L"minjazz.cdm");
    
    ObjectDescript.guidClass = CLSID_DirectMusicChordMap;
    ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
    for (short n = 0; n < NUM_STYLES; n++)
    {
        for (short m = 0; m < NUM_CHORDMAP; m++)
        {
            if (hr == S_OK)
            {
                wcscpy(ObjectDescript.wszFileName, awszChordMap[n][m]);
                hr = gpLoader->GetObject(&ObjectDescript, IID_IDirectMusicChordMap, (void**)&gapChordMap[n][m]);
            }
        }
    }
    
    if (hr != S_OK)
    {
        return CleanupAndExit("Couldn't load a ChordMap");
    }
    
    BSTR bstrDefault = SysAllocString(L"Default 2");
    BSTR bstrShields = SysAllocString(L"Shields");
    
    for (n = 0; n < NUM_STYLES; n++)
    {
        if (hr == S_OK)
        {
            hr = gapStyle[n]->GetBand(bstrShields, &gapShieldBand[n]);
        }
        if (hr == S_OK)
        {
            hr = gapShieldBand[n]->Download(gpPerformance);
        }
        if (hr == S_OK)
        {
            hr = gapStyle[n]->GetBand(bstrDefault, &gapDefaultBand[n]);
        }
        if (hr == S_OK)
        {
            hr = gapDefaultBand[n]->Download(gpPerformance);
        }
    }
    
    SysFreeString(bstrDefault);
    SysFreeString(bstrShields);
    
    if (hr != S_OK)
    {
        return CleanupAndExit("Couldn't find a band");
    }
    
    for (n = 0; n < NUM_STYLES; n++)
    {
        if (hr == S_OK)
        {
            hr = gapShieldBand[n]->CreateSegment(&gapShieldSegment[n]);
        }
        if (hr == S_OK)
        {
            hr = gapDefaultBand[n]->CreateSegment(&gapDefaultSegment[n]);
        }
    }
    
    if (hr != S_OK)
    {
        return CleanupAndExit("Couldn't create a band segment");
    }
    
    GUID guid;
    guid = GUID_NOTIFICATION_SEGMENT;
    gpPerformance->AddNotificationType( guid );
    guid = GUID_NOTIFICATION_MEASUREANDBEAT;
    gpPerformance->AddNotificationType( guid );
    
    //////////////// DMUSIC END ////////////////////
    
    score = 0;
    if( bTest )
        ShowLevelCount = 1000;
    
    if( bUseEmulation )
        ddrval = DirectDrawCreate((GUID*)DDCREATE_EMULATIONONLY, &lpDD, NULL );
    else
        ddrval = DirectDrawCreate( NULL, &lpDD, NULL );
    
    if( ddrval != DD_OK )
        return CleanupAndExit("DirectDrawCreate Failed!");
    
    ddrval = lpDD->SetCooperativeLevel( hWndMain,
        DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN );
    if( ddrval != DD_OK )
        return CleanupAndExit("SetCooperativeLevel Failed");
    
#ifdef NT_HACK
    DDSurfDesc.dwSize = sizeof(DDSurfDesc);
    ddrval = lpDD->lpVtbl->GetDisplayMode(lpDD,&DDSurfDesc);
    if(ddrval == DD_OK)
        ScreenBpp = DDSurfDesc.ddpfPixelFormat.dwRGBBitCount;
#endif
    
    // set the mode
    ddrval = lpDD->SetDisplayMode( ScreenX, ScreenY, ScreenBpp );
    if( ddrval != DD_OK )
        return CleanupAndExit("SetDisplayMode Failed!");
    
    // check the color key hardware capabilities
    dwTransType = DDBLTFAST_SRCCOLORKEY;
    ddcaps.dwSize = sizeof( ddcaps );
    
#ifdef DEBUG
    if( GetProfileInt( "Donuts", "force_dest_blt", 0) )
    {
        dwTransType = DDBLTFAST_DESTCOLORKEY;
    }
    bHELBlt = GetProfileInt( "Donuts", "force_HEL_blt", bHELBlt );
#endif
    
    // Create surfaces
    memset( &ddsd, 0, sizeof( ddsd ) );
    ddsd.dwSize = sizeof( ddsd );
    ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
        DDSCAPS_FLIP |
        DDSCAPS_COMPLEX;
    ddsd.dwBackBufferCount = 1;
    ddrval = lpDD->CreateSurface( &ddsd, &lpFrontBuffer, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface FrontBuffer Failed!");
    
    // get a pointer to the back buffer
    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
    ddrval = lpFrontBuffer->GetAttachedSurface(
        &ddscaps,
        &lpBackBuffer );
    if( ddrval != DD_OK )
        return CleanupAndExit("GetAttachedDurface Failed!");
    
    ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
#ifdef DEBUG
    if( bHELBlt )
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
#endif
    ddsd.dwWidth = 320;
    ddsd.dwHeight = 384;
    ddrval = lpDD->CreateSurface( &ddsd, &lpDonut, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpDonut Failed!");
    
    ddsd.dwHeight = 128;
    ddrval = lpDD->CreateSurface( &ddsd, &lpPyramid, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpPyramid Failed!");
    
    ddsd.dwHeight = 32;
    ddrval = lpDD->CreateSurface( &ddsd, &lpCube, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpCube Failed!");
    
    ddsd.dwHeight = 32;
    ddrval = lpDD->CreateSurface( &ddsd, &lpSphere, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpSphere Failed!");
    // Set the background color fill color
    
    ddsd.dwHeight = 256;
    ddrval = lpDD->CreateSurface( &ddsd, &lpShip, NULL ); 
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpShip Failed!");
    
    ddsd.dwHeight = 16;
    ddrval = lpDD->CreateSurface( &ddsd, &lpNum, NULL );
    if( ddrval != DD_OK )
        return CleanupAndExit("CreateSurface lpNum Failed!");
    
    if( !RestoreSurfaces() )
        return CleanupAndExit("RestoreSurfaces Failed!");
    
    DL.next = DL.prev = &DL;            // null display list
    DL.type = OBJ_SHIP;
    DL.surf = lpShip;
    lastTickCount = GetTickCount();
    
    // initialize DirectInput
    if(!InitInput(hInst, hWndMain))
        return CleanupAndExit("DirectInput initialization Failed!");
    
    if(bTest)
    {
        ProgramState = PS_ACTIVE;
        setup_game();
    }
    else
    {
        ProgramState = PS_SPLASH;
    }
    return TRUE;
}

///////////////// DMUSIC BEGIN ////////////////////
void CleanUpDMusic()
{
    if (gpLoader)
    {
        gpLoader->ClearCache(GUID_DirectMusicAllTypes);
        gpLoader->Release();
    }
    
    
    if (gpComposer)
    {
        gpComposer->Release();
    }
    
    if (gpIntroTemplate)
    {
        gpIntroTemplate->Release();
    }
    
    if (gpGameTemplate)
    {
        gpGameTemplate->Release();
    }
    
    for (short n = 0; n < NUM_STYLES; n++)
    {
        if (gapShieldBand[n])
        {
            gapShieldBand[n]->Unload(gpPerformance);
            gapShieldBand[n]->Release();
        }
        if (gapDefaultBand[n])
        {
            gapDefaultBand[n]->Unload(gpPerformance);
            gapDefaultBand[n]->Release();
        }
        if (gapShieldSegment[n])
        {
            gapShieldSegment[n]->Release();
        }
        
        if (gapDefaultSegment[n])
        {
            gapDefaultSegment[n]->Release();
        }
        
        if (gapStyle[n])
        {
            gapStyle[n]->Release();
        }
        
        for (short m = 0; m < NUM_CHORDMAP; m++)
        {
            if (gapChordMap[n][m])
            {
                gapChordMap[n][m]->Release();
            }
        }
        
        for (m = 0; m < NUM_MOTIFS; m++)
        {
            if (gapMotif[n][m])
            {
                gapMotif[n][m]->Release();
            }
        }
    }
    
    if (gpPerformance)
    {
        gpPerformance->Stop( NULL, NULL, 0, 0 );
        gpPerformance->CloseDown();
        gpPerformance->Release();
    }
    
    for (n = 0; n < NUM_SEGMENTS; n++)
    {
        if (gapSegment[n])
        {
            gapSegment[n]->Release();
        }
    }
    
    if (gpDirectMusic)
    {
        gpDirectMusic->Release();
    }
    
    CoUninitialize();
}
//////////////// DMUSIC END ////////////////////

BOOL CleanupAndExit( char *err)
{
#ifdef DEBUG
    wsprintf(DebugBuf, "___CleanupAndExit  err = %s\n", err );
    OutputDebugString( DebugBuf );
#endif
    
    // make the cursor visible
    SetCursor(LoadCursor( NULL, IDC_ARROW ));
    bMouseVisible = TRUE;
    
    if( lpDonut != NULL )
        lpDonut->Release(  );
    
    if( lpPyramid != NULL )
        lpPyramid->Release(  );
    
    if( lpCube != NULL )
        lpCube->Release(  );
    
    if( lpSphere != NULL )
        lpSphere->Release(  );
    
    if( lpShip != NULL )
        lpShip->Release(  );
    
    if( lpNum != NULL )
        lpNum->Release(  );
    
    if( lpFrontBuffer != NULL )
        lpFrontBuffer->Release(  );
    
    if( lpArtPalette != NULL )
        lpArtPalette->Release(  );
    
    if( lpSplashPalette != NULL )
        lpSplashPalette->Release(  );
    
    if( lpDD != NULL )
        lpDD->Release(  );
    
    // clean up DirectInput objects
    CleanupInput();
    
    ///////////////// DMUSIC BEGIN ////////////////////
    CleanUpDMusic();
    //////////////// DMUSIC END ////////////////////
    
    //
    // warn user if there is one
    //
    
    if( !bStress )
    {
        MessageBox( hWndMain, err, "ERROR", MB_OK );
    }
    return FALSE;
}

void bltSplash( void )
{
    HRESULT     ddrval;
    HBITMAP     hbm;
    
    // set the palette before loading the splash screen
    lpFrontBuffer->SetPalette( lpSplashPalette );
    
    hbm = (HBITMAP)LoadImage( GetModuleHandle( NULL ), "SPLASH", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
    if ( NULL == hbm )
        return;
    
    // if the surface is lost, DDCopyBitmap will fail and the surface will
    // be restored in FlipScreen.
    ddrval = DDCopyBitmap( lpBackBuffer, hbm, 0, 0, 0, 0 );
    
    DeleteObject( hbm );
    
    FlipScreen();
}

#ifdef USE_DSOUND
//
// play a sound, but first set the panning according to where the
// object is on the screen.  fake 3D sound.
//
void playPanned(HSNDOBJ hSO, DBLNODE *object)
{
    IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer(hSO);
    
    if(!bWantSound)
        return;   // No sound our Work is done
    
    if (pDSB)
    {
        switch(ScreenX)
        {
        case 320:
            IDirectSoundBuffer_SetPan(pDSB, (LONG)((20000.0 *
                ((object->dst.right + object->dst.left) / 2) / 320.0) - 10000.0));
            break;
        case 640:
            IDirectSoundBuffer_SetPan(pDSB, (LONG)((20000.0 *
                ((object->dst.right + object->dst.left) / 2) / 640.0) - 10000.0));
            break;
        case 1024:
            IDirectSoundBuffer_SetPan(pDSB, (LONG)((20000.0 *
                ((object->dst.right + object->dst.left) / 2) / 1024.0) - 10000.0));
            break;
        case 1280:
            IDirectSoundBuffer_SetPan(pDSB, (LONG)((20000.0 *
                ((object->dst.right + object->dst.left) / 2) / 1280.0) - 10000.0));
            break;
        }
        
        IDirectSoundBuffer_Play(pDSB, 0, 0, 0);
    }
}
#endif

void UpdateFrame( void )
{
    switch( ProgramState )
    {
    case PS_SPLASH:
        // display the splash screen
        bltSplash();
        return;
    case PS_ACTIVE:
        UpdateDisplayList();
        CheckForHits();
        DrawDisplayList();
        if ( isDisplayListEmpty() )
        {
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjStop(hsoEngineIdle);
                SndObjStop(hsoEngineRev);
            }
#endif
            bPlayIdle = FALSE;
            bPlayRev = FALSE;
            lastThrust = lastShield = FALSE;
            ProgramState = PS_BEGINREST;
            restCount = GetTickCount();
            initLevel( ++level );
            ///////////// DMUSIC BEGIN //////////////
            ComposeNewSegments();
            if (bMusicEnabled)
            {
                // Do an Autotransition with ending to the new first segment
                if (gapSegment[SEGMENT_TRANS_2]) 
                {
                    gapSegment[SEGMENT_TRANS_2]->Release();
                }
                gpComposer->AutoTransition(
                    gpPerformance,
                    gapSegment[SEGMENT_1], 
                    DMUS_COMMANDT_END, 
                    DMUS_COMPOSEF_BEAT | DMUS_COMPOSEF_LONG,
                    gapChordMap[gnLastStyle] [gnLastChordMap], 
                    &gapSegment[SEGMENT_TRANS_2],
                    NULL, NULL
                    );
            }
            ///////////// DMUSIC END //////////////
        }
        return;
    case PS_BEGINREST:
#ifdef USE_DSOUND
        if(bWantSound)
        {
            SndObjPlay(hsoBeginLevel, 0);
        }
#endif
        ProgramState = PS_REST;
        //
        // FALLTHRU
        //
    case PS_REST:
        /////////////// DMUSIC BEGIN //////////////////////
        if( !bMusicEnabled && ( GetTickCount() - restCount ) > ShowLevelCount ||
            bMusicEnabled && TransitionDone() )
        {
            /////////////// DMUSIC END //////////////////////
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjPlay(hsoEngineIdle, DSBPLAY_LOOPING);
            }
#endif
            bPlayIdle = TRUE;
            lastTickCount = GetTickCount();
            ProgramState = PS_ACTIVE;
        }
        else
        {
            DisplayLevel();
        }
        return;
    }
}

void DisplayLevel( void )
{
    char buf[10];
    
    EraseScreen();
    buf[0] = 10 + '0';
    buf[1] = 11 + '0';
    buf[2] = 12 + '0';
    buf[3] = 11 + '0';
    buf[4] = 10 + '0';
    buf[5] = '\0';
    bltScore( buf, ScreenX/2-64, ScreenY/2-8 );
    buf[0] = level / 100 + '0';
    buf[1] = level / 10 + '0';
    buf[2] = level % 10 + '0';
    buf[3] = '\0';
    bltScore( buf, ScreenX/2+22, ScreenY/2-8 );
    FlipScreen();
}

void bltScore( char *num, int x, int y )
{
    char *c;
    RECT    src;
    int     i;
    HRESULT ddrval;
    
    for(c=num; *c != '\0'; c++)
    {
        while( 1 )
        {
            i = *c - '0';
            src.left = i*16;
            src.top = 0;
            src.right = src.left + 16;
            src.bottom = src.top + 16;
            ddrval = lpBackBuffer->BltFast( x, y, lpNum, &src, dwTransType );
            if( ddrval == DD_OK )
            {
                break;
            }
            if( ddrval == DDERR_SURFACELOST )
            {
                if( !RestoreSurfaces() )
                    return;
            }
            if( ddrval != DDERR_WASSTILLDRAWING )
            {
                return;
            }
        }
        x += 16;
    }
}

void CheckForHits( void )
{
    LPDBLNODE   bullet, target, save;
    int         frame, x, y, l, t;
    BOOL        hit;
    
    // update screen rects
    target = &DL;
    do
    {
        frame = (DWORD)target->frame;
        switch( target->type )
        {
        case OBJ_DONUT:
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 64;
            target->dst.bottom = target->dst.top + 64;
            target->src.left = 64 * (frame % 5);
            target->src.top = 64 * (frame /5);
            target->src.right = target->src.left + 64;
            target->src.bottom = target->src.top + 64;
            break;
        case OBJ_PYRAMID:
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 32;
            target->dst.bottom = target->dst.top + 32;
            target->src.left = 32 * (frame % 10);
            target->src.top = 32 * (frame /10);
            target->src.right = target->src.left + 32;
            target->src.bottom = target->src.top + 32;
            break;
        case OBJ_SPHERE:
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 16;
            target->dst.bottom = target->dst.top + 16;
            target->src.left = 16 * (frame % 20);
            target->src.top = 16 * (frame /20);
            target->src.right = target->src.left + 16;
            target->src.bottom = target->src.top + 16;
            break;
        case OBJ_CUBE:
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 16;
            target->dst.bottom = target->dst.top + 16;
            target->src.left = 16 * (frame % 20);
            target->src.top = 16 * (frame /20);
            target->src.right = target->src.left + 16;
            target->src.bottom = target->src.top + 16;
            break;
        case OBJ_SHIP:
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 32;
            target->dst.bottom = target->dst.top + 32;
            if( lastShield )
                target->src.top = 32 * (frame / 10) + 128;
            else
                target->src.top = 32 * (frame /10);
            target->src.left = 32 * (frame % 10);
            target->src.right = target->src.left + 32;
            target->src.bottom = target->src.top + 32;
            break;
        case OBJ_BULLET:
            frame = (DWORD)target->frame/20 % 4;
            target->dst.left = (DWORD)target->posx;
            target->dst.top = (DWORD)target->posy;
            target->dst.right = target->dst.left + 3;
            target->dst.bottom = target->dst.top + 3;
            target->src.left = BULLET_X + frame*4;
            target->src.top = BULLET_Y;
            target->src.right = target->src.left + 3;
            target->src.bottom = target->src.top + 3;
            break;
        }
        target = target->next;
    }
    while( target != &DL );
    
    bullet=&DL;
    do
    {
        hit = FALSE;
        if((bullet->type != OBJ_BULLET) && (bullet != &DL))
        {
            bullet = bullet->next;
            continue;
        }
        
        x = (bullet->dst.left + bullet->dst.right) / 2;
        y = (bullet->dst.top + bullet->dst.bottom) / 2;
        for(target=DL.next; target != &DL; target = target->next)
        {
            if( ( target->type != OBJ_DONUT ) &&
                ( target->type != OBJ_PYRAMID ) &&
                ( target->type != OBJ_SPHERE ) &&
                ( target->type != OBJ_CUBE ) )
                continue;
            
            if( (x >= target->dst.left) &&
                (x <  target->dst.right) &&
                (y >= target->dst.top) &&
                (y <  target->dst.bottom) )
            {
                if ((bullet != &DL) || !lastShield)
                {
                    // the bullet hit the target
                    switch( target->type )
                    {
                    case OBJ_DONUT:
#ifdef USE_DSOUND
                        if(bWantSound)
                        {
                            playPanned(hsoDonutExplode, target);
                        }
#endif
                        //////////////// DMUSIC BEGIN /////////
                        if (bMusicEnabled)
                        {
                            gpPerformance->PlaySegment(
                                gapMotif[gnCurrentStyle][MOTIF_BLOWUP], 
                                DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
                        }
                        
                        //////////////// DMUSIC END /////////
                        addObject( OBJ_PYRAMID, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        addObject( OBJ_PYRAMID, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        addObject( OBJ_PYRAMID, target->dst.left,
                            target->dst.top, -1.0, -1.0 );

                        score += 10;
                        break;
                    case OBJ_PYRAMID:
#ifdef USE_DSOUND
                        if(bWantSound)
                        {
                            playPanned(hsoPyramidExplode, target);
                        }
#endif
                        //////////////// DMUSIC BEGIN /////////
                        if (bMusicEnabled)
                        {
                            if (snMaxBlowUps > 0)
                            {
                                snMaxBlowUps--;
                                gpPerformance->PlaySegment(
                                    gapMotif[gnCurrentStyle][MOTIF_BLOWUPLITE], 
                                    DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
                            }
                        }
                        //////////////// DMUSIC END /////////
                        addObject( OBJ_SPHERE, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        addObject( OBJ_CUBE, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        addObject( OBJ_CUBE, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        score += 20;
                        break;
                    case OBJ_CUBE:
#ifdef USE_DSOUND
                        if(bWantSound)
                        {
                            playPanned(hsoCubeExplode, target);
                        }
#endif
                        //////////////// DMUSIC BEGIN /////////
                        if (bMusicEnabled)
                        {
                            if (snMaxBlowUps > 0)
                            {
                                snMaxBlowUps--;
                                gpPerformance->PlaySegment(
                                    gapMotif[gnCurrentStyle][MOTIF_BLOWUPLITE], 
                                    DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
                            }
                        }
                        //////////////// DMUSIC END /////////
                        addObject( OBJ_SPHERE, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        addObject( OBJ_SPHERE, target->dst.left,
                            target->dst.top, -1.0, -1.0 );
                        break;
                        score += 40;
                    case OBJ_SPHERE:
#ifdef USE_DSOUND
                        if(bWantSound)
                        {
                            playPanned(hsoSphereExplode, target);
                        }
#endif
                        //////////////// DMUSIC BEGIN /////////
                        if (bMusicEnabled)
                        {
                            if (snMaxBlowUps > 0)
                            {
                                snMaxBlowUps--;
                                gpPerformance->PlaySegment(
                                    gapMotif[gnCurrentStyle][MOTIF_BLOWUPLITE], 
                                    DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
                            }
                        }
                        //////////////// DMUSIC END /////////
                        score += 20;
                    }
                    
                    l = target->dst.left;
                    t = target->dst.top;
                    DeleteFromList( target );
                }
                
                hit = TRUE;
            }
            
            if( hit )
            {
                //////////// DMUSIC BEGIN ///////////////
                if (!bAnyHits)
                {
                    if (bMusicEnabled && !gbShieldsOn)
                    {
                        // Do an AutoTransition to the 2nd segment
                        if (gapSegment[SEGMENT_TRANS_1])
                        {
                            gapSegment[SEGMENT_TRANS_1]->Release();
                        }
                        gpComposer->AutoTransition(
                            gpPerformance,
                            gapSegment[SEGMENT_2], DMUS_COMMANDT_FILL, 
                            DMUS_COMPOSEF_MODULATE | DMUS_COMPOSEF_MEASURE,
                            gapChordMap[gnCurrentStyle] [gnCurrentChordMap], 
                            &gapSegment[SEGMENT_TRANS_1],
                            NULL, NULL
                            );
                        bAnyHits = TRUE;
                    }
                    snSubLevel = 2;
                }
                //////////// DMUSIC END ///////////////
                if( bullet == &DL )
                {
                    hit = FALSE;
                    if (!lastShield && !showDelay && !bTest)
                    {
#ifdef USE_DSOUND
                        if(bWantSound)
                        {
                            playPanned(hsoShipExplode, bullet);
                        }
#endif
                        //////////////// DMUSIC BEGIN /////////
                        if (bMusicEnabled)
                        {
                            gpPerformance->PlaySegment(
                                gapMotif[gnCurrentStyle][MOTIF_DEATH], 
                                DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
                        }
                        //////////////// DMUSIC END /////////
                        score -= 150;
                        if (score < 0)
                            score = 0;
                        
                        addObject( OBJ_SPHERE, l, t, -1.0, -1.0 );
                        addObject( OBJ_SPHERE, l, t, -1.0, -1.0 );
                        addObject( OBJ_SPHERE, l, t, -1.0, -1.0 );
                        addObject( OBJ_SPHERE, l, t, -1.0, -1.0 );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        addObject( OBJ_BULLET, l, t,
                            randDouble( -0.5, 0.5 ), randDouble( -0.5, 0.5 ) );
                        initShip(TRUE);
                    }
                }
                
                break;
            }
        }
        
        if( hit )
        {
            save = bullet;
            bullet = bullet->next;
            
            DeleteFromList( save );
        }
        else
        {
            bullet = bullet->next;
        }
        
    } while (bullet != &DL);
}

void EraseScreen( void )
{
    DDBLTFX     ddbltfx;
    HRESULT     ddrval;
    
    // iForceErase forces us to erase so we can get of stray GDI pixels
    if (iForceErase)
    {
        iForceErase--;
    }
    else if( bSpecialEffects )   // cool looking screen with no colorfill
    {
        return;
    }
    
    // Erase the background
    ZeroMemory(&ddbltfx, sizeof( ddbltfx ));
    ddbltfx.dwSize = sizeof( ddbltfx );
    ddbltfx.dwFillColor = dwFillColor;
    while( 1 )
    {
        ddrval = lpBackBuffer->Blt( NULL, NULL,
            NULL, DDBLT_COLORFILL, &ddbltfx );
        
        if( ddrval == DD_OK )
        {
            break;
        }
        if( ddrval == DDERR_SURFACELOST )
        {
            if( !RestoreSurfaces() )
                return;
        }
        if( ddrval != DDERR_WASSTILLDRAWING )
        {
            return;
        }
    }
}

void FlipScreen( void )
{
    HRESULT     ddrval;
    
    // Flip the surfaces
    while( 1 )
    {
        ddrval = lpFrontBuffer->Flip( NULL, 0 );
        if( ddrval == DD_OK )
        {
            break;
        }
        if( ddrval == DDERR_SURFACELOST )
        {
            if( !RestoreSurfaces() )
            {
                return;
            }
        }
        if( ddrval != DDERR_WASSTILLDRAWING )
        {
            break;
        }
    }
}

void DrawDisplayList( void )
{
    LPDBLNODE   thiss;
    LPDBLNODE   last;
    HRESULT     ddrval;
    char        scorebuf[11];
    int         rem;
    
    // blt everything in reverse order if we are doing destination transparency
    // calculate score string
    scorebuf[0] = score/10000000 + '0';
    rem = score % 10000000;
    scorebuf[1] = rem/1000000 + '0';
    rem = score % 1000000;
    scorebuf[2] = rem/100000 + '0';
    rem = score % 100000;
    scorebuf[3] = rem/10000 + '0';
    rem = score % 10000;
    scorebuf[4] = rem/1000 + '0';
    rem = score % 1000;
    scorebuf[5] = rem/100 + '0';
    rem = score % 100;
    scorebuf[6] = rem/10 + '0';
    rem = score % 10;
    scorebuf[7] = rem + '0';
#ifdef USE_DSOUND
    if( bSoundEnabled )
    {
        scorebuf[8] = 14 + '0';
        scorebuf[9] = 13 + '0';
        scorebuf[10] = '\0';
    }
    else
#endif
    {
        scorebuf[8] = '\0';
    }
    
    EraseScreen();
    if( dwTransType == DDBLTFAST_DESTCOLORKEY )
    {
        bltScore(scorebuf, 10, ScreenY-26);
        
        if( bShowFrameCount )
            DisplayFrameRate();
        
        thiss = DL.next; // start with the topmost bitmap
        last = DL.next; // don't blt it twice
        
        if (showDelay)
            last = &DL;
    }
    else
    {
        thiss = &DL;    // start with the bottommost bitmap (the ship)
        last = &DL; // don't blt it twice
        
        if (showDelay)
            thiss = thiss->prev;
    }
    
    do
    {
        while( 1 )
        {
            if( thiss->src.right - thiss->src.left == 0 )
                break; // new objects don't have a src rect yet

            ddrval = lpBackBuffer->BltFast( thiss->dst.left, thiss->dst.top, thiss->surf, &(thiss->src), dwTransType );
            if( ddrval == DD_OK )
            {
                break;
            }
            if( ddrval == DDERR_SURFACELOST )
            {
                if( !RestoreSurfaces() )
                    return;
            }
            if( ddrval != DDERR_WASSTILLDRAWING )
            {
                return;
            }
        }
        if( dwTransType != DDBLTFAST_DESTCOLORKEY )
        {
            thiss = thiss->prev;
        }
        else
        {
            thiss = thiss->next;
        }
    }
    while( thiss != last );
    
    if( dwTransType != DDBLTFAST_DESTCOLORKEY )
    {
        bltScore(scorebuf, 10, ScreenY-26);
        
        if( bShowFrameCount )
            DisplayFrameRate();
    }
    
    FlipScreen();
}

void DisplayFrameRate( void )
{
    DWORD               time2;
    char                buff[256];
    
    dwFrameCount++;
    time2 = timeGetTime() - dwFrameTime;
    if( time2 > 1000 )
    {
        dwFrames = (dwFrameCount*1000)/time2;
        dwFrameTime = timeGetTime();
        dwFrameCount = 0;
    }
    if( dwFrames == 0 )
    {
        return;
    }
    
    if (dwFrames != dwFramesLast)
    {
        dwFramesLast = dwFrames;
    }
    
    if( dwFrames > 99 )
    {
        dwFrames = 99;
    }
    buff[0] = (char)((dwFrames / 10) + '0');
    buff[1] = (char)((dwFrames % 10) + '0');
    buff[2] = '\0';
    bltScore(buff, ScreenX/2-25, 10);
}

void DeleteFromList( LPDBLNODE thiss )
{
    thiss->next->prev = thiss->prev;
    thiss->prev->next = thiss->next;
    LocalFree( thiss );
}

void UpdateDisplayList( void )
{
    LPDBLNODE   thiss;
    LPDBLNODE   save;
    DWORD       thisTickCount = GetTickCount();
    DWORD       tickDiff = thisTickCount - lastTickCount;
    double      maxx, maxy;
    double      maxframe;
    DWORD       input = 0L;
    BOOL        event = FALSE;
    
    if( bTest )
    {
        input |= (KEY_RIGHT | KEY_FIRE);
    }
    lastTickCount = thisTickCount;
    
    input = ReadGameInput();
    
    if (showDelay)
    {
        showDelay -= (int)tickDiff;
        if (showDelay < 0)
        {
            showDelay = 0;
            lastShield = FALSE;
            initShip( FALSE );
        }
    }
    
    // update the ship
    if( !showDelay )
    {
        DL.posx += DL.velx * (double)tickDiff;
        DL.posy += DL.vely * (double)tickDiff;
    }
    if( DL.posx > MAX_SHIP_X )
    {
        DL.posx = MAX_SHIP_X;
        DL.velx = -DL.velx;
        event = TRUE;
    }
    else if ( DL.posx < 0 )
    {
        DL.posx =0;
        DL.velx = -DL.velx;
        event = TRUE;
    }
    if( DL.posy > MAX_SHIP_Y )
    {
        DL.posy = MAX_SHIP_Y;
        DL.vely = -DL.vely;
        event = TRUE;
    }
    else if ( DL.posy < 0 )
    {
        DL.posy =0;
        DL.vely = -DL.vely;
        event = TRUE;
    }
    if (event)
    {
#ifdef USE_DSOUND
        if(bWantSound)
        {
            playPanned(hsoShipBounce, &DL);
        }
#endif
        //////////////// DMUSIC BEGIN /////////
        if (bMusicEnabled)
        {
            gpPerformance->PlaySegment(
                gapMotif[gnCurrentStyle][MOTIF_BOUNCE], 
                DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
        }
        //////////////// DMUSIC END /////////
        event = FALSE;
    }
    
    if ((event = (showDelay || ((input & KEY_SHIELD) == KEY_SHIELD))) !=
        lastShield)
    {
        if (event && !showDelay)
        {
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjPlay(hsoShieldBuzz, DSBPLAY_LOOPING);
            }
#endif
            bPlayBuzz = TRUE;
            //////////////// DMUSIC BEGIN /////////
            gbShieldsOn = TRUE;
            if (bMusicEnabled)
            {
                // Set the band to the shields band
                gpPerformance->PlaySegment(
                    gapShieldSegment[gnCurrentStyle], 
                    DMUS_SEGF_SECONDARY, 0, NULL);
                gpPerformance->PlaySegment(
                    gapMotif[gnCurrentStyle][MOTIF_SHIELD], 
                    DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID, 0, NULL);
            }
            //////////////// DMUSIC END /////////
        }
        else
        {
            //////////////// DMUSIC BEGIN /////////
            // Set the band to the default band
            gbShieldsOn = FALSE;
            if (bMusicEnabled)
            {
                // Set the band to the shields band
                gpPerformance->PlaySegment(
                    gapDefaultSegment[gnCurrentStyle], 
                    DMUS_SEGF_SECONDARY, 0, NULL);
                
            }
            //////////////// DMUSIC END /////////
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjStop(hsoShieldBuzz);
            }
#endif
            bPlayBuzz = FALSE;
        }
        lastShield = event;
    }
    if (event)
    {
        input &= ~(KEY_FIRE);
    }
    
    if (input & KEY_FIRE)
    {
        if( !showDelay )
        {
            // add a bullet to the scene
            score--;
            if(score < 0)
                score = 0;
            
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjPlay(hsoFireBullet, 0);
            }
#endif
            addObject( OBJ_BULLET, Dirx[(int)DL.frame]*6.0 + 16.0 + DL.posx,
                Diry[(int)DL.frame]*6.0 + 16.0 + DL.posy,
                Dirx[(int)DL.frame]*500.0/1000.0,
                Diry[(int)DL.frame]*500.0/1000.0 );
        }
    }
    
    event = FALSE;
    if( input & KEY_LEFT )
    {
        DL.frame -= 1.0;
        if( DL.frame < 0.0 )
            DL.frame += MAX_SHIP_FRAME;
    }
    if( input & KEY_RIGHT )
    {
        DL.frame += 1.0;
        if( DL.frame >= MAX_SHIP_FRAME)
            DL.frame -= MAX_SHIP_FRAME;
    }
    if( input & KEY_UP )
    {
        DL.velx += Dirx[(int)DL.frame] * 10.0/1000.0;
        DL.vely += Diry[(int)DL.frame] * 10.0/1000.0;
        event = TRUE;
    }
    if( input & KEY_DOWN )
    {
        DL.velx -= Dirx[(int)DL.frame] * 10.0/1000.0;
        DL.vely -= Diry[(int)DL.frame] * 10.0/1000.0;
        event = TRUE;
    }
    
    if (event != lastThrust)
    {
        if (event)
        {
            input &= ~KEY_STOP;
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjStop(hsoSkidToStop);
                SndObjPlay(hsoEngineRev, DSBPLAY_LOOPING);
            }
#endif
            bPlayRev = TRUE;
        }
        else
        {
#ifdef USE_DSOUND
            if(bWantSound)
            {
                SndObjStop(hsoEngineRev);
            }
#endif
            bPlayRev = FALSE;
        }
        
        lastThrust = event;
    }
    
    if( input & KEY_STOP )
    {
#ifdef USE_DSOUND
        if(bWantSound)
        {
            if (DL.velx || DL.vely)
                playPanned(hsoSkidToStop, &DL);
        }
#endif
        
        DL.velx = 0;
        DL.vely = 0;
    }
    
    thiss = DL.next;
    do
    {
        thiss->posx += thiss->velx * (double)tickDiff;
        thiss->posy += thiss->vely * (double)tickDiff;
        thiss->frame += thiss->delay * (double)tickDiff;
        switch( thiss->type )
        {
        case OBJ_DONUT:
            maxx = (double)MAX_DONUT_X;
            maxy = (double)MAX_DONUT_Y;
            maxframe = (double)MAX_DONUT_FRAME;
            break;
        case OBJ_PYRAMID:
            maxx = (double)MAX_PYRAMID_X;
            maxy = (double)MAX_PYRAMID_Y;
            maxframe = (double)MAX_PYRAMID_FRAME;
            break;
        case OBJ_SPHERE:
            maxx = (double)MAX_SPHERE_X;
            maxy = (double)MAX_SPHERE_Y;
            maxframe = (double)MAX_SPHERE_FRAME;
            break;
        case OBJ_CUBE:
            maxx = (double)MAX_CUBE_X;
            maxy = (double)MAX_CUBE_Y;
            maxframe = (double)MAX_CUBE_FRAME;
            break;
        case OBJ_BULLET:
            maxx = (double)MAX_BULLET_X;
            maxy = (double)MAX_BULLET_Y;
            maxframe = (double)MAX_BULLET_FRAME;
            if( thiss->frame >= (double)MAX_BULLET_FRAME )
            {
                save = thiss;
                thiss = thiss->next;
                DeleteFromList( save );
                continue;
            }
            break;
        }
        if( thiss != &DL )
        {
            if( thiss->posx > maxx )
            {
                thiss->posx = maxx;
                thiss->velx = -thiss->velx;
            }
            else if ( thiss->posx < 0 )
            {
                thiss->posx =0;
                thiss->velx = -thiss->velx;
            }
            if( thiss->posy > maxy )
            {
                thiss->posy = maxy;
                thiss->vely = -thiss->vely;
            }
            else if ( thiss->posy < 0 )
            {
                thiss->posy =0;
                thiss->vely = -thiss->vely;
            }
            if( thiss->frame >= maxframe )
            {
                thiss->frame -= maxframe;
            }
            thiss = thiss->next;
        }
    }
    while( thiss != &DL );
}

BOOL isDisplayListEmpty( void )
{
    LPDBLNODE ptr;
    
    for(ptr=DL.next; ptr != &DL; ptr = ptr->next)
    {
        if(ptr->type != OBJ_BULLET)
            return FALSE;
    }
    return TRUE;
}

void initShip( BOOL delay )
{
    DL.posx = (double)(ScreenX/2-16);       // center the ship
    DL.posy = (double)(ScreenY/2-16);
    DL.frame = 0.0;
    if( bTest )
    {
        DL.velx = 0.25;
        DL.vely = 0.5;
    }
    else
    {
        DL.velx = DL.vely = 0.0;    // not moving
    }
    if( !bTest && delay )
        showDelay = DEF_SHOW_DELAY;
}

void initLevel( int level )
{
    int     i;
    
    // clear any stray bullets out of the display list
    while( DL.next != &DL )
    {
        DeleteFromList( DL.next );
    }
    //for(i=0; i<(2*level-1); i++)
    for(i=0; i < ( ( level - 1) % 8 ) + 1; i++)
    {
        addObject( OBJ_DONUT, -1.0, -1.0, -1.0, -1.0 );
    }
    initShip(TRUE);
}

void addObject( SHORT type, double x, double y, double vx, double vy )
{
    LPDBLNODE   newt;
    
    newt = (LPDBLNODE) LocalAlloc( LPTR, sizeof( DBLNODE ) );
    if( newt == NULL)
    {
        OutputDebugString( "Out of memory!\n" );
        return;
    }

    newt->type = type;
    switch( type )
    {
    case OBJ_DONUT:
        if( x < 0.0) // no position specified?
        {
            newt->posx = randDouble( 0.0, (double)MAX_DONUT_X );
            newt->posy = randDouble( 0.0, (double)MAX_DONUT_Y );
        }
        else
        {
            newt->posx = x;
            newt->posy = y;
        }
        newt->velx = randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->vely = randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->frame = randDouble( 0, 30 );
        newt->delay = 30.0*randDouble( 0.1, 0.4 )/1000.0;
        newt->surf = lpDonut;
        linkObject( newt );
        break;
    case OBJ_PYRAMID:
        if( x < 0) // no position specified?
        {
            newt->posx = randDouble( 0.0, (double)MAX_PYRAMID_X );
            newt->posy = randDouble( 0.0, (double)MAX_PYRAMID_Y );
        }
        else
        {
            newt->posx = x;
            newt->posy = y;
        }
        newt->velx = 1.5*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->vely = 1.5*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->frame = randDouble( 0, 30 );
        newt->delay = 40.0*randDouble( 0.3, 1.0 )/1000.0;
        newt->surf = lpPyramid;
        linkObject( newt ); 
        break;
    case OBJ_SPHERE:
        if( x < 0) // no position specified?
        {
            newt->posx = randDouble( 0.0, (double)MAX_SPHERE_X );
            newt->posy = randDouble( 0.0, (double)MAX_SPHERE_Y );
        }
        else
        {
            newt->posx = x;
            newt->posy = y;
        }
        newt->velx = 3.0*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->vely = 3.0*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->frame = randDouble( 0, 30 );
        newt->delay = 40.0*randDouble( 1.5, 2.0 )/1000.0;
        newt->surf = lpSphere;
        linkObject( newt );
        break;
    case OBJ_CUBE:
        if( x < 0) // no position specified?
        {
            newt->posx = randDouble( 0.0, (double)MAX_CUBE_X );
            newt->posy = randDouble( 0.0, (double)MAX_CUBE_Y );
        }
        else
        {
            newt->posx = x;
            newt->posy = y;
        }
        newt->velx = 4.0*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->vely = 4.0*randDouble( -50.0/1000.0, 50.0/1000.0 );
        newt->frame = randDouble( 0, 30 );
        newt->delay = 40.0*randDouble( 0.8, 2.0 )/1000.0;
        newt->surf = lpCube;
        linkObject( newt );
        break;
    case OBJ_BULLET:
        newt->posx = x;
        newt->posy = y;
        newt->velx = vx;
        newt->vely = vy;
        newt->frame = 0.0;
        newt->delay = 1.0;
        newt->surf = lpNum;
        linkObject( newt );
        break;
    }
}

void linkObject( LPDBLNODE newt )
{
    newt->next = DL.next;
    newt->prev = &DL;
    DL.next->prev = newt;
    DL.next = newt;
}

void linkLastObject( LPDBLNODE newt )
{
    newt->prev = DL.prev;
    newt->next = &DL;
    DL.prev->next = newt;
    DL.prev = newt;
}


BOOL RestoreSurfaces( void )
{
    HRESULT ddrval;
    HBITMAP     hbm;
    
    ddrval = lpFrontBuffer->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpDonut->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpPyramid->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpCube->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpSphere->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpShip->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    ddrval = lpNum->Restore();
    if( ddrval != DD_OK )
        return FALSE;
    
    // Create and set the palette for the splash bitmap
    lpSplashPalette = DDLoadPalette( lpDD, "SPLASH" );
    if( NULL == lpSplashPalette )
        return CleanupAndExit("DDLoadPalette SPLASH");
    
    // Create and set the palette for the art bitmap
    lpArtPalette = DDLoadPalette( lpDD, "DONUTS8" );
    if( NULL == lpArtPalette )
        return CleanupAndExit("DDLoadPalette DONUTS");
    
    // set the palette before loading the art
    lpFrontBuffer->SetPalette( lpArtPalette );
    
    hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
    
    if( NULL == hbm )
    {
        OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
        return FALSE;
    }
    
    ddrval = DDCopyBitmap( lpDonut, hbm, 0, 0, 320, 384 );
    if( ddrval != DD_OK )
    {
        OutputDebugString( "Could not copy bitmap!\n" );
        DeleteObject( hbm );
        return FALSE;
    }
    
    // NOTE: Why are we calling LoadImage again?  StretchBlt (which is
    // called in DDCopyBitmap) does not work properly when performing
    // an 8-bpp to 24- or 32-bpp blt multiple times from the same
    // bitmap.  The workaround is to call LoadImage before each
    // StretchBlt because the first StretchBlt after a LoadImage will
    // work.
    if(ScreenBpp >= 24)
    {
        DeleteObject( hbm );
        hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
        
        if( NULL == hbm )
        {
            OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
            return FALSE;
        }
    }
    
    ddrval = DDCopyBitmap( lpPyramid, hbm, 0, 384, 320, 128 );
    if( ddrval != DD_OK )
    {
        DeleteObject( hbm );
        return FALSE;
    }
    
    if(ScreenBpp >= 24)
    {
        DeleteObject( hbm );
        hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
        
        if( NULL == hbm )
        {
            OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
            return FALSE;
        }
    }
    
    ddrval = DDCopyBitmap( lpSphere, hbm, 0, 512, 320, 32 );
    if( ddrval != DD_OK )
    {
        DeleteObject( hbm );
        return FALSE;
    }
    
    if(ScreenBpp >= 24)
    {
        DeleteObject( hbm );
        hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
        
        if( NULL == hbm )
        {
            OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
            return FALSE;
        }
    }
    
    ddrval = DDCopyBitmap( lpCube, hbm, 0, 544, 320, 32 );
    if( ddrval != DD_OK )
    {
        DeleteObject( hbm );
        return FALSE;
    }
    
    if(ScreenBpp >= 24)
    {
        DeleteObject( hbm );
        hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
        
        if( NULL == hbm )
        {
            OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
            return FALSE;
        }
    }
    
    ddrval = DDCopyBitmap( lpShip, hbm, 0, 576, 320, 256 );
    if( ddrval != DD_OK )
    {
        DeleteObject( hbm );
        return FALSE;
    }
    
    if(ScreenBpp >= 24)
    {
        DeleteObject( hbm );
        hbm = (HBITMAP)LoadImage(GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
        
        if( NULL == hbm )
        {
            OutputDebugString( "Could not load DONUTS8 bitmap!\n" );
            return FALSE;
        }
    }
    
    ddrval = DDCopyBitmap( lpNum, hbm, 0, 832, 320, 16 );
    if( ddrval != DD_OK )
    {
        DeleteObject( hbm );
        return FALSE;
    }
    
    DeleteObject( hbm );
    
    // set colorfill colors and color keys according to bitmap contents
    dwFillColor = DDColorMatch( lpDonut, CLR_INVALID );
    
    DDSetColorKey( lpDonut, CLR_INVALID );
    DDSetColorKey( lpPyramid, CLR_INVALID );
    DDSetColorKey( lpCube, CLR_INVALID );
    DDSetColorKey( lpSphere, CLR_INVALID );
    DDSetColorKey( lpShip, CLR_INVALID );
    DDSetColorKey( lpNum, CLR_INVALID );
    
    return TRUE;
}


int randInt( int low, int high )
{
    int range = high - low;
    int num = rand() % range;
    return( num + low );
}

double randDouble( double low, double high )
{
    double range = high - low;
    double num = range * (double)rand()/(double)RAND_MAX;
    return( num + low );
}

#ifdef USE_DSOUND
void InitializeSound( void )
{
    if(!bWantSound)
        return; // out of here
    bSoundEnabled = FALSE;
    if (SUCCEEDED(DirectSoundCreate(NULL, &lpDS, NULL)))
    {
        if (SUCCEEDED(lpDS->SetCooperativeLevel( hWndMain,
            DSSCL_PRIORITY)))
        {
            hsoBeginLevel     = SndObjCreate(lpDS, "BeginLevel",      1);
            hsoEngineIdle     = SndObjCreate(lpDS, "EngineIdle",      1);
            hsoEngineRev      = SndObjCreate(lpDS, "EngineRev",       1);
            hsoSkidToStop     = SndObjCreate(lpDS, "SkidToStop",      1);
            hsoShieldBuzz     = SndObjCreate(lpDS, "ShieldBuzz",      1);
            hsoShipExplode    = SndObjCreate(lpDS, "ShipExplode",     1);
            hsoFireBullet     = SndObjCreate(lpDS, "Gunfire",        25);
            hsoShipBounce     = SndObjCreate(lpDS, "ShipBounce",      4);
            hsoDonutExplode   = SndObjCreate(lpDS, "DonutExplode",   10);
            hsoPyramidExplode = SndObjCreate(lpDS, "PyramidExplode", 12);
            hsoCubeExplode    = SndObjCreate(lpDS, "CubeExplode",    15);
            hsoSphereExplode  = SndObjCreate(lpDS, "SphereExplode",  10);
            bSoundEnabled = TRUE;
            
            //#ifdef USE_DSOUND   this should be dead code Josephc
            if( bPlayIdle )
                SndObjPlay(hsoEngineIdle, DSBPLAY_LOOPING);
            
            if( bPlayBuzz )
                SndObjPlay(hsoShieldBuzz, DSBPLAY_LOOPING);
            
            if( bPlayRev )
                SndObjPlay(hsoEngineRev, DSBPLAY_LOOPING);
            //#endif
        }
        else
        {
            OutputDebugString( "Could not set DSound's coop level!\n" );
            lpDS->Release(); 
            lpDS = NULL;
        }
    }
    else
        OutputDebugString( "Could not create DSound!\n" );
}

void DestroySound( void )
{
    if(!bWantSound)
        return; //No work to be done
    bSoundEnabled = FALSE;
    if (lpDS)
    {
        SndObjDestroy(hsoBeginLevel);
        hsoBeginLevel = NULL;
        SndObjDestroy(hsoEngineIdle);
        hsoEngineIdle = NULL;
        SndObjDestroy(hsoEngineRev);
        hsoEngineRev = NULL;
        SndObjDestroy(hsoSkidToStop);
        hsoSkidToStop = NULL;
        SndObjDestroy(hsoShieldBuzz);
        hsoShieldBuzz = NULL;
        SndObjDestroy(hsoShipExplode);
        hsoShipExplode = NULL;
        SndObjDestroy(hsoFireBullet);
        hsoFireBullet = NULL;
        SndObjDestroy(hsoShipBounce);
        hsoShipBounce = NULL;
        SndObjDestroy(hsoDonutExplode);
        hsoDonutExplode = NULL;
        SndObjDestroy(hsoPyramidExplode);
        hsoPyramidExplode = NULL;
        SndObjDestroy(hsoCubeExplode);
        hsoCubeExplode = NULL;
        SndObjDestroy(hsoSphereExplode);
        hsoSphereExplode = NULL;
        
    }
}
#endif

int getint(char**p, int def)
{
    int i=0;
    
    
    while (IS_SPACE(**p))
        (*p)++;
    
    if (!IS_NUM(**p))
        return def;
    
    while (IS_NUM(**p))
        i = i*10 + *(*p)++ - '0';
    
    while (IS_SPACE(**p))
        (*p)++;
    
    return i;
}


static char szDirectMusicMedia[] = "\\DMusic\\Media";
BOOL GetSearchPath(WCHAR wszPath[MAX_PATH])
{
    HKEY    hkDirectX;
    BOOL    bRet = FALSE;
    char    szPath[MAX_PATH];
    DWORD   cbPath;
    
    
    // Get DirectX SDK search path from the registry
    //
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\DirectX",
        0,                          // Reserved
        KEY_READ,
        &hkDirectX))
    {
        OutputDebugString( "Could not find registry path for DXSDK!\n" );
        return FALSE;
    }
    
    cbPath = sizeof(szPath);
    if (RegQueryValueEx(hkDirectX,
        "DXSDK Samples Path",
        NULL,                   // Reserved
        NULL,                   // Type: don't care
        (LPBYTE)szPath,
        &cbPath) == ERROR_SUCCESS)
    {
        if (cbPath + sizeof(szDirectMusicMedia) > MAX_PATH)
        {
            return FALSE;
        }
        
        strcat(szPath, szDirectMusicMedia);
        
        // DirectMusic requires the search path as a wide string
        //
        mbstowcs(wszPath, 
            szPath,
            MAX_PATH);
        bRet = TRUE;
    }
    else
        OutputDebugString( "Could not find registry path for DXSDK!\n" );
    
    RegCloseKey(hkDirectX);
    return bRet;
}

