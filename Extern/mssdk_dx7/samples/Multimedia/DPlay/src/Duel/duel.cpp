//-----------------------------------------------------------------------------
// File: Duel.cpp
//
// Desc: Multi-player game
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include "duel.h"
#include "gameproc.h"
#include "gfx.h"
#include "DPUtil.h"
#include "diutil.h"
#include "dsutil.h"
#include "lobby.h"



//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
// This GUID allows DirectPlay to find other instances of the same game on
// the network.  So it must be unique for every game, and the same for 
// every instance of that game.  // {33925241-05F8-11d0-8063-00A0C90AE891}
GUID g_AppGUID = { 0x33925241, 0x5f8, 0x11d0, { 0x80, 0x63, 0x0, 0xa0, 0xc9, 0xa, 0xe8, 0x91 } };

extern DWORD          g_dwFrameCount;
extern DWORD          g_dwFrameTime;
extern int            g_nProgramState;
extern SHIP           g_OurShip;
extern DPID           g_LocalPlayerDPID;

static BOOL           g_bReinitialize; // Used for switching display modes

TCHAR     g_strAppName[256] = "Duel";               // The name of the sample
HANDLE    g_hDPMessageEvent = NULL;                 // Not used in this sample, needed for DPConnect.cpp
TCHAR     g_strLocalPlayerName[MAX_PLAYER_NAME];    // Local player name
TCHAR     g_strSessionName[MAX_SESSION_NAME];       // Default session name
TCHAR     g_strPreferredProvider[MAX_SESSION_NAME]; // Default preferred provider

LPDPLCONNECTION    g_pDPLConnection = NULL;  
LPDIRECTPLAYLOBBY3 g_pDPLobby       = NULL;  

HWND      g_hwndMain;             // Main application window handle
HKEY      g_hDuelKey = NULL;      // Duel registry key handle
HINSTANCE g_hInst;                // Application instance handle        
BOOL      g_bShowFrameCount=TRUE; // Show FPS ?
BOOL      g_bIsActive;            // Is the application active ?
BOOL      g_bHostPlayer;          // Are we hosting or joining a game       
DWORD     g_dwKeys;               // User keyboard input
DWORD     g_dwOldKeys;            // Last frame's keyboard input
BOOL      g_bFullscreen=FALSE;    // Window or FullScreen mode ?
RECT      g_rcWindow;             // client rectangle of main window
BOOL      g_bReliable;            // sends are reliable
BOOL      g_bAsync;               // asynchronous sends
BOOL      g_bAsyncSupported;      // asynchronous sends supported
BOOL      g_bUseProtocol;         // DirectPlay Protocol messaging




//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
extern int     DPConnect_StartDirectPlayConnect( HINSTANCE hInst, BOOL bBackTrack = FALSE );
extern HRESULT DPConnect_CheckForLobbyLaunch( BOOL* pbLaunchedByLobby );

LONG WINAPI MainWndproc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT InitApplication( HINSTANCE hInst );
HRESULT ReadRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue, DWORD dwLength, TCHAR* strDefault );
HRESULT WriteRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue );
VOID    CleanupApplication();
BOOL    WasLaunchedByLobby();
BOOL    FinishLobbyLaunch();
VOID    DoHelp();




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc:
//-----------------------------------------------------------------------------
int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE, LPSTR, int )
{
    MSG     msg;
    BOOL    bLaunchedByLobby;
    HRESULT hr;

    g_hInst = hInstance;

    // Read information from registry
    RegCreateKeyEx( HKEY_CURRENT_USER, DUEL_KEY, 0, NULL,
                    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, 
                    &g_hDuelKey, NULL );

    ReadRegKey( g_hDuelKey, "Player Name", 
                g_strLocalPlayerName, MAX_PLAYER_NAME, "" );
    ReadRegKey( g_hDuelKey, "Session Name", 
                g_strSessionName, MAX_SESSION_NAME, "" );
    ReadRegKey( g_hDuelKey, "Preferred Provider", 
                g_strPreferredProvider, MAX_SESSION_NAME, "" );

    CoInitialize( NULL );

    if( FAILED( InitApplication( hInstance ) ) )
        return 0;

    // See if we were launched from a lobby server
    hr = DPConnect_CheckForLobbyLaunch( &bLaunchedByLobby );
    if( FAILED(hr) )
        return 1;

    if( bLaunchedByLobby )
    {
        // Start game
        PostMessage( g_hwndMain, UM_LAUNCH, 0, 0 );
        g_bIsActive = TRUE;
    }

    g_dwFrameTime = timeGetTime();

    while( TRUE )
    {
        if( g_bIsActive )
        {
            // Any windows messages ? (returns immediately)
            if( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
            {
                if( !GetMessage( &msg, NULL, 0, 0 ) )
                    break;

                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
            else
            {
                // Poll our receive queue. Polling is used in the sample only for simplicity.
                // Receiving messages using an event is the recommended way.
                if( g_nProgramState != PS_SPLASH )
                {
                    ReceiveMessages();
                    LobbyMessageReceive(LMR_PROPERTIES);
                }

                // update screen
                if( !UpdateFrame() )
                    ExitGame();     // posts QUIT msg
            }
        }
        else
        {
            // Any windows messages ? (blocks until a message arrives)
            if( !GetMessage( &msg, NULL, 0, 0 ) )
                break;

            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
    }

    CoUninitialize();

    // Write information to the registry
    WriteRegKey( g_hDuelKey, "Player Name", g_strLocalPlayerName );
    WriteRegKey( g_hDuelKey, "Session Name", g_strSessionName );
    WriteRegKey( g_hDuelKey, "Preferred Provider", g_strPreferredProvider );

    return msg.wParam;
}




//-----------------------------------------------------------------------------
// Name: MainWndproc()
// Desc: Callback for all Windows messages
//-----------------------------------------------------------------------------
LONG WINAPI MainWndproc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    PAINTSTRUCT ps;
    HDC         hdc;

    switch( msg )
    {
        case WM_SIZE:
        case WM_MOVE:
            // Get the client rectangle
            if( g_bFullscreen )
            {
                SetRect( &g_rcWindow, 0, 0, GetSystemMetrics(SM_CXSCREEN),
                         GetSystemMetrics(SM_CYSCREEN) );
            }
            else
            {
                GetClientRect( hWnd, &g_rcWindow );
                ClientToScreen( hWnd, (POINT*)&g_rcWindow );
                ClientToScreen( hWnd, (POINT*)&g_rcWindow+1 );
            }
            break;

        case WM_ACTIVATE:
            // Ignore this message during reinitializing graphics
            if( g_bReinitialize )
                return 0;

            // When we are deactivated, although we don't update our screen, we
            // still need to to empty our receive queue periodically as
            // messages will pile up otherwise. Polling the receive queue
            // continuously even when we are deactivated causes our app to
            // consume all the CPU time. To avoid hogging the CPU, we block on
            // GetMessage() WIN API and setup a timer to wake ourselves up at
            // regular intervals to process our messages.

            if( LOWORD(wParam) == WA_INACTIVE )
            {
                // Aeactivated
                g_bIsActive = FALSE;
                if( PS_ACTIVE == g_nProgramState )
                    SetTimer( hWnd, RECEIVE_TIMER_ID, RECEIVE_TIMEOUT, NULL );
            }
            else
            {
                // Activated
                g_bIsActive = TRUE;
                if( PS_ACTIVE == g_nProgramState )
                    KillTimer( hWnd, RECEIVE_TIMER_ID );
            }

            // set game palette, if activated in game mode
            if( g_bIsActive && (g_nProgramState != PS_SPLASH) )
                SetGamePalette();

            DIUtil_ReacquireInputDevices();

            return 0;

        case WM_CREATE:
            break;

        case WM_SYSKEYUP:
            switch( wParam )
            {
                // Handle ALT+ENTER (fullscreen/window mode)
                case VK_RETURN:
                    // Mode switch is allowed only during the game
                    if( g_nProgramState == PS_ACTIVE )
                    {
                        g_bReinitialize = TRUE;
                        ReleaseLocalData();  //only sound buffers have to be rels'd anyway.
                        CleanupGameSounds();
                        DIUtil_CleanupInput();
                        CleanupGraphics();
                        DestroyWindow( g_hwndMain );
                        g_bFullscreen = !g_bFullscreen;
                        InitGraphics();
                        DIUtil_InitInput( g_hwndMain );
                        InitializeGameSounds();
                        InitLocalSoundData();
                        g_bReinitialize = FALSE;
                    }
                    break;
            }
            break;

        case WM_KEYDOWN:
            switch( wParam )
            {
                case 'a':
                case 'A':
                    // Toggle Async sends on/off
                    if( g_bAsyncSupported )
                    {
                        g_bAsync = !g_bAsync;
                        UpdateTitle();      // caption bar status
                    }
                    break;

                case 'r':
                case 'R':
                    // Toggle reliable sends
                    g_bReliable = !g_bReliable;
                    UpdateTitle();
                    break;

                case VK_F1:
                    // Display help
                    DoHelp();
                    break;

                case VK_F5:
                    // Toggle frame rate display
                    g_bShowFrameCount = !g_bShowFrameCount;
                    if( g_bShowFrameCount )
                    {
                        g_dwFrameCount = 0;
                        g_dwFrameTime = timeGetTime();
                    }
                    break;

                case VK_RETURN:
                    // Launch game setup wizard
                    if( (g_nProgramState == PS_SPLASH) && !g_bFullscreen )
                    {
                        int nExitCode;
                        nExitCode = DPConnect_StartDirectPlayConnect( g_hInst, FALSE );

                        // Figure out what happened, and post a reflecting message
                        if( nExitCode == EXITCODE_FORWARD )
                            PostMessage(g_hwndMain, UM_LAUNCH, 0, 0);

                        if( nExitCode == EXITCODE_QUIT )
                            PostMessage(g_hwndMain, UM_ABORT, 0, 0);

                        if( nExitCode == EXITCODE_LOBBYCONNECT )
                            PostMessage( g_hwndMain, UM_LAUNCH, 0, 0 );

                        if( nExitCode == EXITCODE_ERROR )
                        {
                            MessageBox( g_hwndMain, TEXT("Mutliplayer connect failed. "
                                        "The sample will now quit."),
                                        TEXT("DirectPlay Sample"), MB_OK | MB_ICONERROR );
                            PostMessage(g_hwndMain, UM_ABORT, 0, 0);
                        }
                    }
                    break;

                case VK_ESCAPE:
                case VK_F12:
                    // Exit the game
                    ExitGame();
                    return 0;
            }
            break;

        case WM_ERASEBKGND:
            return 1;

        case WM_PAINT:
            hdc = BeginPaint( hWnd, &ps );
            if( g_nProgramState == PS_SPLASH )
            {
                // Display the splash screen
                BltSplashScreen( NULL );
            }

            EndPaint( hWnd, &ps );
            return 1;

        case UM_LAUNCH:
        case UM_ABORT:
            // if we were launched by the lobby and not (failed to finish a lobby launch)
            // where wParam is bLobbyLaunched
            if( msg == UM_LAUNCH )
            {
                // Init lobby msg support for reporting score
                // Note that we don't release the lobby object
                LobbyMessageInit();

                // Start the game in rest mode
                g_nProgramState = PS_REST;
                LaunchGame();
                return 1;
            }
            // Else aborting
            ExitGame();
            return 1;

        case WM_TIMER:
            ReceiveMessages();
            LobbyMessageReceive( LMR_PROPERTIES );
            break;

        case WM_DESTROY:
            // If g_bReinitialize is TRUE don't quit, we are just switching
            // display modes
            if( !g_bReinitialize )
            {
                CleanupApplication();
                PostQuitMessage( 0 );
            }
            return 0;

        default:
            break;
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}



//-----------------------------------------------------------------------------
// Name: InitApplication()
// Desc: Do that initialization stuff...
//-----------------------------------------------------------------------------
HRESULT InitApplication( HINSTANCE hInst )
{
    WNDCLASS wndClass = { CS_DBLCLKS, MainWndproc, 0, 0, hInst,
                          LoadIcon( hInst, MAKEINTRESOURCE(IDI_MAIN)),
                          LoadCursor(NULL, IDC_ARROW), 
                          (HBRUSH)GetStockObject(BLACK_BRUSH),
                          NULL, TEXT("DuelClass") };
    RegisterClass( &wndClass );

    // Initialize all components
    if( FAILED( InitGraphics() ) )
        return E_FAIL;
    
    if( FAILED( DIUtil_InitInput( g_hwndMain ) ) )
        return E_FAIL;

    if( FAILED( InitializeGameSounds() ) )
    {
        // Can play game without sound. Do not exit
    }

    // Start in splash mode
    g_nProgramState = PS_SPLASH;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CleanupApplication()
// Desc: Calls clean up on all components
//-----------------------------------------------------------------------------
VOID CleanupApplication()
{
    CleanupComm();
    CleanupGameSounds();
    CleanupGraphics();
    DIUtil_CleanupInput();
    DPLobbyRelease();               // in case we were doing lobby messages
}




//-----------------------------------------------------------------------------
// Name: ShowError()
// Desc: Displays error to the user
//-----------------------------------------------------------------------------
VOID ShowError( int iStrID )
{
    TCHAR strMsg[MAX_ERRORMSG];
    LoadString( g_hInst, iStrID, strMsg, MAX_ERRORMSG );
    MessageBox( g_hwndMain, strMsg, TEXT("Duel Message"), MB_OK );
}




//-----------------------------------------------------------------------------
// Name: UpdateTitle()
// Desc: Updates the window title based on application status
//-----------------------------------------------------------------------------
VOID UpdateTitle()
{
    // Build the window title
    TCHAR strTitle[MAX_WINDOWTITLE] = TEXT("Duel");

    // State options in window title
    if( g_bHostPlayer | g_bUseProtocol | g_bReliable | g_bAsync )
    {   
        strcat( strTitle, " - |" );
        if( g_bHostPlayer )
            _tcscat( strTitle, TEXT(" Host |") );
        if( g_bUseProtocol )
            _tcscat( strTitle, TEXT(" Protocol |") );
        if( g_bReliable )
            _tcscat( strTitle, TEXT(" Reliable |") );
        if( g_bAsync )
            _tcscat( strTitle, TEXT(" Async |") );
    }

    // Change window title
    SetWindowText( g_hwndMain, strTitle );
}




//-----------------------------------------------------------------------------
// Name: DoHelp()
// Desc: Display a Help summary in a message box.
//-----------------------------------------------------------------------------
VOID DoHelp()
{
    TCHAR strHelpMsg[MAX_HELPMSG];
    LoadString( g_hInst, IDS_DUEL_HELP, strHelpMsg, MAX_HELPMSG );
    MessageBox( g_hwndMain, strHelpMsg, TEXT("DUEL"), MB_OK );
}




//-----------------------------------------------------------------------------
// Name: ReadRegKey()
// Desc: Read a registry key 
//-----------------------------------------------------------------------------
HRESULT ReadRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue, 
                    DWORD dwLength, TCHAR* strDefault )
{
	DWORD dwType;
	LONG bResult;

	bResult = RegQueryValueEx( hKey, strName, 0, &dwType, 
							 (LPBYTE) strValue, &dwLength );
	if ( bResult != ERROR_SUCCESS )
		strcpy( strValue, strDefault );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WriteRegKey()
// Desc: Writes a registry key 
//-----------------------------------------------------------------------------
HRESULT WriteRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue )
{
	LONG bResult;

	bResult = RegSetValueEx( hKey, strName, 0, REG_SZ, 
							 (LPBYTE) strValue, strlen(strValue) + 1 );
	if ( bResult != ERROR_SUCCESS )
		return E_FAIL;

    return S_OK;
}

