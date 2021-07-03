//
// MLMain.cpp
//
// WinMain and associated stuff for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>

#include "debug.h"
#include "resource.h"
#include "MusicLines.h"
#include "MLInput.h"
#include "MLRender.h"
#include "MLGame.h"
#include "MLMusic.h"

#define MOVE_TIME_MS           30
extern long gcMoveCount;

// Global class objects
//
Input               *theInput           = NULL;
GraphicsEngine      *theGraphicsEngine  = NULL;
Game                *theGame            = NULL;
Music               *theMusic           = NULL;

static HINSTANCE ghInstance;                        // Our instance handle
static char szAppName[] = "MusicLines";             // Class name and window name
static BOOL fFullScreen = FALSE;                    // Config dialog: is full screen?
static BOOL fPlayer1Human = FALSE;
static BOOL fPlayer2Human = TRUE;

struct DifficultySetting
{
    LPSTR   pstrName;
    int     iSetting;
} 
DifficultySettings[] =
{
    { "Easy",       4 },
    { "Medium",     7 },
    { "Hard",       10 },
    { "Impossible", 9999},
};

static int cDifficultySettings = sizeof(DifficultySettings) / sizeof(DifficultySettings[0]);
static DifficultySetting *pDifficultySetting = &DifficultySettings[2];  // Hard

// WndProc and message handlers
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
BOOL OnCreate(HWND hWnd, CREATESTRUCT FAR* lpCreateStruct);
void OnClose(HWND hWnd);
BOOL DoConfigureDialog();
BOOL CALLBACK  ConfigureDlgProc(HWND hWndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
void SetPlayerCombo(HWND hWndCombo, int PlayerNo, BOOL fIsHuman);
void GetPlayerCombo(HWND hWndCombo, BOOL *fIsHuman);


// Other things to handle locally
//
void CALLBACK CallOnTimer(UINT uTimerID, UINT uMsg, DWORD dwUser, DWORD dw1, DWORD dw2);

// WinMain
//
// Init order is important here.
//
// We first register the main window class. This is done here since the WndProc is global and dispatches
// messages to the classes which need them.
//
// Then we create the graphics engine. It in turn creates the main window. It must be the one to do this
// since the window must created with size and style bits appropriate to fullscreen or windowed activation.
//
// The input class is created in the WM_CREATE handler. The window must have been created at least this far
// in order for the create to succeed.
//
// If all of this succeeds, then the actual game is also created in WM_CREATE. 
//
int WINAPI WinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow)
{
    WNDCLASS wc;
    HWND hWnd;
    MSG msg;
    DWORD dwNextMoveTime;
    LONG lSleep;
    DWORD dwAfterSleep;
    BOOL fMove;

#ifdef _DEBUG
    TraceSetup();

    timeBeginPeriod(1);
#endif

    ghInstance = hInstance;
    CoInitialize(NULL);

    ZeroMemory(&wc, sizeof(wc));
    wc.style                = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc          = WndProc;
    wc.hInstance            = hInstance;
    wc.hIcon                = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor              = LoadCursor(NULL, MAKEINTRESOURCE(IDC_ARROW));
    wc.hbrBackground        = (HBRUSH)(COLOR_WINDOW + 1);
    wc.lpszClassName        = szAppName;

    if (!RegisterClass(&wc))
    {
        MessageBox(NULL,
                   "RegisterClass failed",
                   szAppName,
                   MB_ICONEXCLAMATION|MB_OK);
		goto Cleanup;
    }

    if (!DoConfigureDialog()) 
    {
       goto Cleanup;
    }

    theGraphicsEngine = new GraphicsEngine(szAppName, fFullScreen);
    hWnd = theGraphicsEngine->Enter();

    if (hWnd == NULL)
    {
        MessageBox(NULL,
                   "Failed to initialize graphics engine.",
                   szAppName,
                   MB_ICONEXCLAMATION|MB_OK);
        goto Cleanup;
    }

    theInput = new Input();

    if (!theInput->Enable(ghInstance, hWnd))
    {
        theGraphicsEngine->SetDialogUp(TRUE);
        MessageBox(hWnd,
                   "Failed to initialize DirectInput",
                   szAppName,
                   MB_ICONEXCLAMATION|MB_OK);
        theGraphicsEngine->SetDialogUp(FALSE);
        return FALSE;
    }

    theMusic = new Music();
    if (!theMusic->Initialize(hWnd))
    {
        theGraphicsEngine->SetDialogUp(TRUE);
        MessageBox(hWnd,
                   "Failed to initialize DirectMusic.",
                   szAppName,
                   MB_ICONEXCLAMATION|MB_OK);
        theGraphicsEngine->SetDialogUp(FALSE);
        return FALSE;
    }

    Game::PlayerType types[2];
    types[0] = fPlayer1Human ? Game::PlayerHuman : Game::PlayerComputer;
    types[1] = fPlayer2Human ? Game::PlayerHuman : Game::PlayerComputer;

    theGame = new Game(hWnd, pDifficultySetting->iSetting, types);
    if (!theGame->Initialize())
    {
        theGraphicsEngine->SetDialogUp(TRUE);
        MessageBox(hWnd,
                   "Failed to initialize game logic.",
                   szAppName,
                   MB_ICONEXCLAMATION|MB_OK);
        theGraphicsEngine->SetDialogUp(FALSE);
        return FALSE;
    }
 
    dwNextMoveTime = 0;
    dwAfterSleep = timeGetTime();
    for (;;)
    {
        if (PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
        {
            if (!GetMessage(&msg, NULL, 0, 0))
            {
                break;
            }
             
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        else
        {
            fMove = FALSE;
            if (gcMoveCount > 1)
            {
                fMove = theGraphicsEngine->Active();
                gcMoveCount -= 1;
            }
            
            theGame->RenderFrame(fMove);

            lSleep = timeGetTime() - dwAfterSleep;
            if (lSleep < 10)
            {
                lSleep = 10;
            }

            Sleep(lSleep);
            dwAfterSleep = timeGetTime();
        }
    }

    Trace(0, "Out of message loop");

Cleanup:
    if (theInput)
    {
         theInput->Disable();
         delete theInput;
    }

    if (theGraphicsEngine)
    {
        theGraphicsEngine->Leave();
        delete theGraphicsEngine;
    }

    if (theMusic)
    {
        delete theMusic;
    }

    CoUninitialize();
#ifdef _DEBUG
    timeEndPeriod(1);
#endif

    return msg.wParam;
}           

// WndProc
//
// Let the graphics engine take a crack at the message; otherwise, handle the few
// standard messages we process.
//
LRESULT CALLBACK WndProc(
    HWND hWnd, 
    UINT uMsg, 
    WPARAM wParam, 
    LPARAM lParam)
{
    LRESULT lResult;

    // Graphics engine gets first shot at any message.
    //
    if (theGraphicsEngine->WndProc(hWnd, uMsg, wParam, lParam, &lResult))
    {
        return lResult;
    }

    // We get whatever's left. Not a whole lot is done here.
    //
    switch (uMsg)
    {
        HANDLE_MSG(hWnd, WM_CREATE,             OnCreate);
        HANDLE_MSG(hWnd, WM_CLOSE,              OnClose);

    default:
        return DefWindowProc(hWnd, uMsg, wParam, lParam);
    }    
}

// OnCreate
//
// Try to create the input class object, then the game.
// Fail creation of the window on any failure.
//
BOOL OnCreate(
    HWND hWnd, 
    CREATESTRUCT FAR* lpCreateStruct)
{
    return TRUE;
}

// OnClose
//
// Just shut down the message loop. Everything else will be done after the loop
// exits.
//
// Exception is the timer so we won't be trying to update the display during shutdown.
//
void OnClose(
    HWND hWnd)
{
    PostQuitMessage(0);
}

BOOL DoConfigureDialog()
{
    return (BOOL)DialogBox(ghInstance, 
                           MAKEINTRESOURCE(IDD_CONFIGURE),
                           NULL,
                           (DLGPROC)ConfigureDlgProc);
}

BOOL CALLBACK ConfigureDlgProc(
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    HWND hWndCombo;
    int  idxCombo;
    int  idxDifficulty;
    DifficultySetting *pDiff;
    BOOL fTemp;

    switch (uMsg)
    {
    case WM_INITDIALOG:
        CheckRadioButton(hWnd, 
                         IDC_FULLSCREEN, 
                         IDC_WINDOWED, 
                         fFullScreen ? IDC_FULLSCREEN : IDC_WINDOWED);

        hWndCombo = GetDlgItem(hWnd, IDC_DIFFICULTY);

        pDiff = DifficultySettings;
        for (idxDifficulty = 0; idxDifficulty < cDifficultySettings; ++idxDifficulty, ++pDiff)
        {
            idxCombo = ComboBox_AddString(hWndCombo, pDiff->pstrName);
            if (idxCombo >= 0) 
            {
                ComboBox_SetItemData(hWndCombo, idxCombo, (LPARAM)pDiff);         
                if (pDifficultySetting == pDiff)
                {
                    ComboBox_SetCurSel(hWndCombo, idxCombo);
                }
            }
        }

        SetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER1), 1, fPlayer1Human);
        SetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER2), 2, fPlayer2Human);

        return TRUE;

    case WM_COMMAND:
        switch (LOWORD(wParam))
        {
            case IDC_PLAYER1:
            case IDC_PLAYER2:
                GetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER1), &fTemp);
                if (fTemp)
                {
                    GetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER2), &fTemp);
                }

                EnableWindow(GetDlgItem(hWnd, IDC_DIFFICULTY), !fTemp);

                break;

            case IDOK:
                fFullScreen       = (IsDlgButtonChecked(hWnd, IDC_FULLSCREEN) == BST_CHECKED);

                hWndCombo = GetDlgItem(hWnd, IDC_DIFFICULTY);
                idxCombo = ComboBox_GetCurSel(hWndCombo);
                if (idxCombo >= 0)
                {
                    pDifficultySetting = (DifficultySetting*)ComboBox_GetItemData(hWndCombo, idxCombo);
                }

                GetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER1), &fPlayer1Human);
                GetPlayerCombo(GetDlgItem(hWnd, IDC_PLAYER2), &fPlayer2Human);

                EndDialog(hWnd, 1);
                return TRUE;

            case IDCANCEL:
                EndDialog(hWnd, 0);
                return TRUE;
        }
        break;
    }

    return FALSE;
}

void SetPlayerCombo(
    HWND hWndCombo,
    int PlayerNo, 
    BOOL fIsHuman)
{
    int idxCombo;

    idxCombo = ComboBox_AddString(hWndCombo, 
                                 (PlayerNo == 1 ? "Human [AWDS]" : "Human [Arrow keys]"));
    if (idxCombo >= 0)
    {
        ComboBox_SetItemData(hWndCombo, idxCombo, 1);
        if (fIsHuman)
        {
            ComboBox_SetCurSel(hWndCombo, idxCombo);
        }
    }
    
    idxCombo = ComboBox_AddString(hWndCombo, "Computer");
    if (idxCombo >= 0)
    {
        ComboBox_SetItemData(hWndCombo, idxCombo, 0);
        if (!fIsHuman)
        {
            ComboBox_SetCurSel(hWndCombo, idxCombo);
        }
    }
}    

void GetPlayerCombo(
    HWND hWndCombo,
    BOOL *fIsHuman)
{
    int idxCombo;

    idxCombo = ComboBox_GetCurSel(hWndCombo);
    if (idxCombo >= 0)
    {
        *fIsHuman = (BOOL)ComboBox_GetItemData(hWndCombo, idxCombo);
    }
}
