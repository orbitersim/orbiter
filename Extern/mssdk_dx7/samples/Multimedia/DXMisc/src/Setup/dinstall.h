//-----------------------------------------------------------------------------
// File: DInstall.H
//
// Desc: Header file for example code showing how to use DirectXSetup.
//
//
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
void SetButtons(HWND hDlg, DWORD wMsgType);
DLGPROC DlgProc(HWND hDlg, WORD message, WPARAM wParam, LPARAM lParam);
BOOL DirectXInstall(HWND hWnd);
void DirectXGetVersion(void );

//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------

#define DSETUP_REINSTALL	0x00000080

#define SHOW_ALL		1
#define SHOW_UPGRADES	2
#define SHOW_PROBLEMS	3
#define SHOW_NONE		4
