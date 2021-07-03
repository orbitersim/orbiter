/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: file.cpp
 *
 ***************************************************************************/

#include <d3drmwin.h>
#include "viewer.h"
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <commdlg.h>

char* OpenNewFile( HWND hwnd, const char *wndTitle )
{
    static char file[256];
    static char fileTitle[256];
    static char filter[] = "X files (*.x)\0*.x\0"
			   "All Files (*.*)\0*.*\0";
    OPENFILENAME ofn;

    lstrcpy( file, "");
    lstrcpy( fileTitle, "");

    ofn.lStructSize       = sizeof(OPENFILENAME);
    ofn.hwndOwner         = hwnd;
#ifdef WIN32
    ofn.hInstance         = (HINSTANCE) GetWindowLong(hwnd, GWL_HINSTANCE);
#else
    ofn.hInstance         = (HINSTANCE) GetWindowWord(hwnd, GWW_HINSTANCE);
#endif
    ofn.lpstrFilter       = filter;
    ofn.lpstrCustomFilter = (LPSTR) NULL;
    ofn.nMaxCustFilter    = 0L;
    ofn.nFilterIndex      = 1L;
    ofn.lpstrFile         = file;
    ofn.nMaxFile          = sizeof(file);
    ofn.lpstrFileTitle    = fileTitle;
    ofn.nMaxFileTitle     = sizeof(fileTitle);
    ofn.lpstrInitialDir   = NULL;
    ofn.lpstrTitle        = wndTitle;
    ofn.nFileOffset       = 0;
    ofn.nFileExtension    = 0;
    ofn.lpstrDefExt       = "*.x";
    ofn.lCustData         = 0;

    ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

    if (GetOpenFileName(&ofn))
        return (char*)ofn.lpstrFile;
    else
        return NULL;
}
