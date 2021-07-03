/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: color.cpp
 *
 ***************************************************************************/

#include <d3drmwin.h>
#include "viewer.h"
#include <windows.h>
#include <string.h>
#include <commdlg.h>
#include <d3drm.h>

static DWORD dwNewAlpha;

VOID
setAlphaDialog(HWND hdlg, DWORD dwVal)
{
    TCHAR szVal[5];

    wsprintf(szVal, "%d", dwVal);
    SendDlgItemMessage(hdlg, COLOR_ALPHA, EM_SETSEL,
		       (WPARAM)0, (LPARAM)-1);
    SendDlgItemMessage(hdlg, COLOR_ALPHA, EM_REPLACESEL,
		       (WPARAM)FALSE, (LPARAM)szVal);
}

#define COLOR_OK	0
#define COLOR_BADVAL	1
#define COLOR_NONEWVAL	2

DWORD
getAlphaFromDialog(HWND hdlg, LPDWORD pdwAlpha)
{
    WORD cchAlpha;
    TCHAR szAlpha[5];
    TCHAR* pszStop;
    DWORD dwAlpha;

    cchAlpha = (WORD)SendDlgItemMessage(hdlg, COLOR_ALPHA,
					EM_LINELENGTH,
					(WPARAM)0, (LPARAM)0);
    if (cchAlpha > 3)
    {
	return (COLOR_BADVAL);
    }

    if (cchAlpha == 0)
    {
	return (COLOR_NONEWVAL);
    }

    memcpy( szAlpha, &cchAlpha, sizeof(WORD) );
    SendDlgItemMessage(hdlg, COLOR_ALPHA, EM_GETLINE,
		       (WPARAM)0, (LPARAM)szAlpha);
    szAlpha[cchAlpha] = '\0';
    pszStop = &szAlpha[cchAlpha];
    dwAlpha = strtoul(szAlpha, &pszStop, 10);
    if (dwAlpha > 255)
    {
	return (COLOR_BADVAL);
    }

    *pdwAlpha = dwAlpha;

    return (COLOR_OK);
}

UINT APIENTRY CCHookProc(HWND hdlg, UINT uiMsg, WPARAM wParam, LPARAM lParam)
{
    DWORD dwAlpha;
    DWORD dwRet;

    switch (uiMsg)
    {
	case WM_INITDIALOG:
	    setAlphaDialog(hdlg, dwNewAlpha);
	    break;

	case WM_COMMAND:
	    if (wParam == IDOK)
	    {
		/*
		 * Get the Alpha Value
		 */
		dwRet = getAlphaFromDialog(hdlg, &dwAlpha);
		switch (dwRet)
		{
		    case COLOR_NONEWVAL:
		    	break;
		    case COLOR_OK:
		    	dwNewAlpha = dwAlpha;
			break;
		    case COLOR_BADVAL:
			setAlphaDialog(hdlg, dwNewAlpha);
			break;
		}
	    }

	    break;
    }

    return 0; /* Always give default behaviour */
}


int ChooseNewColor(HWND win, D3DCOLOR* current)
{
    CHOOSECOLOR cc;
    COLORREF clr;
    COLORREF aclrCust[16];
    int i;

    for (i = 0; i < 16; i++)
	aclrCust[i] = RGB(255, 255, 255);

    clr =
	RGB
	(   (int) (255 * D3DRMColorGetRed(*current)),
	    (int) (255 * D3DRMColorGetGreen(*current)),
	    (int) (255 * D3DRMColorGetBlue(*current))
	);

    memset(&cc, 0, sizeof(CHOOSECOLOR));
    cc.lStructSize = sizeof(CHOOSECOLOR);
    cc.hwndOwner = win;
    cc.hInstance = (HWND)GetWindowLong(win, GWL_HINSTANCE);
    cc.rgbResult = clr;
    cc.lpCustColors = aclrCust;
    cc.Flags = CC_RGBINIT|CC_FULLOPEN|CC_ENABLETEMPLATE|CC_ENABLEHOOK;
    cc.lpfnHook = CCHookProc;
    cc.lpTemplateName = "ViewerChooseColor";
    dwNewAlpha = (DWORD) (255 * D3DRMColorGetAlpha(*current));

    if (ChooseColor(&cc))
    {	*current =
	    D3DRMCreateColorRGBA
	    (	D3DVAL(GetRValue(cc.rgbResult) / D3DVAL(255.0)),
		D3DVAL(GetGValue(cc.rgbResult) / D3DVAL(255.0)),
		D3DVAL(GetBValue(cc.rgbResult) / D3DVAL(255.0)),
		D3DVAL(dwNewAlpha / D3DVAL(255.0))
	    );
	return TRUE;
    }
    else return FALSE;
}
