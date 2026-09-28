// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DlgCtrl.h"
#ifdef __linux__
#include "DlgCtrlLocal.h"
#include "OrbiterResource.h"
#include <QImage>
#endif // __linux__

#ifndef __linux__
LRESULT FAR PASCAL MsgProc_PropertyList (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
static QWidget *CreatePropertyList (const RESCONTROL*, QWidget *parent) { return new PropertyListCtrl (parent); }
#endif // __linux__

#ifndef __linux__
void RegisterPropertyList (HINSTANCE hInst)
#else // __linux__
void RegisterPropertyList (void *hInst)
#endif // __linux__
{
#ifndef __linux__
	WNDCLASS wndClass;
#else // __linux__
	// Register window class for property list
	oapiRegisterResControl (hInst, "OrbiterCtrl_PropertyList", CreatePropertyList);
#endif // __linux__

#ifndef __linux__
	// Register window class for level indicator
	wndClass.style = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc   = MsgProc_PropertyList;
	wndClass.cbClsExtra    = 0;
	wndClass.cbWndExtra    = 16;
	wndClass.hInstance     = hInst;
	wndClass.hIcon         = NULL;
	wndClass.hCursor       = LoadCursor (NULL, IDC_ARROW);
	wndClass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH);
	wndClass.lpszMenuName  = NULL;
	wndClass.lpszClassName = "OrbiterCtrl_PropertyList";
	RegisterClass (&wndClass);

	HMODULE hExeInst = GetModuleHandle (NULL);
	PropertyList::hBmpArrows = LoadBitmap (hExeInst, MAKEINTRESOURCE (286));
#else // __linux__
	// arrow bitmap from Orbiter's own resources
	PropertyList::hBmpArrows = oapiLoadResImage (NULL, 286);
#endif // __linux__
}

#ifndef __linux__
void UnregisterPropertyList (HINSTANCE hInst)
#else // __linux__
void UnregisterPropertyList (void *hInst)
#endif // __linux__
{
#ifndef __linux__
	UnregisterClass ("OrbiterCtrl_PropertyList", hInst);
	DeleteObject (PropertyList::hBmpArrows);
}

LRESULT FAR PASCAL MsgProc_PropertyList (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	PropertyList *pl;

	switch (uMsg) {
	case WM_PAINT:
		pl = (PropertyList*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
		pl->OnPaint (hWnd);
		return 0;
	case WM_SIZE:
		pl = (PropertyList*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
		if (pl) pl->OnSize (LOWORD (lParam), HIWORD (lParam));
		return 0;
	case WM_VSCROLL:
		pl = (PropertyList*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
		pl->OnVScroll (LOWORD(wParam), HIWORD(wParam));
		return 0;
	case WM_LBUTTONDOWN:
		pl = (PropertyList*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
		pl->OnLButtonDown (LOWORD(lParam), HIWORD(lParam));
		return 0;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
	oapiUnregisterResControl (hInst, "OrbiterCtrl_PropertyList");
	delete PropertyList::hBmpArrows;
	PropertyList::hBmpArrows = NULL;
#endif // __linux__
}
