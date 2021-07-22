// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DlgCtrl.h"

long FAR PASCAL MsgProc_PropertyList (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

void RegisterPropertyList (HINSTANCE hInst)
{
	WNDCLASS wndClass;

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
}

void UnregisterPropertyList (HINSTANCE hInst)
{
	UnregisterClass ("OrbiterCtrl_PropertyList", hInst);
	DeleteObject (PropertyList::hBmpArrows);
}

long FAR PASCAL MsgProc_PropertyList (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	PropertyList *pl;

	switch (uMsg) {
	case WM_PAINT:
		pl = (PropertyList*)GetWindowLong (hWnd, GWL_USERDATA);
		pl->OnPaint (hWnd);
		return 0;
	case WM_SIZE:
		pl = (PropertyList*)GetWindowLong (hWnd, GWL_USERDATA);
		if (pl) pl->OnSize (LOWORD (lParam), HIWORD (lParam));
		return 0;
	case WM_VSCROLL:
		pl = (PropertyList*)GetWindowLong (hWnd, GWL_USERDATA);
		pl->OnVScroll (LOWORD(wParam), HIWORD(wParam));
		return 0;
	case WM_LBUTTONDOWN:
		pl = (PropertyList*)GetWindowLong (hWnd, GWL_USERDATA);
		pl->OnLButtonDown (LOWORD(lParam), HIWORD(lParam));
		return 0;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}
