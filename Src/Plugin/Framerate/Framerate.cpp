// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Framerate
//                  Part of the ORBITER SDK
//
// Framerate.cpp
// Dialog box for displaying simulation frame rate.
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include <windows.h>
#include <stdio.h>
#include "Orbitersdk.h"
#include "Dialog\Graph.h"
#include "resource.h"

static char *desc = "Simulation frame rate / time step monitor";

// ==============================================================
// Global variables

HINSTANCE g_hInst;                  // module instance handle
HWND g_hDlg;                        // dialog handle
DWORD g_dwCmd;                      // custom function identifier
Graph *g_Graph[2] = {0,0};          // frame rate/time step graphs
double g_T = 0.0;                   // sample system time
double g_simT = 0.0;                // sample simulation time
double g_DT = 1.0;			        // sample interval
DWORD g_fcount;                     // frame counter
bool bDisplay = false;              // display open?
bool bShowGraph[2] = {true, false}; // show graphs?

// ==============================================================
// Local prototypes

void OpenDlgClbk (void *context);
INT_PTR CALLBACK MsgProc (HWND, UINT, WPARAM, LPARAM);
LONG_PTR FAR PASCAL Graph_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

// ==============================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_hInst = hDLL;
	g_hDlg = 0;
	g_dwCmd = oapiRegisterCustomCmd ("Performance Meter", desc, OpenDlgClbk, NULL);

	// register the window class for the data graph
	WNDCLASS wndClass;
	wndClass.style         = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc   = Graph_WndProc;
	wndClass.cbClsExtra    = 0;
	wndClass.cbWndExtra    = 4;
	wndClass.hInstance     = hDLL;
	wndClass.hIcon         = NULL;
	wndClass.hCursor       = LoadCursor (NULL, IDC_CROSS);
	wndClass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH);
	wndClass.lpszMenuName  = NULL;
	wndClass.lpszClassName = "PerfGraphWindow";
	RegisterClass (&wndClass);

	Graph::InitGDI();
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	UnregisterClass ("PerfGraphWindow", g_hInst);
	oapiUnregisterCustomCmd (g_dwCmd);

	Graph::FreeGDI();
}

DLLCLBK void opcPreStep (double simt, double simdt, double mjd)
{
	if (!bDisplay) return; // flight data dialog not open

	double syst = oapiGetSysTime(); // ignore time acceleration for graph updates
	g_fcount++;

	if (syst >= g_T+g_DT) {
		char cbuf[128];
		if (bShowGraph[0]) {
			float fps = (float)(g_fcount / (syst-g_T));
			sprintf (cbuf, "FPS: %0.0f", fps);
			g_Graph[0]->AppendDataPoint (fps);
			g_Graph[0]->SetTitle (cbuf);
			InvalidateRect (GetDlgItem (g_hDlg, IDC_FRAMERATE), NULL, TRUE);
		}
		if (bShowGraph[1] && g_fcount) {
			float dt = (float)(simt - g_simT) / (float)g_fcount;
			sprintf (cbuf, "dt: %0.3fs", dt);
			g_Graph[1]->AppendDataPoint (dt);
			g_Graph[1]->SetTitle (cbuf);
			InvalidateRect (GetDlgItem (g_hDlg, IDC_TIMESTEP), NULL, TRUE);
		}
		g_T      = syst;
		g_simT   = simt;
		g_fcount = 0;
	}
}

void OpenDlgClbk (void *context)
{
	HWND hDlg = oapiOpenDialog (g_hInst, IDD_FRAMERATE, MsgProc);
	if (hDlg) g_hDlg = hDlg; // otherwise open already
}

void ArrangeGraphs (HWND hDlg)
{
	static int hdrofs = 0;
	RECT r;
	GetClientRect (hDlg, &r);
	if (!hdrofs) {
		RECT r2;
		GetWindowRect (GetDlgItem (hDlg, IDC_FRAMERATE), &r2);
		hdrofs = r.bottom - (r2.bottom-r2.top);
	}
	int h = r.bottom-hdrofs;
	int w = r.right;

	SetWindowPos (GetDlgItem (hDlg, IDC_FRAMERATE), 0,
	  			  0, hdrofs, w, (bShowGraph[1] ? h/2 : h),
				  SWP_NOZORDER | SWP_NOCOPYBITS | (bShowGraph[0] ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
	SetWindowPos (GetDlgItem (hDlg, IDC_TIMESTEP), 0,
		          0, hdrofs + (bShowGraph[0] ? h/2 : 0), w, (bShowGraph[0] ? h - h/2 : h),
				  SWP_NOZORDER | SWP_NOCOPYBITS | (bShowGraph[1] ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
}

// =================================================================================
// FlightData dialog message handler
// =================================================================================

INT_PTR CALLBACK MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int i;

	switch (uMsg) {
	case WM_INITDIALOG: {          // initialise dialog
		g_T      = oapiGetSysTime();
		g_simT   = oapiGetSimTime();
		g_fcount = 0;
		g_Graph[0] = new Graph(1);
		g_Graph[0]->SetYLabel ("FPS");
		SetWindowLongPtr (GetDlgItem (hDlg, IDC_FRAMERATE), 0, 0);
		g_Graph[1] = new Graph(1);
		g_Graph[1]->SetYLabel ("dt");
		SetWindowLongPtr (GetDlgItem (hDlg, IDC_TIMESTEP), 0, 1);
		bDisplay = true;
		SendDlgItemMessage (hDlg, IDC_SHOW_FRAMERATE, BM_SETCHECK, bShowGraph[0] ? BST_CHECKED : BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_SHOW_TIMESTEP,  BM_SETCHECK, bShowGraph[1] ? BST_CHECKED : BST_UNCHECKED, 0);
		ArrangeGraphs (hDlg);
		} return TRUE;
	case WM_DESTROY:               // destroy dialog box
		for (i = 0; i < 2; i++) {
			delete g_Graph[i];
			g_Graph[i] = 0;
		}
		bDisplay = false;
		return 0;
	case WM_SIZE:                  // dialog box resized
		ArrangeGraphs (hDlg);
		return 0;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:             // close dialog
			oapiCloseDialog (g_hDlg);
			return TRUE;
		case IDC_SHOW_FRAMERATE: // show/hide frame rate graph
			if (HIWORD (wParam) == BN_CLICKED) {
				bShowGraph[0] = !bShowGraph[0];
				ArrangeGraphs (hDlg);
				SendDlgItemMessage (hDlg, IDC_SHOW_FRAMERATE, BM_SETCHECK, bShowGraph[0] ? BST_CHECKED : BST_UNCHECKED, 0);
			}
			return 0;
		case IDC_SHOW_TIMESTEP:  // show/hide time step graph
			if (HIWORD (wParam) == BN_CLICKED) {
				bShowGraph[1] = !bShowGraph[1];
				ArrangeGraphs (hDlg);
				SendDlgItemMessage (hDlg, IDC_SHOW_TIMESTEP,  BM_SETCHECK, bShowGraph[1] ? BST_CHECKED : BST_UNCHECKED, 0);
			}
			return 0;
		}
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
}

// =================================================================================
// Graph canvas message handler
// =================================================================================

LONG_PTR FAR PASCAL Graph_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT: {
		RECT r;
		PAINTSTRUCT ps;
		HDC hDC;
		int gw, gh;
		GetClientRect (hWnd, &r);
		gw = r.right-r.left;
		gh = r.bottom-r.top;
		hDC = BeginPaint (hWnd, &ps);
		SetViewportOrgEx (hDC, 0, 0, NULL);
		int idx = GetWindowLongPtr (hWnd, 0);
		if (bShowGraph[idx]) g_Graph[idx]->Refresh (hDC, gw, gh);
		EndPaint (hWnd, &ps);
		} break;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

