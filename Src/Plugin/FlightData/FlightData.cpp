// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//          Copyright (C) 2003-2016 Martin Schweiger
//                   All rights reserved
//
// FlightData.cpp
// Dialog box for displaying vessel flight data during the
// simulation.
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include <stdio.h>
#include <windows.h>
#include "orbitersdk.h"
#include "resource.h"
#include "FDGraph.h"

#define NGRAPH 9
#define NRATE 4

// ==============================================================
// Global variables

HINSTANCE g_hInst;          // module instance handle
HWND g_hDlg;                // dialog handle
DWORD g_dwCmd;              // custom function identifier
DWORD g_nGraph = 0;         // number of displayed graphs
FlightDataGraph **g_Graph;  // list of graphs currently displayed
VESSEL *g_VESSEL = 0;       // current reference vessel
double g_T = 0.0;           // sample time
double g_DT;                // sample interval
bool g_bRecording;          // recorder on/off
bool g_bLogging;            // log to file on/off
static bool g_bResetLog = true;

static char *desc = "Open a window to track flight parameters of a spacecraft.";
static char *logfile = "FlightData.log";

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
	g_dwCmd = oapiRegisterCustomCmd ("Flight Data Monitor", desc, OpenDlgClbk, NULL);
	// adds a new command into Orbiter's custom function list

	// register the window class for the data graph
	WNDCLASS wndClass;
	wndClass.style       = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc = Graph_WndProc;
	wndClass.cbClsExtra    = 0;
	wndClass.cbWndExtra    = 0;
	wndClass.hInstance     = hDLL;
	wndClass.hIcon         = NULL;
	wndClass.hCursor       = LoadCursor (NULL, IDC_CROSS);
	wndClass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH);
	wndClass.lpszMenuName  = NULL;
	wndClass.lpszClassName = "GraphWindow";
	RegisterClass (&wndClass);

	Graph::InitGDI();
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	UnregisterClass ("GraphWindow", g_hInst);
	oapiUnregisterCustomCmd (g_dwCmd);

	Graph::FreeGDI();
}

DLLCLBK void opcPreStep (double simt, double simdt, double mjd)
{
	if (!g_hDlg) return; // flight data dialog not open
	if (!g_bRecording) return; // recorder turned off

	double syst = oapiGetSysTime(); // ignore time acceleration for graph updates

	if (syst >= g_T+g_DT) {

		FILE *f = 0;
		if (g_bLogging) {
			f = fopen (logfile, "at");
			fprintf (f, "%10.2f", simt);
		}

		for (DWORD i = 0; i < g_nGraph; i++)
			g_Graph[i]->AppendDataPoint (f);

		if (f) {
			fprintf (f, "\n");
			fclose (f);
		}

		g_T = syst;
		InvalidateRect (GetDlgItem (g_hDlg, IDC_GRAPH), NULL, TRUE);
	}
}

void OpenDlgClbk (void *context)
{
	HWND hDlg = oapiOpenDialog (g_hInst, IDD_FLIGHTDATA, MsgProc);
	if (hDlg) g_hDlg = hDlg; // otherwise open already
}

void ResetVesselList (HWND hDlg)
{
	SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_RESETCONTENT, 0, 0);
	DWORD i, n = oapiGetVesselCount();
	if (!n) return; // this should not happen
	char name[256];
	for (i = 0; i < n; i++) {
		oapiGetObjectName (oapiGetVesselByIndex (i), name, 256);
		SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_ADDSTRING, 0, (LPARAM)name);
	}
	g_VESSEL = oapiGetVesselInterface (oapiGetFocusObject());
	i = SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_FINDSTRINGEXACT, -1, (LPARAM)g_VESSEL->GetName());
	SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_SETCURSEL, (i != CB_ERR ? i:0), 0);

}

void ResetVessel (VESSEL *vessel)
{
	g_VESSEL = vessel;
	for (DWORD i = 0; i < g_nGraph; i++)
		g_Graph[i]->ResetData();
}

void AddGraph (HWND hDlg, int which)
{
	RECT rw, rc;
	int dh;
	DWORD i;
	char cbuf[256];

	for (i = 0; i < g_nGraph; i++)
		if (g_Graph[i]->DType() == which) return; // already present

	// stretch dialog window to accomodate the new graph
	HWND hCanvas = GetDlgItem (hDlg, IDC_GRAPH);
	GetWindowRect (hDlg, &rw);
	GetWindowRect (hCanvas, &rc);
	dh = (g_nGraph ? (rc.bottom-rc.top)/g_nGraph : (rc.right-rc.left)/2);

	SetWindowPos (hDlg, 0, 0, 0, rw.right-rw.left, rw.bottom-rw.top+dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);
	SetWindowPos (hCanvas, 0, 0, 0, rc.right-rc.left, rc.bottom-rc.top+dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);

	// add new graph to the list
	FlightDataGraph **tmp = new FlightDataGraph*[g_nGraph+1];
	if (g_nGraph) {
		memcpy (tmp, g_Graph, g_nGraph*sizeof(FlightDataGraph*));
		delete []g_Graph;
	}
	g_Graph = tmp;
	switch (which) {
	case 4:
	case 5:
	case 6:
		g_Graph[g_nGraph] = new FlightDataGraph (which, 2); break;
	default:
		g_Graph[g_nGraph] = new FlightDataGraph (which); break;
	}
	LoadString (g_hInst, IDS_DATA+which, cbuf, 256);
	g_Graph[g_nGraph]->SetTitle (cbuf);
	LoadString (g_hInst, IDS_YLABEL+which, cbuf, 256);
	g_Graph[g_nGraph]->SetYLabel (cbuf);
	LoadString (g_hInst, IDS_LEGEND+which, cbuf, 256);
	g_Graph[g_nGraph]->SetLegend (cbuf);
	g_nGraph++;
}

void DelGraph (HWND hDlg, int which)
{
	RECT rw, rc;
	DWORD i, j;
	int dh;

	for (i = 0; i < g_nGraph; i++)
		if (g_Graph[i]->DType() == which) break;
	if (i == g_nGraph) return; // not found

	// shrink dialog window
	HWND hCanvas = GetDlgItem (hDlg, IDC_GRAPH);
	GetWindowRect (hDlg, &rw);
	GetWindowRect (hCanvas, &rc);
	dh = (rc.bottom-rc.top)/g_nGraph;

	SetWindowPos (hDlg, 0, 0, 0, rw.right-rw.left, rw.bottom-rw.top-dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);
	SetWindowPos (hCanvas, 0, 0, 0, rc.right-rc.left, rc.bottom-rc.top-dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);

	// remove graph from the list
	FlightDataGraph **tmp = (g_nGraph > 1 ? new FlightDataGraph*[g_nGraph-1] : 0);
	if (tmp) {
		for (i = j = 0; i < g_nGraph; i++)
			if (g_Graph[i]->DType() == which) delete g_Graph[i];
			else tmp[j++] = g_Graph[i];
		delete []g_Graph;
	}
	g_Graph = tmp;
	g_nGraph--;
}

void WriteLogHeader (bool start)
{
	FILE *f = fopen (logfile, g_bResetLog ? "wt":"at");
	if (g_bResetLog) {
		fprintf (f, "Orbiter Flight Data Log Record\n");
		fprintf (f, "==============================\n");
		fprintf (f, "Columns:\n");
		fprintf (f, "\tTIME:     simulation time (seconds)\n");
		fprintf (f, "\tALT:      altitude (km)\n");
		fprintf (f, "\tAIRSPEED: airspeed (m/s)\n");
		fprintf (f, "\tMACH:     Mach number\n");
		fprintf (f, "\tTEMP:     freestream temperature (K)\n");
		fprintf (f, "\tSTP:      static pressure (kPa)\n");
		fprintf (f, "\tDNP:      dynamic pressure (kPa)\n");
		fprintf (f, "\tAOA:      angle of attack (deg)\n");
		fprintf (f, "\tSLIP:     horizontal slip angle (deg)\n");
		fprintf (f, "\tLIFT:     total lift force (kN)\n");
		fprintf (f, "\tDRAG:     total drag force (kN)\n");
		fprintf (f, "\tL/D:      lift/drag ratio\n");
		fprintf (f, "\tMASS:     vessel mass (kg)\n\n");
		g_bResetLog = false;
	}
	if (start) {
		fprintf (f, "# Log started for %s\n", g_VESSEL->GetName());
		fprintf (f, "# ____TIME");
		for (DWORD i = 0; i < g_nGraph; i++)
			g_Graph[i]->WriteHeader (f);
		fprintf (f, "\n");
	} else {
		fprintf (f, "# Log stopped for %s\n", g_VESSEL->GetName());
	}
	fclose (f);
}

// =================================================================================
// FlightData dialog message handler
// =================================================================================

INT_PTR CALLBACK MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG: {
		char cbuf[256];
		int i;
		for (i = 0; i < NGRAPH; i++) {
			LoadString (g_hInst, IDS_DATA+i, cbuf, 256);
			SendDlgItemMessage (hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)cbuf);
		}
		for (i = 0; i < NRATE; i++) {
			static char *ratestr[NRATE] = {"0.01","0.1","1","10"};
			SendDlgItemMessage (hDlg, IDC_RATE, CB_ADDSTRING, 0, (LPARAM)ratestr[i]);
		}
		SendDlgItemMessage (hDlg, IDC_RATE, CB_SETCURSEL, 2, 0);
		g_DT = 1.0;
		g_T  = 0.0;
		ResetVesselList (hDlg);
		g_bRecording = g_bLogging = false;
		} return TRUE;
	case WM_DESTROY:
		if (g_bRecording && g_bLogging) WriteLogHeader (false);
		if (g_nGraph) {
			for (DWORD i = 0; i < g_nGraph; i++) delete g_Graph[i];
			delete []g_Graph;
			g_nGraph = 0;
		}
		g_hDlg = 0;
		return 0;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			oapiCloseDialog (g_hDlg);
			return TRUE;
		case IDC_VESSELLIST:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				int item = SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_GETCURSEL, 0, 0);
				if (item == CB_ERR) return TRUE;
				char cbuf[256];
				SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_GETLBTEXT, item, (LPARAM)cbuf);
				OBJHANDLE hv = oapiGetVesselByName (cbuf);
				if (hv) {
					if (g_bRecording && g_bLogging) WriteLogHeader (false);
					ResetVessel (oapiGetVesselInterface(hv));
					if (g_bRecording && g_bLogging) WriteLogHeader (true);
				}
				return TRUE;
			}
			break;
		case IDC_RATE:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				int i, item = SendDlgItemMessage (hDlg, IDC_RATE, CB_GETCURSEL, 0, 0);
				for (i = 0, g_DT = 100.0; i < item; i++) g_DT *= 0.1;
				return TRUE;
			}
			break;
		case IDC_DATALIST:
			if (HIWORD (wParam) == LBN_SELCHANGE) {
				for (DWORD i = 0; i < NGRAPH; i++) {
					if (SendDlgItemMessage (hDlg, IDC_DATALIST, LB_GETSEL, i, 0))
						AddGraph (hDlg, i);
					else
						DelGraph (hDlg, i);
				}
				if (g_bRecording && g_bLogging) {
					WriteLogHeader (false);
					WriteLogHeader (true);
				}
			}
			break;
		case IDC_LOG:
			g_bLogging = (SendDlgItemMessage (hDlg, IDC_LOG, BM_GETCHECK, 0, 0) == BST_CHECKED);
			if (g_bRecording) WriteLogHeader (g_bLogging);
			return TRUE;
		case IDC_STARTSTOP:
			g_bRecording = !g_bRecording;
			SetWindowText (GetDlgItem (hDlg, IDC_STARTSTOP), g_bRecording ? "Stop":"Start");
			if (g_bLogging) WriteLogHeader (g_bRecording);
			return TRUE;
		case IDC_RESET:
			for (DWORD i = 0; i < g_nGraph; i++)
				g_Graph[i]->ResetData();
			return TRUE;
		}
		break;
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
}

// =================================================================================
// Graph canvas message handler
// =================================================================================

LONG_PTR FAR PASCAL Graph_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT:
		if (g_nGraph) {
			RECT r;
			PAINTSTRUCT ps;
			HDC hDC;
			DWORD i;
			int gw, gh;
			GetClientRect (hWnd, &r);
			gw = r.right-r.left;
			gh = (r.bottom-r.top)/g_nGraph;
			hDC = BeginPaint (hWnd, &ps);
			for (i = 0; i < g_nGraph; i++) {
				SetViewportOrgEx (hDC, 0, i*gh, NULL);
				g_Graph[i]->Refresh (hDC, gw, gh);
			}
			EndPaint (hWnd, &ps);
		}
		break;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

