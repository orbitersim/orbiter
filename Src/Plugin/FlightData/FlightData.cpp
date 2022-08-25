// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//
// FlightData.cpp
// Dialog box for displaying vessel flight data during the
// simulation.
// ==============================================================

#define ORBITER_MODULE
#include <windows.h>
#include <list>
#include "orbitersdk.h"
#include "FDGraph.h"
#include "resource.h"

// ==============================================================
// The module interface class - singleton implementation

namespace oapi {

	/// \brief Plugin for graphically displaying vessel flight parameters
	///    and avionics data
	class FlightData : public Module {
	public:
		/// \brief Soliton instance server for FlightData plugin
		/// \param hDLL module instance handle
		static FlightData* GetInstance(HINSTANCE hDLL);

		/// \brief Soliton instance destructor
		static void DelInstance();

		/// \brief Entry point for open dialog callback
		static void hookOpenDlg(void* context);

		/// \brief Open dialog callback
		void clbkOpenDlg(void* context);

		/// \brief Time step notification callback
		void clbkPreStep(double simt, double simdt, double mjd);

		/// \brief Vessel creation notification callback
		/// \param hVessel Handle of new vessel
		void clbkNewVessel(OBJHANDLE hVessel);

		/// \brief Vessel deletion notification callback
		/// \param hVessel Handle of vessel to be deleted 
		void clbkDeleteVessel(OBJHANDLE hVessel);

		/// \brief Entry point for dialog message procedure
		static INT_PTR CALLBACK hookDlgMsgProc(HWND hDlg, UINT uInt, WPARAM wParam, LPARAM lParam);

		/// \brief Dialog message procedure
		INT_PTR DlgMsgProc(HWND hDlg, UINT uInt, WPARAM wParam, LPARAM lParam);

		/// \brief Entry point for graph message procedure
		static LONG_PTR FAR PASCAL hookGraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

		/// \brief Graph message procedure
		LONG_PTR FAR PASCAL GraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	protected:
		/// \brief Protected constructor
		FlightData(HINSTANCE hDLL);

		/// \brief Protected destructor
		~FlightData();

		/// \brief Init dialog message handler
		INT_PTR InitDialog(HWND hDlg);

		/// \brief Destroy dialog message handler
		INT_PTR DestroyDialog();

		/// \brief Re-initialise the vessel selection list 
		/// \param hDlg dialog handle
		void ResetVesselList(HWND hDlg);

		/// \brief Reset all data graphs
		void ResetData();

		/// \brief Switch monitoring focus to a new vessel 
		void PickVesselFromList(HWND hDlg);

		/// \brief Add a new data graph to the stack
		/// \param hDlg dialog handle
		/// \param which data type id
		void AddGraph(HWND hDlg, int which);

		/// \brief Deleta a data graph from the stack
		/// \param hDlg dialog handle
		/// \param which data type id
		void DelGraph(HWND hDlg, int which);

		/// \brief Refresh the currently displayed data graphs
		/// \param hWnd Graph window handle
		/// \return 0
		LONG_PTR FAR PASCAL PaintGraph(HWND hWnd);

		/// \brief Initialise or finalise the data log file
		/// \param start If true, initialise file, otherwise finalise file
		void WriteLogHeader(bool start);

	private:
		static FlightData* self; ///> Soliton instance pointer
		DWORD m_dwCmd;           ///> Handle for plugin entry in custom command list
		HWND m_hDlg;             ///> Dialog window handle
		std::list<FlightDataGraph*> m_graph;  ///> List of displayed graphs

		VESSEL* m_pVessel;       ///> current focus vessel
		double m_sysT;           ///> current system time
		double m_DT;             ///> sample interval
		bool m_bRecording;       ///> Recording active
		bool m_bLogging;         ///> Logging active
		bool m_bResetLog;        ///> Reset log file on next output
		std::string m_sLogfile;  ///> Log file name
	};

} // namespace oapi


// ==============================================================
// API interface
// ==============================================================

/// \brief Module entry point 
/// \param hDLL module handle
DLLCLBK void InitModule(HINSTANCE hDLL)
{
	// Create and register the module
	oapiRegisterModule(oapi::FlightData::GetInstance(hDLL));
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	// Delete the module
	oapi::FlightData::DelInstance();
}


// ==============================================================
// FlightData module interface class
// ==============================================================

oapi::FlightData* oapi::FlightData::self = nullptr;

// --------------------------------------------------------------

oapi::FlightData* oapi::FlightData::GetInstance(HINSTANCE hDLL)
{
	if (!self)
		self = new FlightData(hDLL);
	return self;
}

// --------------------------------------------------------------

void oapi::FlightData::DelInstance()
{
	if (self) {
		delete self;
		self = nullptr;
	}
}

// --------------------------------------------------------------

oapi::FlightData::FlightData(HINSTANCE hDLL)
	: Module(hDLL)
	, m_hDlg(NULL)
{
	// Register the window class for the data graph
	WNDCLASS wndClass;
	wndClass.style = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc = hookGraphMsgProc;
	wndClass.cbClsExtra = 0;
	wndClass.cbWndExtra = 0;
	wndClass.hInstance = hDLL;
	wndClass.hIcon = NULL;
	wndClass.hCursor = LoadCursor(NULL, IDC_CROSS);
	wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
	wndClass.lpszMenuName = NULL;
	wndClass.lpszClassName = "GraphWindow";
	RegisterClass(&wndClass);

	Graph::InitGDI();

	// Register the custom command for the plugin
	static char* desc = "Open a window to track flight parameters of a spacecraft.";
	m_dwCmd = oapiRegisterCustomCmd("Flight Data Monitor", desc, hookOpenDlg, NULL);

	m_pVessel = nullptr;
	m_sysT = 0.0;
	m_DT = 0.0;
	m_bRecording = false;
	m_bLogging = false;
	m_bResetLog = true;
	m_sLogfile = "FlightData.log";
}

// --------------------------------------------------------------

oapi::FlightData::~FlightData()
{
	// Unregister the window class for the graph
	UnregisterClass("GraphWindow", GetModule());

	Graph::FreeGDI();

	// Unregister the custom command for calling the plugin
	oapiUnregisterCustomCmd(m_dwCmd);
}

// --------------------------------------------------------------
// Per-frame update

void oapi::FlightData::clbkPreStep(double simt, double simdt, double mjd)
{
	if (!m_hDlg) return; // dialog window not open
	if (!m_bRecording) return; // recorder turned off
	if (!m_pVessel) return; // no focus vessel

	double syst = oapiGetSysTime(); // ignore time acceleration for graph updates

	if (syst >= m_sysT + m_DT) {
		FILE* f = 0;
		if (m_bLogging) {
			f = fopen(m_sLogfile.c_str(), "at");
			fprintf(f, "%10.2f", simt);
		}
		for (auto it = m_graph.begin(); it != m_graph.end(); it++)
			(*it)->AppendDataPoint(m_pVessel, f);
		if (f) {
			fprintf(f, "\n");
			fclose(f);
		}
		m_sysT = syst;
		InvalidateRect(GetDlgItem(m_hDlg, IDC_GRAPH), NULL, TRUE);
	}
}

// --------------------------------------------------------------
// vessel creation notification

void oapi::FlightData::clbkNewVessel(OBJHANDLE hVessel)
{
	if (!m_hDlg) return; // dialog window not open

	ResetVesselList(m_hDlg);
}

// --------------------------------------------------------------
// vessel deletion notification

void oapi::FlightData::clbkDeleteVessel(OBJHANDLE hVessel)
{
	if (!m_hDlg) return; // dialog window not open

	VESSEL* v = oapiGetVesselInterface(hVessel);
	if (v == m_pVessel) {
		m_pVessel = oapiGetFocusInterface();
		ResetData();
	}
	ResetVesselList(m_hDlg);
}

// --------------------------------------------------------------
// Set up the dialog window

INT_PTR oapi::FlightData::InitDialog(HWND hDlg)
{
	const std::vector<std::string> ratestr = { "0.01", "0.1", "1", "10" };

	// Populate the data type list from the known graph types
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_Altitude::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_Airspeed::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_VSpeed::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_MachNumber::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_Temperature::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_Pressure::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_AOA::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_LiftDrag::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_LDRatio::Title().c_str());
	SendDlgItemMessage(hDlg, IDC_DATALIST, LB_ADDSTRING, 0, (LPARAM)FDGraph_Mass::Title().c_str());

	// Populate the update rate list
	for (int i = 0; i < ratestr.size(); i++)
		SendDlgItemMessage(hDlg, IDC_RATE, CB_ADDSTRING, 0, (LPARAM)ratestr[i].c_str());
	SendDlgItemMessage(hDlg, IDC_RATE, CB_SETCURSEL, 2, 0);

	m_pVessel = oapiGetFocusInterface();
	ResetVesselList(hDlg);
	m_DT = 1.0;
	m_sysT = 0.0;
	m_bRecording = m_bLogging = false;

	return TRUE;
}

// --------------------------------------------------------------

INT_PTR oapi::FlightData::DestroyDialog()
{
	// delete all data graphs
	for (auto it = m_graph.begin(); it != m_graph.end(); it++)
		delete (*it);
	m_graph.clear();

	if (m_bRecording && m_bLogging) WriteLogHeader(false);
	m_hDlg = NULL;
	return 0;
}

// --------------------------------------------------------------

void oapi::FlightData::ResetVesselList(HWND hDlg)
{
	SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_RESETCONTENT, 0, 0);
	DWORD i, n = oapiGetVesselCount();
	if (!n) return; // this should not happen
	char name[256];
	for (i = 0; i < n; i++) {
		oapiGetObjectName(oapiGetVesselByIndex(i), name, 256);
		SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_ADDSTRING, 0, (LPARAM)name);
	}
	i = SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_FINDSTRINGEXACT, -1, (LPARAM)m_pVessel->GetName());
	SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_SETCURSEL, (i != CB_ERR ? i : 0), 0);
}

// --------------------------------------------------------------

void oapi::FlightData::ResetData()
{
	for (auto it = m_graph.begin(); it != m_graph.end(); it++)
		(*it)->ResetData();
}

// --------------------------------------------------------------

void oapi::FlightData::PickVesselFromList(HWND hDlg)
{
	int item = SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_GETCURSEL, 0, 0);
	if (item == CB_ERR) return;
	char cbuf[256];
	SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_GETLBTEXT, item, (LPARAM)cbuf);
	OBJHANDLE hv = oapiGetVesselByName(cbuf);
	if (hv) {
		if (m_bRecording && m_bLogging) WriteLogHeader(false);
		m_pVessel = oapiGetVesselInterface(hv);
		for (auto it = m_graph.begin(); it != m_graph.end(); it++)
			(*it)->ResetData();
		if (m_bRecording && m_bLogging) WriteLogHeader(true);
	}
}

// --------------------------------------------------------------

void oapi::FlightData::AddGraph(HWND hDlg, int which)
{
	RECT rw, rc;
	int dh;
	DWORD i;
	size_t ng = m_graph.size();
	char cbuf[256];

	if (SendDlgItemMessage(hDlg, IDC_DATALIST, LB_GETTEXT, which, (LPARAM)cbuf) == LB_ERR)
		return;
	for (auto it = m_graph.begin(); it != m_graph.end(); it++)
		if ((*it)->Title() == cbuf) return; // data graph already present

	// stretch dialog window to accomodate the new graph
	HWND hCanvas = GetDlgItem(hDlg, IDC_GRAPH);
	GetWindowRect(hDlg, &rw);
	GetWindowRect(hCanvas, &rc);
	dh = (ng ? (rc.bottom - rc.top) / ng : (rc.right - rc.left) / 2);

	SetWindowPos(hDlg, 0, 0, 0, rw.right - rw.left, rw.bottom - rw.top + dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);
	SetWindowPos(hCanvas, 0, 0, 0, rc.right - rc.left, rc.bottom - rc.top + dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);

	// add new graph to the list
	m_graph.push_back(FlightDataGraph::CreateGraph(cbuf));
}

// --------------------------------------------------------------

void oapi::FlightData::DelGraph(HWND hDlg, int which)
{
	RECT rw, rc;
	DWORD i, j;
	int dh;
	size_t ng = m_graph.size();
	char cbuf[256];

	if (SendDlgItemMessage(hDlg, IDC_DATALIST, LB_GETTEXT, which, (LPARAM)cbuf) == LB_ERR)
		return;
	for (auto it = m_graph.begin(); it != m_graph.end(); it++)
		if ((*it)->Title() == cbuf) {

			// shrink dialog window
			HWND hCanvas = GetDlgItem(hDlg, IDC_GRAPH);
			GetWindowRect(hDlg, &rw);
			GetWindowRect(hCanvas, &rc);
			dh = (rc.bottom - rc.top) / ng;

			SetWindowPos(hDlg, 0, 0, 0, rw.right - rw.left, rw.bottom - rw.top - dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);
			SetWindowPos(hCanvas, 0, 0, 0, rc.right - rc.left, rc.bottom - rc.top - dh, SWP_NOMOVE | SWP_NOREPOSITION | SWP_NOZORDER);

			// remove graph from the list
			delete (*it);
			m_graph.erase(it);

			break;
		}
}

// --------------------------------------------------------------

LONG_PTR FAR PASCAL oapi::FlightData::PaintGraph(HWND hWnd)
{
	size_t ng = m_graph.size();
	if (ng) {
		RECT r;
		PAINTSTRUCT ps;
		HDC hDC;
		DWORD i;
		GetClientRect(hWnd, &r);
		int gw = r.right - r.left;
		int gh = (r.bottom - r.top) / ng;
		int gtop = 0;
		hDC = BeginPaint(hWnd, &ps);
		for (auto it = m_graph.begin(); it != m_graph.end(); it++) {
			SetViewportOrgEx(hDC, 0, gtop, NULL);
			(*it)->Refresh(hDC, gw, gh);
			gtop += gh;
		}
		EndPaint(hWnd, &ps);
	}

	return 0;
}

// --------------------------------------------------------------

void oapi::FlightData::WriteLogHeader(bool start)
{
	FILE* f = fopen(m_sLogfile.c_str(), m_bResetLog ? "wt" : "at");
	if (m_bResetLog) { // write out header
		fprintf(f, "Orbiter Flight Data Log Record\n");
		fprintf(f, "==============================\n");
		fprintf(f, "Columns:\n");
		fprintf(f, "\tTIME:     simulation time (seconds)\n");
		fprintf(f, "\tALT:      altitude (km)\n");
		fprintf(f, "\tAIRSPEED: airspeed (m/s)\n");
		fprintf(f, "\tVSPEED:   vertical speed (m/s)\n");
		fprintf(f, "\tMACH:     Mach number\n");
		fprintf(f, "\tTEMP:     freestream temperature (K)\n");
		fprintf(f, "\tSTP:      static pressure (kPa)\n");
		fprintf(f, "\tDNP:      dynamic pressure (kPa)\n");
		fprintf(f, "\tAOA:      angle of attack (deg)\n");
		fprintf(f, "\tSLIP:     horizontal slip angle (deg)\n");
		fprintf(f, "\tLIFT:     total lift force (kN)\n");
		fprintf(f, "\tDRAG:     total drag force (kN)\n");
		fprintf(f, "\tL/D:      lift/drag ratio\n");
		fprintf(f, "\tTOTMASS:  total vessel mass (kg)\n");
		fprintf(f, "\tPRPMASS:  propellant mass (kg)\n\n");
		m_bResetLog = false;
	}
	if (start) {
		fprintf(f, "# Log started for %s\n", m_pVessel->GetName());
		fprintf(f, "# ____TIME");
		for (auto it = m_graph.begin(); it != m_graph.end(); it++)
			(*it)->WriteHeader(f);
		fprintf(f, "\n");
	}
	else {
		fprintf(f, "# Log stopped for %s\n", m_pVessel->GetName());
	}
	fclose(f);
}

// --------------------------------------------------------------

void oapi::FlightData::hookOpenDlg(void* context)
{
	self->clbkOpenDlg(context);
}

// --------------------------------------------------------------

void oapi::FlightData::clbkOpenDlg(void* context)
{
	HWND hDlg = oapiOpenDialog(GetModule(), IDD_FLIGHTDATA, hookDlgMsgProc);
	if (hDlg) m_hDlg = hDlg; // otherwise open already
}

// --------------------------------------------------------------

INT_PTR CALLBACK oapi::FlightData::hookDlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return self->DlgMsgProc(hDlg, uMsg, wParam, lParam);
}

// --------------------------------------------------------------
// FlightData dialog message handler

INT_PTR oapi::FlightData::DlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return InitDialog(hDlg);
	case WM_DESTROY:
		return DestroyDialog();
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDCANCEL:
			oapiCloseDialog(m_hDlg);
			return TRUE;
		case IDC_VESSELLIST:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				PickVesselFromList(hDlg);
				return TRUE;
			}
			break;
		case IDC_RATE:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				int i, item = SendDlgItemMessage(hDlg, IDC_RATE, CB_GETCURSEL, 0, 0);
				for (i = 0, m_DT = 100.0; i < item; i++) m_DT *= 0.1;
				return TRUE;
			}
			break;
		case IDC_DATALIST:
			if (HIWORD(wParam) == LBN_SELCHANGE) {
				int n = SendDlgItemMessage(hDlg, IDC_DATALIST, LB_GETCOUNT, 0, 0);
				for (DWORD i = 0; i < n; i++) {
					if (SendDlgItemMessage(hDlg, IDC_DATALIST, LB_GETSEL, i, 0))
						AddGraph(hDlg, i);
					else
						DelGraph(hDlg, i);
				}
				if (m_bRecording && m_bLogging) {
					WriteLogHeader(false);
					WriteLogHeader(true);
				}
			}
			break;
		case IDC_LOG:
			m_bLogging = (SendDlgItemMessage(hDlg, IDC_LOG, BM_GETCHECK, 0, 0) == BST_CHECKED);
			if (m_bRecording) WriteLogHeader(m_bLogging);
			return TRUE;
		case IDC_STARTSTOP:
			m_bRecording = !m_bRecording;
			SetWindowText(GetDlgItem(hDlg, IDC_STARTSTOP), m_bRecording ? "Stop" : "Start");
			if (m_bLogging) WriteLogHeader(m_bRecording);
			return TRUE;
		case IDC_RESET:
			ResetData();
			return TRUE;
		}
		break;
	}
	return oapiDefDialogProc(hDlg, uMsg, wParam, lParam);
}

// --------------------------------------------------------------

LONG_PTR FAR PASCAL oapi::FlightData::hookGraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return self->GraphMsgProc(hWnd, uMsg, wParam, lParam);
}

// --------------------------------------------------------------
// Graph canvas message handler

LONG_PTR FAR PASCAL oapi::FlightData::GraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT:
		return PaintGraph(hWnd);
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}
