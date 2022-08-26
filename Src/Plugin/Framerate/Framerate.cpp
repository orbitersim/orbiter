// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Framerate
//                  Part of the ORBITER SDK
//
// Framerate.cpp
// Dialog box for displaying simulation frame rate.
// ==============================================================

#define ORBITER_MODULE
#include <windows.h>
#include "Orbitersdk.h"
#include "Dialog\Graph.h"
#include "resource.h"

// ==============================================================
// The module interface class - singleton implementation

namespace oapi {

	/// \brief Plugin for graphically displaying frame rate and time step length.
	class Framerate : public Module {
	public:
		/// \brief Soliton instance server for Framerate plugin
		/// \param hDLL nodule instance handle
		static Framerate* GetInstance(HINSTANCE hDLL);

		/// \brief Soliton instance destructor
		static void DelInstance();

		/// \brief Entry point for open dialog callback
		static void hookOpenDlg(void* context);

		/// \brief Open dialog callback
		void clbkOpenDlg(void* context);

		/// \brief Time step notification callback
		void clbkPreStep(double simt, double simdt, double mjd);

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
		Framerate(HINSTANCE hDLL);

		/// \brief Protected destructor
		~Framerate();

		/// \brief Init dialog message handler
		INT_PTR InitDialog(HWND hDlg);

		/// \brief Destroy dialog message handler
		INT_PTR DestroyDialog();

		/// \brief Toggle graph view message handler
		INT_PTR ToggleGraph(int which);

		/// \brief Arrange graphs after user choice or resize
		void ArrangeGraphs(HWND hDlg);

		/// \brief Graph repaint message handler
		LONG_PTR FAR PASCAL PaintGraph(HWND hWnd);

	private:
		static Framerate* self;  ///> Soliton instance pointer
		DWORD m_dwCmd;           ///> Handle for plugin entry in custom command list
		HWND m_hDlg;             ///> Dialog window handle
		Graph* m_graph[2];       ///> graph instances for frame rate and time step lengths
		bool m_showGraph[2];     ///> graph visible?

		double m_sysT;           ///> current system time
		double m_simT;           ///> current simulation time
		double m_DT;             ///> sample interval
		DWORD m_fcount;          ///> frame counter
	};

} // namespace oapi


// ==============================================================
// API interface
// ==============================================================

/// \brief Module entry point 
/// \param hDLL module handle
DLLCLBK void InitModule (HINSTANCE hDLL)
{
	// Create and register the module
	oapiRegisterModule(oapi::Framerate::GetInstance(hDLL));
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Delete the module
	oapi::Framerate::DelInstance();
}


// ==============================================================
// Framerate module interface class
// ==============================================================

oapi::Framerate* oapi::Framerate::self = nullptr;

// --------------------------------------------------------------

oapi::Framerate* oapi::Framerate::GetInstance(HINSTANCE hDLL)
{
	if (!self)
		self = new Framerate(hDLL);
	return self;
}

// --------------------------------------------------------------

void oapi::Framerate::DelInstance()
{
	if (self) {
		delete self;
		self = nullptr;
	}
}

// --------------------------------------------------------------

oapi::Framerate::Framerate(HINSTANCE hDLL)
	: Module(hDLL)
	, m_hDlg(NULL)
{
	// Register the window class for the data graph
	WNDCLASS wndClass;
	wndClass.style = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc = hookGraphMsgProc;
	wndClass.cbClsExtra = 0;
	wndClass.cbWndExtra = 4;
	wndClass.hInstance = hDLL;
	wndClass.hIcon = NULL;
	wndClass.hCursor = LoadCursor(NULL, IDC_CROSS);
	wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
	wndClass.lpszMenuName = NULL;
	wndClass.lpszClassName = "PerfGraphWindow";
	RegisterClass(&wndClass);

	for (int i = 0; i < 2; i++)
		m_graph[i] = 0;
	Graph::InitGDI();

	// Register the custom command for the plugin
	static char* desc = "Simulation frame rate / time step monitor";
	m_dwCmd = oapiRegisterCustomCmd("Performance Meter", desc, hookOpenDlg, NULL);

	m_sysT = 0.0;
	m_simT = 0.0;
	m_DT = 1.0;
	m_fcount = 0;
	m_showGraph[0] = true;
	m_showGraph[1] = false;
}

// --------------------------------------------------------------

oapi::Framerate::~Framerate()
{
	// Unregister the window class for the graph
	UnregisterClass("PerfGraphWindow", GetModule());

	Graph::FreeGDI();

	// Unregister the custom command for calling the plugin
	oapiUnregisterCustomCmd(m_dwCmd);
}

// --------------------------------------------------------------
// Per-frame update

void oapi::Framerate::clbkPreStep(double simt, double simdt, double mjd)
{
	if (!m_hDlg) return; // dialog window not open

	double syst = oapiGetSysTime(); // ignore time acceleration for graph updates
	m_fcount++;

	if (syst >= m_sysT + m_DT) {
		char cbuf[128];
		if (m_showGraph[0]) {
			float fps = (float)(m_fcount / (syst - m_sysT));
			sprintf(cbuf, "FPS: %0.0f", fps);
			m_graph[0]->AppendDataPoint(fps);
			m_graph[0]->SetTitle(cbuf);
			InvalidateRect(GetDlgItem(m_hDlg, IDC_FRAMERATE), NULL, TRUE);
		}
		if (m_showGraph[1] && m_fcount) {
			float dt = (float)(simt - m_simT) / (float)m_fcount;
			sprintf(cbuf, "dt: %0.3fs", dt);
			m_graph[1]->AppendDataPoint(dt);
			m_graph[1]->SetTitle(cbuf);
			InvalidateRect(GetDlgItem(m_hDlg, IDC_TIMESTEP), NULL, TRUE);
		}
		m_sysT = syst;
		m_simT = simt;
		m_fcount = 0;
	}
}

// --------------------------------------------------------------
// Set up the dialog window

INT_PTR oapi::Framerate::InitDialog(HWND hDlg)
{
	m_sysT = oapiGetSysTime();
	m_simT = oapiGetSimTime();
	m_fcount = 0;
	m_graph[0] = new Graph(1);
	m_graph[0]->SetYLabel("FPS");
	SetWindowLongPtr(GetDlgItem(hDlg, IDC_FRAMERATE), 0, 0);
	m_graph[1] = new Graph(1);
	m_graph[1]->SetYLabel("dt");
	SetWindowLongPtr(GetDlgItem(hDlg, IDC_TIMESTEP), 0, 1);
	SendDlgItemMessage(hDlg, IDC_SHOW_FRAMERATE, BM_SETCHECK, m_showGraph[0] ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hDlg, IDC_SHOW_TIMESTEP, BM_SETCHECK, m_showGraph[1] ? BST_CHECKED : BST_UNCHECKED, 0);
	ArrangeGraphs(hDlg);

	return TRUE;
}

// --------------------------------------------------------------
// Destroy the dialog window

INT_PTR oapi::Framerate::DestroyDialog()
{
	for (int i = 0; i < 2; i++) {
		delete m_graph[i];
		m_graph[i] = 0;
	}
	m_hDlg = NULL;
	return 0;
}

// --------------------------------------------------------------
// Toggle graph display

INT_PTR oapi::Framerate::ToggleGraph(int which)
{
	const int dlg_id[2] = { IDC_SHOW_FRAMERATE, IDC_SHOW_TIMESTEP };

	m_showGraph[which] = !m_showGraph[which];
	ArrangeGraphs(m_hDlg);
	SendDlgItemMessage(m_hDlg, dlg_id[which], BM_SETCHECK, m_showGraph[which] ? BST_CHECKED : BST_UNCHECKED, 0);
	return 0;
}

// --------------------------------------------------------------
// Arrange graphs in dialog window after resizing

void oapi::Framerate::ArrangeGraphs(HWND hDlg)
{
	static int hdrofs = 0;
	RECT r;
	GetClientRect(hDlg, &r);
	if (!hdrofs) {
		RECT r2;
		GetWindowRect(GetDlgItem(hDlg, IDC_FRAMERATE), &r2);
		hdrofs = r.bottom - (r2.bottom - r2.top);
	}
	int h = r.bottom - hdrofs;
	int w = r.right;

	SetWindowPos(GetDlgItem(hDlg, IDC_FRAMERATE), 0,
		0, hdrofs, w, (m_showGraph[1] ? h / 2 : h),
		SWP_NOZORDER | SWP_NOCOPYBITS | (m_showGraph[0] ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
	SetWindowPos(GetDlgItem(hDlg, IDC_TIMESTEP), 0,
		0, hdrofs + (m_showGraph[0] ? h / 2 : 0), w, (m_showGraph[0] ? h - h / 2 : h),
		SWP_NOZORDER | SWP_NOCOPYBITS | (m_showGraph[1] ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
}

// --------------------------------------------------------------

void oapi::Framerate::hookOpenDlg(void* context)
{
	self->clbkOpenDlg(context);
}

// --------------------------------------------------------------

void oapi::Framerate::clbkOpenDlg(void* context)
{
	HWND hDlg = oapiOpenDialog(GetModule(), IDD_FRAMERATE, hookDlgMsgProc);
	if (hDlg) m_hDlg = hDlg; // otherwise open already
}

// --------------------------------------------------------------

INT_PTR CALLBACK oapi::Framerate::hookDlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return self->DlgMsgProc(hDlg, uMsg, wParam, lParam);
}

// --------------------------------------------------------------
// Framerate dialog message handler

INT_PTR oapi::Framerate::DlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int i;

	switch (uMsg) {
	case WM_INITDIALOG:            // initialise dialog
		return InitDialog(hDlg);
	case WM_DESTROY:               // destroy dialog box
		return DestroyDialog();
	case WM_SIZE:                  // dialog box resized
		ArrangeGraphs(hDlg);
		return 0;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDCANCEL:             // close dialog
			oapiCloseDialog(m_hDlg);
			return TRUE;
		case IDC_SHOW_FRAMERATE: // show/hide frame rate graph
			if (HIWORD(wParam) == BN_CLICKED)
				return ToggleGraph(0);
			break;
		case IDC_SHOW_TIMESTEP:  // show/hide time step graph
			if (HIWORD(wParam) == BN_CLICKED)
				return ToggleGraph(1);
			break;
		}
		break;
	}
	return oapiDefDialogProc(hDlg, uMsg, wParam, lParam);
}

// ---------------------------------------------------------------------------------
// Graph canvas message handler

LONG_PTR FAR PASCAL oapi::Framerate::hookGraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return self->GraphMsgProc(hWnd, uMsg, wParam, lParam);
}

LONG_PTR FAR PASCAL oapi::Framerate::GraphMsgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT:
		return PaintGraph(hWnd);
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

LONG_PTR FAR PASCAL oapi::Framerate::PaintGraph(HWND hWnd)
{
	RECT r;
	PAINTSTRUCT ps;
	HDC hDC;
	int gw, gh;
	GetClientRect(hWnd, &r);
	gw = r.right - r.left;
	gh = r.bottom - r.top;
	hDC = BeginPaint(hWnd, &ps);
	SetViewportOrgEx(hDC, 0, 0, NULL);
	int idx = GetWindowLongPtr(hWnd, 0);
	if (m_showGraph[idx]) m_graph[idx]->Refresh(hDC, gw, gh);
	EndPaint(hWnd, &ps);

	return 0;
}
