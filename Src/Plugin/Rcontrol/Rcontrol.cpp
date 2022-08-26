// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: Rcontrol
//                  Part of the ORBITER SDK
//
// Rcontrol.cpp
//
// A small plugin which allows to manipulate spacecraft main and
// retro controls via a dialog box.
// ==============================================================

#define ORBITER_MODULE
#include "Orbitersdk.h"
#include "DlgCtrl.h"
#include "resource.h"

// ==============================================================
// The module interface class - singleton implementation

namespace oapi {

	/// \brief Plugin for controlling a spacecrafts's engines via
	///   a dialog box.
	class RControl: public Module {
	public:
		/// \brief Soliton instance server for RControl plugin
		/// \param hDLL nodule instance handle
		static RControl* GetInstance(HINSTANCE hDLL);

		/// \brief Soliton instance destructor
		static void DelInstance();

		/// \brief Entry point for open dialog callback
		static void hookOpenDlg(void* context);

		/// \brief Open dialog callback
		void clbkOpenDlg(void* context);

		/// \brief Callback for time frame start 
		/// \param simt simulation time [s]
		/// \param simdt simulation step [s]
		/// \param mjd MJD date [days]
		void clbkPreStep(double simt, double simdt, double mjd);

		/// \brief Entry point for dialog message procedure
		static INT_PTR CALLBACK hookDlgMsgProc(HWND hDlg, UINT uInt, WPARAM wParam, LPARAM lParam);

		/// \brief Dialog message procedure
		INT_PTR DlgMsgProc(HWND hDlg, UINT uInt, WPARAM wParam, LPARAM lParam);

	protected:
		/// \brief Protected constructor 
		/// \param hDLL module instance handle
		RControl(HINSTANCE hDLL);

		/// \brief Protected destructor
		~RControl();

		/// \brief Init dialog message handler
		INT_PTR InitDialog(HWND hDlg);

		/// \brief Destroy dialog message handler
		INT_PTR DestroyDialog();

		/// \brief Populate the vessel list
		/// \param hDlg dialog handle
		void CreateVesselList(HWND hDlg);

	private:
		static RControl* self; ///> Soliton instance pointer
		DWORD m_dwCmd;         ///> Handle for plugin entry in custom command list
		HWND m_hDlg;           ///> Dialog window handle

		VESSEL* m_pVessel;     ///> vessel pointer
		int m_maingauge;       ///> main throttle slider position
		int m_retrogauge;      ///> retro throttle slider position
		int m_hovergauge;      ///> hover throttle slider position
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
	oapiRegisterModule(oapi::RControl::GetInstance(hDLL));
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	// Delete the module
	oapi::RControl::DelInstance();
}


// ==============================================================
// RControl module interface class
// ==============================================================

oapi::RControl* oapi::RControl::self = nullptr;

// --------------------------------------------------------------

oapi::RControl* oapi::RControl::GetInstance(HINSTANCE hDLL)
{
	if (!self)
		self = new RControl(hDLL);
	return self;
}

// --------------------------------------------------------------

void oapi::RControl::DelInstance()
{
	if (self) {
		delete self;
		self = nullptr;
	}
}

// --------------------------------------------------------------

oapi::RControl::RControl(HINSTANCE hDLL)
	: Module(hDLL)
	, m_hDlg(NULL)
{
	// Register the custom command for the plugin
	m_dwCmd = oapiRegisterCustomCmd("Remote Vessel Control",
		"Operate the engines of any spacecraft from a dialog box.",
		hookOpenDlg, NULL);

	// Register custom dialog controls
	oapiRegisterCustomControls(hDLL);

	m_maingauge = 0;
	m_retrogauge = 0;
	m_hovergauge = 0;
	m_pVessel = nullptr;
}

// --------------------------------------------------------------

oapi::RControl::~RControl()
{
	// Unregister the custom command for calling the plugin
	oapiUnregisterCustomCmd(m_dwCmd);

	// Unregister custom dialog controls
	oapiUnregisterCustomControls(GetModule());
}

// --------------------------------------------------------------

INT_PTR oapi::RControl::InitDialog(HWND hDlg)
{
	CreateVesselList(hDlg);
	return TRUE;
}

// --------------------------------------------------------------

INT_PTR oapi::RControl::DestroyDialog()
{
	m_hDlg = NULL;
	return 0;
}

// --------------------------------------------------------------

void oapi::RControl::CreateVesselList(HWND hDlg)
{
	char cbuf[128];
	DWORD i, n = oapiGetVesselCount();
	SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_RESETCONTENT, 0, 0);
	for (i = 0; i < n; i++) {
		OBJHANDLE hVessel = oapiGetVesselByIndex(i);
		oapiGetObjectName(hVessel, cbuf, 128);
		SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	m_pVessel = oapiGetFocusInterface();
	SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_SELECTSTRING, 0, (LPARAM)m_pVessel->GetName());
}

// --------------------------------------------------------------

void oapi::RControl::clbkPreStep (double simt, double simdt, double mjd)
{
	if (!m_hDlg) return;

	int slider;
	slider = (int)(m_pVessel->GetThrusterGroupLevel (THGROUP_MAIN) * 100.0 + 0.5);
	if (slider != m_maingauge)
		oapiSetGaugePos (GetDlgItem (m_hDlg, IDC_MAIN_GAUGE), m_maingauge = slider);
	slider = (int)(m_pVessel->GetThrusterGroupLevel (THGROUP_RETRO) * 100.0 + 0.5);
	if (slider != m_retrogauge)
		oapiSetGaugePos (GetDlgItem (m_hDlg, IDC_RETRO_GAUGE), m_retrogauge = slider);
	slider = (int)(m_pVessel->GetThrusterGroupLevel (THGROUP_HOVER) * 100.0 + 0.5);
	if (slider != m_hovergauge)
		oapiSetGaugePos (GetDlgItem (m_hDlg, IDC_HOVER_GAUGE), m_hovergauge = slider);

	// RCS button status
	double rcslevel = oapiGetGaugePos (GetDlgItem (m_hDlg, IDC_RCSLEVEL)) * 0.01;
	if (SendDlgItemMessage (m_hDlg, IDC_RCS_PITCHUP, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHUP, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_PITCHDOWN, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHDOWN, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_YAWLEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWLEFT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_YAWRIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWRIGHT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_BANKLEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKLEFT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_BANKRIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKRIGHT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_UP, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_UP, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_DOWN, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_DOWN, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_LEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_LEFT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_RIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_RIGHT, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_FORWARD, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_FORWARD, rcslevel);
	else if (SendDlgItemMessage (m_hDlg, IDC_RCS_BACK, BM_GETSTATE, 0, 0) & BST_PUSHED)
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BACK, rcslevel);
}

// --------------------------------------------------------------

void oapi::RControl::hookOpenDlg(void* context)
{
	self->clbkOpenDlg(context);
}

// --------------------------------------------------------------

void oapi::RControl::clbkOpenDlg(void* context)
{
	HWND hDlg = oapiOpenDialog(GetModule(), IDD_INTERFACE, hookDlgMsgProc);
	if (hDlg) {
		m_hDlg = hDlg;

		GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
		oapiSetGaugeParams(GetDlgItem(hDlg, IDC_RCSLEVEL), &gp);
		oapiSetGaugePos(GetDlgItem(hDlg, IDC_RCSLEVEL), 100);
		gp.color = GAUGEPARAM::RED;
		oapiSetGaugeParams(GetDlgItem(hDlg, IDC_MAIN_GAUGE), &gp);
		oapiSetGaugePos(GetDlgItem(hDlg, IDC_MAIN_GAUGE), m_maingauge = 0);
		gp.base = GAUGEPARAM::RIGHT;
		oapiSetGaugeParams(GetDlgItem(hDlg, IDC_RETRO_GAUGE), &gp);
		oapiSetGaugePos(GetDlgItem(hDlg, IDC_RETRO_GAUGE), m_retrogauge = 0);
		gp.base = GAUGEPARAM::BOTTOM;
		oapiSetGaugeParams(GetDlgItem(hDlg, IDC_HOVER_GAUGE), &gp);
		oapiSetGaugePos(GetDlgItem(hDlg, IDC_HOVER_GAUGE), m_hovergauge = 0);
	}
}

// --------------------------------------------------------------

INT_PTR CALLBACK oapi::RControl::hookDlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return self->DlgMsgProc(hDlg, uMsg, wParam, lParam);
}

// --------------------------------------------------------------

INT_PTR oapi::RControl::DlgMsgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char cbuf[128];
	int idx;
	OBJHANDLE hVessel;

	switch (uMsg) {
	case WM_INITDIALOG:
		return InitDialog(hDlg);
	case WM_DESTROY:
		return DestroyDialog();
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDCANCEL:
			oapiCloseDialog(hDlg);
			return TRUE;
		case IDC_FOCUS:
			oapiSetFocusObject(m_pVessel->GetHandle());
			break;
		case IDC_VESSELLIST:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				idx = SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_GETCURSEL, 0, 0);
				SendDlgItemMessage(hDlg, IDC_VESSELLIST, CB_GETLBTEXT, idx, (LPARAM)cbuf);
				hVessel = oapiGetVesselByName(cbuf);
				if (hVessel) m_pVessel = oapiGetVesselInterface(hVessel);
			}
			break;

		case IDC_KILLROT:
		case IDC_KILLROT2:
			m_pVessel->ToggleNavmode(NAVMODE_KILLROT);
			return TRUE;
		case IDC_HLEVEL:
			m_pVessel->ToggleNavmode(NAVMODE_HLEVEL);
			return TRUE;
		case IDC_PROGRADE:
			m_pVessel->ToggleNavmode(NAVMODE_PROGRADE);
			return TRUE;
		case IDC_RETROGRADE:
			m_pVessel->ToggleNavmode(NAVMODE_RETROGRADE);
			return TRUE;
		case IDC_NORMAL:
			m_pVessel->ToggleNavmode(NAVMODE_NORMAL);
			return TRUE;
		case IDC_ANTINORMAL:
			m_pVessel->ToggleNavmode(NAVMODE_ANTINORMAL);
			return TRUE;
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID((HWND)lParam)) {
		case IDC_MAIN_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				m_pVessel->SetThrusterGroupLevel(THGROUP_MAIN, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		case IDC_RETRO_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				m_pVessel->SetThrusterGroupLevel(THGROUP_RETRO, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		case IDC_HOVER_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				m_pVessel->SetThrusterGroupLevel(THGROUP_HOVER, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		}
		break;
	}
	return oapiDefDialogProc(hDlg, uMsg, wParam, lParam);
}
