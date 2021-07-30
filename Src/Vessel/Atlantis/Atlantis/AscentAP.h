// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//
// AscentAP.h
// Class interface for Atlantis ascent autopilot
// Automatic control of ascent profile from liftoff to
// ET separation using engine gimballing of SSME and SRB engines
// ==============================================================

#ifndef __ATLANTIS_ASCENTAP
#define __ATLANTIS_ASCENTAP

#include "Common\Dialog\TabDlg.h"

class Atlantis;
class Graph;

struct ProfSample {
	double t;
	double v;
};

// ==============================================================
// class AscentAP: ascent autopilot
// ==============================================================

class AscentAP {
	friend class AscentApMfd;
	friend class AscentAPDlg;

public:
	AscentAP (Atlantis *atlantis);
	~AscentAP ();

	Atlantis *GetVessel () { return vessel; }
	void Launch ();
	double StartMissionTime (double simt); // start MET counter without engaging AP
	void Update (double simt);
	bool Active() const { return active; }
	void Engage () { active = true; }
	void Disengage ();
	double GetMET (double simt) const;
	double GetMT0 () const { return t_launch; }

	double GetInclination (double lat, double az) const;
	// orbit inclination (0..pi) from current latitude and azimuth

	void SetLaunchAzimuth (double azimuth);
	void SetOrbitAltitude (double alt) { tgt_alt = alt; }
	double GetLaunchAzimuth () const { return launch_azimuth; }
	double GetTargetAzimuth () const { return tgt.az; }
	double GetTargetPitch () const { return tgt.pitch; }
	double GetOrbitAltitude () const { return tgt_alt; }
	double GetTargetInclination ();
	void GetTargetDirection (double met, VECTOR3 &dir, double &tgt_hdg) const;
	void GetTargetRate (double met, VECTOR3 &rate) const;
	void ToggleOMS2();
	bool GetOMS2Schedule() const { return do_oms2; }
	void SaveState (FILEHANDLE scn);
	bool ParseScenarioLine (const char *line);

protected:
	void SetDefaultProfiles ();
	double SSMEThrustProfile(double met);

private:
	double CalcTargetAzimuth () const;
	double CalcTargetPitch () const;
	double GetTargetPitchRate (double dpitch, double vpitch) const;
	double GetTargetYawRate (double dyaw, double vyaw) const;
	double GetTargetRollRate (double tgt, bool tgt_is_heading) const;

	Atlantis *vessel;

	ProfSample *pitch_profile;
	int n_pitch_profile;
	double launch_azimuth;
	double tgt_alt;
	double ecc_min;
	double t_roll_upright;
	double launch_lng, launch_lat;
	double t_launch;
	double met;
	double met_meco, met_oms_start, met_oms_end, schedule_oms;
	double met_oms1_start, schedule_oms1;
	bool active;
	bool met_active;
	bool do_oms2;
	double pt, pspd, acc, pacc, dacc_dt;
	bool pacc_valid;
	struct TGTPRM {
		double inc;   // target orbit inclination
		double lan;   // target orbit longitude of ascending node
		double az;    // current target azimuth
		double pitch; // current target pitch
		MATRIX3 R;    // rotation from equator plane to target plane
	} tgt;
};

// ==============================================================
// class AscentApMfd: MFD interface for ascent autopilot
// ==============================================================

class AscentApMfd: public MFD2 {
public:
	AscentApMfd (DWORD w, DWORD h, VESSEL *v);
	~AscentApMfd();
	bool Update (oapi::Sketchpad *skp);
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool ConsumeKeyBuffered (DWORD key);
	bool ConsumeButton (int bt, int event);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

private:
	void UpdatePg_Prm (oapi::Sketchpad *skp);
	void UpdatePg_Gbl (oapi::Sketchpad *skp);
	void DrawGimbal (oapi::Sketchpad *skp, int cx, int cy, double pitch, double yaw);
	void DecPage();
	void IncPage();
	bool OnLaunch();
	bool OnEngage();
	bool OnDisengage();
	void InitDecAzimuth();
	void InitIncAzimuth();
	void DecAzimuth();
	void IncAzimuth();
	void InitDecAltitude();
	void InitIncAltitude();
	void DecAltitude();
	void IncAltitude();
	void ToggleOMS2Schedule();
	enum SET_MODE { MODE_NONE, MODE_AZIMUTH_DEC, MODE_AZIMUTH_INC } set_mode;
	double ref_t;
	double ref_val;
	AscentAP *ap;
	DWORD cpg;  // current page
	oapi::Pen *pen[2];
};

// ==============================================================
// class AscentAPDlg: dialog interface for ascent autopilot
// ==============================================================

class AscentAPDlgTab;

class AscentAPDlg: public TabbedDialog {
public:
	AscentAPDlg (AscentAP *_ap);
	virtual ~AscentAPDlg ();
	void Update (double simt);
	AscentAP *AP() { return ap; }
	int OnInitDialog (WPARAM wParam);
	int Closed ();

private:
	AscentAP *ap;
};

// ==============================================================
// class AscentAPDlgTab: base class for dialog tabs
// ==============================================================

class AscentAPDlgTab: public TabPage {
public:
	AscentAPDlgTab (AscentAPDlg *frame, int dlgId);

protected:
	AscentAP *ap;
};

// ==============================================================
// class AscentAPDlgTabControl: AP control tab
// ==============================================================

class AscentAPDlgTabControl: public AscentAPDlgTab {
public:
	AscentAPDlgTabControl (AscentAPDlg *frame);

protected:
	int OnInitTab (WPARAM wParam);
	int OnLaunch ();
	int OnCommand (WPARAM wParam, LPARAM lParam);
};

// ==============================================================
// class AscentAPDlgTabGimbal: AP gimbal tab
// ==============================================================

class AscentAPDlgTabGimbal: public AscentAPDlgTab {
public:
	AscentAPDlgTabGimbal (AscentAPDlg *frame);
	~AscentAPDlgTabGimbal();
	void Update (double simt);

protected:
	void RepaintAll (HWND hWnd);
	void PaintGimbalCross (HDC hDC, const RECT &rect, int x, int y);
	void UpdateGimbalCross (HWND hCtrl, int idx, double pitch, double yaw);
	void PaintGimbalBox (HWND hWnd);
	LRESULT DlgProc (HWND, UINT, WPARAM, LPARAM);

private:
	int gimbalx[5], gimbaly[5];
	double rad;
	HPEN pen1, pen2;
};

// ==============================================================
// class AscentAPDlgTabThrust: AP thrust tab
// ==============================================================

class AscentAPDlgTabThrust: public AscentAPDlgTab {
public:
	AscentAPDlgTabThrust (AscentAPDlg *frame);
	~AscentAPDlgTabThrust ();
	void Update (double simt);

protected:
	int OnPaint ();
	void RefreshGraph (Graph *graph, int GraphId);
	LRESULT DlgProc (HWND, UINT, WPARAM, LPARAM);

private:
	Graph *ssmegraph, *srbgraph;
	double updt;
	double dupdt;
};

// ==============================================================
// class AscentAPDlgTabAltitude: AP altitude tab
// ==============================================================

class AscentAPDlgTabAltitude: public AscentAPDlgTab {
public:
	AscentAPDlgTabAltitude (AscentAPDlg *frame);
	~AscentAPDlgTabAltitude ();
	void Update (double simt);

protected:
	int OnPaint ();
	void RefreshGraph (Graph *graph, int GraphId);
	LRESULT DlgProc (HWND, UINT, WPARAM, LPARAM);

private:
	Graph *altgraph;
	double updt;
	double dupdt;
};


// ==============================================================
// auxiliary functions
// ==============================================================

const char *MetStr (double met);

#endif // !__ATLANTIS_ASCENTAP