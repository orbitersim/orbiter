#ifndef __PLBAYOP_H
#define __PLBAYOP_H

#include <windows.h>
#include "Atlantis.h"

// ==============================================================
// class PayloadBayOp
// Manages payload bay operations (bay doors and radiators),
// both the physical state of the systems, and the state of
// the user interface (panel switches, etc.)
// ==============================================================

class PayloadBayOp {
	friend class Atlantis;

public:
	PayloadBayOp (Atlantis *_sts);
	void OpenDialog ();
	void DefineAnimations (UINT vcidx);
	void RegisterVC ();
	void UpdateVC ();
	bool VCMouseEvent (int id, int event, VECTOR3 &p);
	bool VCRedrawEvent (int id, int event, SURFHANDLE surf);

	void Step (double t, double dt);
	inline AnimState::Action GetDoorAction () const { return BayDoorStatus.action; }
	void SetDoorAction (AnimState::Action action, bool simple = false);
	void RevertDoorAction (); // simplified operation

	inline AnimState::Action GetRadiatorAction () const { return RadiatorStatus.action; }
	void SetRadiatorAction (AnimState::Action action);

	void SetRadLatchAction (AnimState::Action action);

	void SetKuAntennaAction (AnimState::Action action);
	void RevertKuAntennaAction (); // simplified operation

	bool ParseScenarioLine (char *line);
	void SaveState (FILEHANDLE scn);

private:
	Atlantis *sts; // vessel instance pointer
	HWND hDlg;     // control dialog handle
	BOOL DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	friend BOOL CALLBACK PlOp_DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	void UpdateDialog (HWND hWnd);
	bool VCDrawTalkback (SURFHANDLE surf, int idx, int label);

	// status of control switches
	enum {BD_ENABLE,BD_DISABLE} BayDoor[2];
	enum {BDO_OPEN,BDO_STOP,BDO_CLOSE} BayDoorOp;
	enum {MP_ON,MP_OFF} MechPwr[2];
	enum {LC_RELEASE,LC_OFF,LC_LATCH} RadLatchCtrl[2];
	enum {RC_DEPLOY,RC_OFF,RC_STOW} RadiatorCtrl[2];
	enum {KU_DEPLOY,KU_GND,KU_STOW} KuCtrl;
	enum {KU_DIRECT_ON, KU_DIRECT_OFF} KuDirectCtrl;

	// VC switch animations
	UINT anim_VC_R13L[11];
	int tkbk_state[6];

	// physical status
	AnimState BayDoorStatus;
	AnimState RadiatorStatus;
	AnimState RadLatchStatus;
	AnimState KuAntennaStatus;
};

#endif // !__PLBAYOP_H