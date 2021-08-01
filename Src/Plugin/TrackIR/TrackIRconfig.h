#ifndef __TRACKIRCONFIG_H
#define __TRACKIRCONFIG_H

#include "orbitersdk.h"
#include "TrackIR.h"

static const char *cfgfile = "TrackIR.cfg";
#define NTAB 4

class TrackIRconfig: public LaunchpadItem {
public:
	TrackIRconfig (TrackIR *_trackir): LaunchpadItem(), trackir(_trackir) {}
	char *Name() { return "TrackIR Configuration"; }
	char *Description();
	bool clbkOpen (HWND hLaunchpad);
	int  clbkWriteConfig ();
	void InitDialog (HWND hDlg);
	void CloseDialog (HWND hDlg);
	void SwitchTab (HWND hDlg);
	void Apply (HWND hDlg);

private:
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
	static INT_PTR CALLBACK TabProc_mode (HWND, UINT, WPARAM, LPARAM);
	static INT_PTR CALLBACK TabProc_cfg  (HWND, UINT, WPARAM, LPARAM);
	static INT_PTR CALLBACK TabProc_trk  (HWND, UINT, WPARAM, LPARAM);
	static INT_PTR CALLBACK TabProc_diag (HWND, UINT, WPARAM, LPARAM);

	static TrackIRconfig *tirc;
	TrackIR *trackir;
	HWND hTab[NTAB];
};

#endif __!TRACKIRCONFIG_H