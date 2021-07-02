// =======================================================================
// class MenuInfoBar
// Rendering and user interface for main menu and info bars

#ifndef __MENUINFOBAR_H
#define __MENUINFOBAR_H

#define STRICT 1
#define FPSMAXSAMPLE 60

#include "OrbiterAPI.h"
#include "GraphicsAPI.h"
#include "Mesh.h"

class Pane;

// =======================================================================
// class MenuInfoBar

class MenuInfoBar {
	friend class ExtraInfoBar;
public:
	enum ActionId { ACTION_NONE, ACTION_PAUSE, ACTION_RUN };
	MenuInfoBar (const Pane *_pane);
	~MenuInfoBar ();
	void Update (double t);
	bool ProcessMouse (UINT event, DWORD state, DWORD x, DWORD y);
	void Render ();
	void SetFOV (double fov);
	void SetWarp (double warp);
	void SetWarpAlways (bool on);
	void SetWarpScientific (bool scientific);
	void SetPaused (bool _paused);
	void SetRecording (bool rec);
	void SetPlayback (bool pback);
	void ToggleAutohide ();
	void SetMenuMode (DWORD mode);
	void SetInfoMode (DWORD mode);
	void SetPauseIndicatorMode (DWORD mode);
	void SetOpacity (int opac);
	void SetOpacityInfo (int opac);
	void SetScrollspeed (int speed);
	void SetLabelOnly (bool labelonly);
	void SetAuxInfobar (int which, DWORD idx);
	inline DWORD GetMenuMode() const { return menumode; }
	inline DWORD GetInfoMode() const { return infomode; }

protected:
	int TexBltString (const char *str, int tgtx, int tgty, int cleantox, char *curstr = 0, int maxn=1024);
	void UpdateMeshVertices ();

private:
	const Pane *pane;
	oapi::GraphicsClient *gc; // client instance
	ExtraInfoBar *eibar[2];   // left/right extra info modes
	int time_upd, sys_upd;    // last time display update
	int viewW;
	int menuX, menuW, menuH;
	int infoW, rinfoW, linfoW;
	int itemW, itemN;
	int flagW, flagH;
	int itemHighlight;
	DWORD scrollzone;
	int scrollrange;
	int scrollspeed; // 1-20
	int opacity; // 0-10
	int opacity_info; // 0-10
	int menustate, infostate; // 0=hidden, 1=displayed, 2=in transition
	double scrollpos, scrollpos_info;
	int scrolldir, scrolldir_info; // 0=stop, 1=scrolling down, -1=scrolling up
	DWORD menumode, infomode, pausemode;
	bool fixedstep;
	bool warp_always;
	bool warp_scientific;
	bool show_action;
	bool action_blink;
	double action_time;
	SURFHANDLE menuSrc, menuTgt;
	Mesh barMesh, miniMesh, flagMesh;
	char datestr[30], mjdstr[30], timestr[30];
	char tgtstr[64], dststr[30], camstr[64], recstr[2];
	bool paused, recording, playback;
	double warp;
	MATRIX3 transf;
};

#endif // !__MENUINFOBAR_H