// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define OAPI_IMPLEMENTATION

#include <dinput.h>
#include "Pane.h"
#include "Orbiter.h"
#include "Config.h"
#include "Mfd.h"
#include "MfdOrbit.h"
#include "MfdSurface.h"
#include "MfdMap.h"
#include "MfdMap_old.h"
#include "MfdDocking.h"
#include "MfdLanding.h"
#include "MfdComms.h"
#include "MfdHsi.h"
#include "MfdAlign.h"
#include "MfdSync.h"
#include "MfdTransfer.h"
#include "MfdUser.h"
#include "Log.h"
#include "Util.h"
#include "Psys.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern TimeData td;
extern Select *g_select;
extern InputBox *g_input;
extern char DBG_MSG[256];

static char KeyStr[11] = "[xShift-x]";
char Key2Char[256] = {
	' ',' ','1','2','3','4','5','6','7','8','9','0','-','=',' ',
	' ','Q','W','E','R','T','Y','U','I','O','P','[',']',' ',
	' ','A','S','D','F','G','H','J','K','L',';','\'','`',' ',
	'\\','Z','X','C','V','B','N','M',',','.','/'};

HPEN Instrument::hdefpen[MAXPEN] = {0};

struct Instrument::DrawResource Instrument::draw[MAXDEFCOL][2] = {0};

// =======================================================================
// class Instrument

	// ******************************************************************
	// **** Default Instrument registration: these functions must be ****
	// ****     updated when a new builtin instrument is defined     ****
	// ******************************************************************

Instrument *Instrument::Create (int type, Pane *_pane,
	int _id, const Spec &spec, Vessel *_vessel, bool restore)
{
	DWORD i;
	
	// check whether mode is disabled
	for (i = 0; i < nDisabledModes; i++)
		if (DisabledModes[i] == type) return 0;

	// check for built-in mode
	switch (type) {
	case MFD_ORBIT:       TRACENEW return new Instrument_Orbit       (_pane, _id, spec, _vessel);
	case MFD_SURFACE:     TRACENEW return new Instrument_Surface     (_pane, _id, spec, _vessel);
	case MFD_MAP:         TRACENEW if (g_pOrbiter->Cfg()->CfgLogicPrm.MFDMapVersion == 0)
								return new Instrument_MapOld (_pane, _id, spec, _vessel);
							  else
								return new Instrument_Map    (_pane, _id, spec, _vessel);
	case MFD_HSI:         TRACENEW return new Instrument_HSI         (_pane, _id, spec, _vessel);
	case MFD_LANDING:     TRACENEW return new Instrument_Landing     (_pane, _id, spec, _vessel, restore);
	case MFD_DOCKING:     TRACENEW return new Instrument_Docking     (_pane, _id, spec, _vessel, restore);
	case MFD_OPLANEALIGN: TRACENEW return new Instrument_OPlaneAlign (_pane, _id, spec, _vessel, restore);
	case MFD_OSYNC:       TRACENEW return new Instrument_OSync       (_pane, _id, spec, _vessel, restore);
	case MFD_TRANSFER:    TRACENEW return new Instrument_Transfer    (_pane, _id, spec, _vessel);
	case MFD_COMMS:       TRACENEW return new Instrument_Comms       (_pane, _id, spec, _vessel);
	case MFD_USERTYPE:    TRACENEW return new Instrument_User        (_pane, _id, spec, _vessel); // generic user type
	}

	// check for specific user-defined mode
	for (i = BUILTIN_MFD_MODES; i < nGlobalModes; i++)
		if (type == GlobalMode[i].id) {
			TRACENEW return new Instrument_User (_pane, _id, spec, _vessel, type, GlobalMode[i]);
		}

	// check for vessel-specific modes
	const MFDMODE *mlist;
	DWORD nmode = _vessel->GetMFDModes (&mlist);
	for (i = 0; i < nmode; i++)
		if (type == mlist[i].id) {
			return new Instrument_User (_pane, _id, spec, _vessel, type, mlist[i]);
		}

	// nothing found
	return 0;
}

Instrument *Instrument::Create (ifstream &ifs, Pane *_pane,
	int _id, const Spec &spec, Vessel *_vessel)
{
	static char *mfdstr[MAXMFD] = {"Left","Right","3","4","5","6","7","8","9","10","11","12"};
	Instrument *instr = 0;
	char header[64], cbuf[256], *pc;
	strcpy (header, "BEGIN_MFD ");
	strcat (header, mfdstr[min(MAXMFD-1,_id)]);

	if (!FindLine (ifs, header)) return 0;
	for (;instr == 0;) {
		if (!ifs.getline (cbuf, 256)) return 0;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) return 0;
		if (!_strnicmp (pc, "TYPE", 4)) {
			pc = trim_string (pc+4);
			if      (!_strnicmp (pc, "Orbit", 5))    instr = Create (MFD_ORBIT, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "Surface", 7))  instr = Create (MFD_SURFACE, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "Map", 3))      instr = Create (MFD_MAP, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "HSI", 3))      instr = Create (MFD_HSI, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "Launch", 6))   instr = Create (MFD_LANDING, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "Docking", 7))  instr = Create (MFD_DOCKING, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "OAlign", 6))   instr = Create (MFD_OPLANEALIGN, _pane, _id, spec, _vessel, false);
			else if (!_strnicmp (pc, "OSync", 5))    instr = Create (MFD_OSYNC, _pane, _id, spec, _vessel, false);
			else if (!_strnicmp (pc, "Transfer", 8)) instr = Create (MFD_TRANSFER, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "COM/NAV", 4))  instr = Create (MFD_COMMS, _pane, _id, spec, _vessel);
			else if (!_strnicmp (pc, "User", 4))     instr = Create (MFD_USERTYPE, _pane, _id, spec, _vessel);
		}
	}
	if (instr) {
		if (!instr->ReadParams (ifs)) { // problem during initialisation
			delete instr;
			instr = 0;
		}
	}
	return instr;
}

void Instrument::GlobalInit (oapi::GraphicsClient *gc)
{
	int i, j;

	// Default settings
	draw[0][0].col = 0x00FF00;  // main colour bright
	draw[0][1].col = 0x008000;  // main colour dim
	draw[1][0].col = 0x00FFFF;  // aux colour 1 bright
	draw[1][1].col = 0x008080;  // aux colour 1 dim
	draw[2][0].col = 0xF0F0F0;  // aux colour 2 bright
	draw[2][1].col = 0x909090;  // aux colour 2 dim
	draw[3][0].col = 0x6060FF;  // aux colour 3 bright
	draw[3][1].col = 0x2020A0;  // aux colour 3 dim
	draw[4][0].col = 0xFF4040;  // aux colour 4 bright
	draw[4][1].col = 0xA00000;  // aux colour 4 dim

	// Read customised settings
	ifstream ifs (g_pOrbiter->ConfigPath ("MFD\\Default"));
	if (ifs) {
		char label[64];
		int c;
		for (i = 0; i < MAXDEFCOL; i++) {
			for (j = 0; j < 2; j++) {
				sprintf (label, "COL_%d_%s", i, j ? "DIM":"BRT");
				if (GetItemHex (ifs, label, c)) draw[i][j].col = c;
			}
		}
	}

	// Create drawing resources
	for (i = 0; i < MAXDEFCOL; i++) {
		for (j = 0; j < 2; j++) {
			draw[i][j].solidpen = (gc ? gc->clbkCreatePen (1, 0, draw[i][j].col) : NULL);
			draw[i][j].dashpen  = (gc ? gc->clbkCreatePen (2, 0, draw[i][j].col) : NULL);
		}
	}

	// Deprecated pens
	COLORREF pencol[MAXPEN] = {
		RGB(0,255,0), RGB(0,255,0), RGB(0,128,0),
		RGB(128,128,0), RGB(64,64,0), RGB(128,128,128)
	};
	for (i = 0; i < MAXPEN; i++) {
		hdefpen[i] = CreatePen (PS_SOLID, 0, pencol[i]);
	}
}

void Instrument::GlobalExit (oapi::GraphicsClient *gc)
{
	int i, j;

	for (i = 0; i < MAXPEN; i++)
		DeleteObject (hdefpen[i]);
	for (i = 0; i < MAXDEFCOL; i++) {
		for (j = 0; j < 2; j++) {
			if (gc && draw[i][j].solidpen) gc->clbkReleasePen (draw[i][j].solidpen);
			if (gc && draw[i][j].dashpen)  gc->clbkReleasePen (draw[i][j].dashpen);
		}
	}
}

void Instrument::RegisterBuiltinModes ()
{
	static MFDMODESPECEX def_mode[BUILTIN_MFD_MODES] = {
		{"Orbit",        DIK_O, 0, 0},
		{"Surface",      DIK_S, 0, 0},
		{"Map",          DIK_M, 0, 0},
		{"HSI",          DIK_H, 0, 0},
		{"VOR/VTOL",     DIK_L, 0, 0},
		{"Docking",      DIK_D, 0, 0},
		{"Align Planes", DIK_A, 0, 0},
		{"Sync Orbit",   DIK_Y, 0, 0},
		{"Transfer",     DIK_X, 0, 0},
		{"COM/NAV",      DIK_C, 0, 0}
	};
	static MFDMODESPEC def_oldmode[BUILTIN_MFD_MODES] = { // obsolete
		{"Orbit",        DIK_O, 0},
		{"Surface",      DIK_S, 0},
		{"Map",          DIK_M, 0},
		{"HSI",          DIK_H, 0},
		{"VOR/VTOL",     DIK_L, 0},
		{"Docking",      DIK_D, 0},
		{"Align Planes", DIK_A, 0},
		{"Sync Orbit",   DIK_Y, 0},
		{"Transfer",     DIK_X, 0},
		{"COM/NAV",      DIK_C, 0}
	};

	static int def_id[BUILTIN_MFD_MODES] = {
		MFD_ORBIT,
		MFD_SURFACE,
		MFD_MAP,
		MFD_HSI,
		MFD_LANDING,
		MFD_DOCKING,
		MFD_OPLANEALIGN,
		MFD_OSYNC,
		MFD_TRANSFER,
		MFD_COMMS,
	};
	if (nGlobalModes) {
		for (DWORD i = BUILTIN_MFD_MODES; i < nGlobalModes; i++)
			delete GlobalMode[i].spec;
		delete []GlobalMode;
	}
	GlobalMode = new MFDMODE[nGlobalModes = BUILTIN_MFD_MODES]; TRACENEW
	for (int j = 0; j < BUILTIN_MFD_MODES; j++) {
		GlobalMode[j].spec = def_mode+j;
		GlobalMode[j].oldspec = def_oldmode+j;
		GlobalMode[j].id   = def_id[j];
	}
}

	// ******************************************************************
	// ****            End default Instrument registration           ****
	// ******************************************************************

int Instrument::ModeIdFromKey (DWORD key)
{
	for (DWORD i = 0; i < nGlobalModes; i++) {
		if (GlobalMode[i].spec->key == key) return GlobalMode[i].id;
	}
	return MFD_NONE;
}

int Instrument::ModeFromNameOld (char *name, MFDMODESPEC **spec)
{
	for (DWORD i = 0; i < nGlobalModes; i++) {
		if (!_stricmp (GlobalMode[i].spec->name, name)) {
			if (spec) *spec = GlobalMode[i].oldspec;
			return GlobalMode[i].id;
		}
	}
	return MFD_NONE;
}

int Instrument::ModeFromName (char *name, MFDMODESPECEX **spec)
{
	for (DWORD i = 0; i < nGlobalModes; i++) {
		if (!_stricmp (GlobalMode[i].spec->name, name)) {
			if (spec) *spec = GlobalMode[i].spec;
			return GlobalMode[i].id;
		}
	}
	return MFD_NONE;
}

int Instrument::VesselModeFromName (const char *name, MFDMODESPECEX **spec)
{
	const MFDMODE *mlist;
	DWORD nmode = vessel->GetMFDModes (&mlist);
	for (DWORD i = 0; i < nmode; i++) {
		if (!_stricmp (mlist[i].spec->name, name)) {
			if (spec) *spec = mlist[i].spec;
			return mlist[i].id;
		}
	}
	return MFD_NONE;
}

int Instrument::RegisterUserMode (MFDMODESPEC *mode)
{
	// OBSOLETE; replaced by RegisterUserMode (MFDMODESPECEX *mode)

	MFDMODE *tmp = new MFDMODE[nGlobalModes+1]; TRACENEW
	if (nGlobalModes) {
		memcpy (tmp, GlobalMode, nGlobalModes*sizeof(MFDMODE));
		delete []GlobalMode;
	}
	GlobalMode = tmp;
	GlobalMode[nGlobalModes].spec = new MFDMODESPECEX; TRACENEW
	GlobalMode[nGlobalModes].oldspec = new MFDMODESPEC; TRACENEW
	memcpy (GlobalMode[nGlobalModes].oldspec, mode, sizeof(MFDMODESPEC));
	GlobalMode[nGlobalModes].id   = nextmodeid;

	// check for duplicate key codes and disable if required
	if (mode->key) {
		DWORD i, k;
		for (i = 0; i < nGlobalModes; i++)
			if (GlobalMode[i].oldspec->key == mode->key) break;
		if (i < nGlobalModes) { // key already assigned
			GlobalMode[nGlobalModes].oldspec->key = 0;
			for (k = OAPI_KEY_1; k <= OAPI_KEY_0; k++) {
				for (i = 0; i < nGlobalModes; i++)
					if (GlobalMode[i].oldspec->key == k) break;
				if (i == nGlobalModes) { // found un-assigned key
					GlobalMode[nGlobalModes].oldspec->key = k;
					break;
				}
			}
		}
	}
	GlobalMode[nGlobalModes].spec->key = GlobalMode[nGlobalModes].oldspec->key;
	GlobalMode[nGlobalModes].spec->name = new char[strlen(mode->name)+1];
	strcpy (GlobalMode[nGlobalModes].spec->name, mode->name);
	GlobalMode[nGlobalModes].spec->msgproc = GlobalMode[nGlobalModes].oldspec->msgproc;
	GlobalMode[nGlobalModes].spec->context = NULL;

	nGlobalModes++;
	return nextmodeid++;
}

int Instrument::RegisterUserMode (MFDMODESPECEX *mode)
{
	MFDMODE *tmp = new MFDMODE[nGlobalModes+1]; TRACENEW
	if (nGlobalModes) {
		memcpy (tmp, GlobalMode, nGlobalModes*sizeof(MFDMODE));
		delete []GlobalMode;
	}
	GlobalMode = tmp;
	GlobalMode[nGlobalModes].spec = new MFDMODESPECEX; TRACENEW
	GlobalMode[nGlobalModes].oldspec = new MFDMODESPEC; TRACENEW
	memcpy (GlobalMode[nGlobalModes].spec, mode, sizeof(MFDMODESPECEX));
	GlobalMode[nGlobalModes].spec->name = new char[strlen(mode->name)+1];
	strcpy (GlobalMode[nGlobalModes].spec->name, mode->name);
	GlobalMode[nGlobalModes].id   = nextmodeid;

	// check for duplicate key codes and disable if required
	if (mode->key) {
		DWORD i, k;
		for (i = 0; i < nGlobalModes; i++)
			if (GlobalMode[i].spec->key == mode->key) break;
		if (i < nGlobalModes) { // key already assigned
			GlobalMode[nGlobalModes].spec->key = 0;
			for (k = OAPI_KEY_1; k <= OAPI_KEY_0; k++) {
				for (i = 0; i < nGlobalModes; i++)
					if (GlobalMode[i].spec->key == k) break;
				if (i == nGlobalModes) { // found un-assigned key
					GlobalMode[nGlobalModes].spec->key = k;
					break;
				}
			}
		}
	}
	GlobalMode[nGlobalModes].oldspec->key = GlobalMode[nGlobalModes].spec->key; // obsolete
	GlobalMode[nGlobalModes].oldspec->name = GlobalMode[nGlobalModes].spec->name; // obsolete
	GlobalMode[nGlobalModes].oldspec->msgproc = GlobalMode[nGlobalModes].spec->msgproc; // obsolete

	nGlobalModes++;
	return nextmodeid++;
}

bool Instrument::UnregisterUserMode (int id)
{
	DWORD i, j, k;
	for (i = BUILTIN_MFD_MODES; i < nGlobalModes; i++)
		if (GlobalMode[i].id == id) break;
	if (i == nGlobalModes) return false;

	delete []GlobalMode[i].spec->name;
	delete GlobalMode[i].spec;
	delete GlobalMode[i].oldspec; // obsolete
	MFDMODE *tmp = new MFDMODE[nGlobalModes-1]; TRACENEW
	for (j = k = 0; j < nGlobalModes; j++)
		if (j != i) { 
			tmp[k].spec = GlobalMode[j].spec;
			tmp[k].oldspec = GlobalMode[j].oldspec; // obsolete
			tmp[k].id = GlobalMode[j].id; 
			k++;
		}
	delete []GlobalMode;
	GlobalMode = tmp;
	nGlobalModes--;
	return true;
}

int Instrument::GetUserMode (DWORD i, MFDMODESPEC *spec)
{
	i += BUILTIN_MFD_MODES;
	if (i < nGlobalModes) {
		spec = GlobalMode[i].oldspec;
		return GlobalMode[i].id;
	} else return -1;
}

int Instrument::GetMFDMode (char *name, const MFDMODESPECEX **spec) const
{
	for (DWORD i = BUILTIN_MFD_MODES; i < nGlobalModes+nVesselModes; i++) {
		const MFDMODE *m = GetMode(i);
		if (!strcmp (m->Spec()->name, name)) {
			*spec = m->Spec();
			return m->Id();
		}
	}
	return -1;
}

int Instrument::GetMFDModeOld (char *name, const MFDMODESPEC **spec) const
{
	for (DWORD i = BUILTIN_MFD_MODES; i < nGlobalModes+nVesselModes; i++) {
		const MFDMODE *m = GetMode(i);
		if (!strcmp (m->Spec()->name, name)) {
			*spec = m->Oldspec();
			return m->Id();
		}
	}
	return -1;
}

int      Instrument::nextmodeid = BUILTIN_MFD_MODES+1;
DWORD    Instrument::nGlobalModes     = 0;
MFDMODE *Instrument::GlobalMode       = 0;

void Instrument::DisableMode (int id)
{
	for (DWORD i = 0; i < nDisabledModes; i++)
		if (DisabledModes[i] == id) return; // disabled already

	int *tmp = new int[nDisabledModes+1]; TRACENEW
	if (nDisabledModes) {
		memcpy (tmp, DisabledModes, nDisabledModes*sizeof(int));
		delete []DisabledModes;
	}
	DisabledModes = tmp;
	DisabledModes[nDisabledModes++] = id;
}

void Instrument::ClearDisabledModes ()
{
	if (nDisabledModes) {
		delete []DisabledModes;
		nDisabledModes = 0;
	}
}

bool Instrument::IsDisabledMode (int id)
{
	for (DWORD i = 0; i < nDisabledModes; i++)
		if (DisabledModes[i] == id) return true;
	return false;
}

Instrument::Instrument (Pane *_pane, int _id, const Spec &spec, Vessel *_vessel, bool defer_alloc)
{
	pane = _pane;
	gc   =  g_pOrbiter->GetGraphicsClient();
	id   = _id;
	flag = spec.flag;
	vessel = _vessel;
	use_skp_interface = true;
	instrDT = g_pOrbiter->Cfg()->CfgLogicPrm.InstrUpdDT;
	updT = td.SimT0-1.0;
	updSysT = td.SysT0-1.0;
	dT = 1.0, pT = -1.0;
	blink = true;
	showmenu = false;
	pageonmenu = true;
	btnpage = 0;
	lastkey = (char)255;
	surf = NULL;
	tex  = NULL;
	//npen = 0;
	modepage = -1;
	mfdfont[0] = 0;
	SetSize (spec, defer_alloc);

	nVesselModes = vessel->GetMFDModes (&VesselMode);
}

Instrument::~Instrument ()
{
	int i;
	if (gc) {
		for (i = 0; i < 4; i++)
			gc->clbkReleaseFont (mfdfont[i]);
		//for (i = 0; i < npen; i++)
		//	gc->clbkReleasePen (mfdpen[i]);
	}
	ReleaseSurface ();
}

bool Instrument::BufKey (char key, double repdelay)
{
	if ((key == lastkey) && (td.SysT0 - lastkeytime < repdelay)) return false;
	lastkey = key;
	lastkeytime = td.SysT0;
	return true;
}

void Instrument::SetBuf (char key)
{
	lastkey = key;
	lastkeytime = td.SysT0;
}

bool Instrument::ConsumeKeyImmediate (char *kstate)
{
	bool accepted = KeyImmediate (kstate); // current MFD has consumed key
	if (accepted && showmenu) {
		showmenu = false, updT = td.SimT0-1.0;
	}
	return accepted;
}

bool Instrument::ConsumeKeyBuffered (DWORD key)
{
	// part 1: global keys
	switch (key) {
	case DIK_F1:     // MFD mode selection
		if ((++modepage)*(nbtl+nbtr) < (int)(nGlobalModes+nVesselModes-nDisabledModes)) {
			showmenu = false;
			DisplayModes (modepage);
		} else {
			modepage = -1;
		}
		pane->RepaintMFDButtons (id, this);
		return true;
	case DIK_F2: {   // next button page
		if (modepage >= 0) return ConsumeKeyBuffered (DIK_F1); // page through mode pages
		int nfunc = BtnMenu(0);
		if (nfunc > 0 && nfunc > nbt) {
			int npage = (nfunc+nbt-1)/nbt;
			if (++btnpage == npage) btnpage = 0;
			pane->RepaintMFDButtons (id, this);
			if (showmenu) DrawMenu ();
		}
		} return true;
	case DIK_GRAVE: // MFD menu
		if (!showmenu) {
			showmenu = true;
			if (modepage >= 0) {
				modepage = -1;
				pane->RepaintMFDButtons (id, this);
			}
		} else if (!pageonmenu) {
			showmenu = false;
		} else {
			ConsumeKeyBuffered (DIK_F2);
			if (!btnpage) showmenu = false;
		}
		if (showmenu) DrawMenu(); // draw the button menu
		else updT = td.SimT0-1.0;     // enforce display refresh
		return true;
	}

	// part 2: mfd mode shortcut keys
	if (modepage >= 0) {
		if (ModeIdFromKey (key) == Type()) {
			modepage = -1;
			pane->RepaintMFDButtons (id, this);
		} else {
			for (DWORD n = 0; n < nGlobalModes+nVesselModes; n++) {
				const MFDMODE *m = GetMode(n);
				if (IsDisabledMode (m->Id())) continue;
				if (key == m->Spec()->key) {
					pane->OpenMFD (id, m->Id());
					break;
				}
			}
		}
		return true;
	}

	// part 3: mfd menu keys
	bool accepted =  KeyBuffered (key); // current MFD has consumed key
	if (accepted && showmenu) {
		showmenu = false, updT = td.SimT0-1.0;
		//pane->RepaintMFDButtons (id, this);
	}
	return accepted;
}

bool Instrument::ConsumeButton (int bt, int event)
{
	if (modepage >= 0) { // MFD mode selection

		// find MFD mode under button
		int pg, slot;
		DWORD n;
		for (n = pg = slot = 0; n < nGlobalModes+nVesselModes; n++) {
			const MFDMODE *m = GetMode(n);
			if (IsDisabledMode(m->Id())) continue;
			if (pg == modepage && slot == bt) break;
			if (++slot == nbtl+nbtr) pg++, slot = 0;
		}
		if (event & PANEL_MOUSE_LBDOWN && n < nGlobalModes+nVesselModes) {
			const MFDMODE *m = GetMode(n);
			if (!pane->OpenMFD (id, m->Id())) { // mode not changed
				modepage = -1;
				pane->RepaintMFDButtons (id, this);
			} // otherwise *this is no longer valid from here!
		}
		return true;
	}
	bt += btnpage*nbt;
	bool accepted = ProcessButton (bt, event);
	if (accepted && showmenu) {
		showmenu = false, updT = td.SimT0-1.0;
		//pane->RepaintMFDButtons (id, this);
	}
	return accepted;
}

const char *Instrument::ButtonLabel (int bt)
{
	static char modelabel[2] = ">";
	if (modepage >= 0) { // mode selection page
		if (flag & MFD_SHOWMODELABELS) {
			return ModeLabel (bt);
		} else {
			modelabel[0] = (bt < nbtl ? '>' : '<');
			bt += modepage*nbt;
			return (bt < nGlobalModes+nVesselModes-nDisabledModes ? modelabel : 0);
		}
	}
	bt += btnpage*nbt;
	return BtnLabel (bt);
}

void Instrument::VesselChanged (Vessel *_vessel)
{
	vessel = _vessel;
}

void Instrument::SetSize (const Spec &spec, bool defer_alloc)
{
	IW = spec.w, IH = spec.h;
	nbtl  = spec.nbtl;
	nbtr  = spec.nbtr;
	nbt   = nbtl+nbtr;
	bt_y0 = spec.bt_y0;
	bt_dy = spec.bt_dy;
	if (!defer_alloc) AllocSurface (IW, IH);

	if (gc && !mfdfont[0]) {
		int h = (IH*9)/200;
		cw = 10, ch = 16; // temporary defaults
		mfdfont[0] = gc->clbkCreateFont (-h, false, "Fixed");
		mfdfont[1] = gc->clbkCreateFont (-(h*3)/4, true, "Sans");
		mfdfont[2] = gc->clbkCreateFont (-(h*3)/4, true, "Sans", oapi::Font::NORMAL, 900);
		mfdfont[3] = gc->clbkCreateFont (-h, true, "Sans");

		oapi::Sketchpad *skp = gc->clbkGetSketchpad (surf);
		if (skp) {
			skp->SetFont (mfdfont[0]);
			DWORD charsize = skp->GetCharSize();
			cw = HIWORD(charsize);
			ch = LOWORD(charsize);
			gc->clbkReleaseSketchpad (skp);
		}
	}
}

void Instrument::AllocSurface (DWORD w, DWORD h)
{
	if (!gc) return;

	DWORD attrib = OAPISURFACE_SKETCHPAD | OAPISURFACE_NOALPHA;
	attrib |= (use_skp_interface ? OAPISURFACE_RENDERTARGET : OAPISURFACE_GDI);

	surf = gc->clbkCreateSurfaceEx (w, h, attrib);
	tex  = gc->clbkCreateSurfaceEx (w, h, OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);
	if (surf) ClearSurface();
}

void Instrument::ReleaseSurface ()
{
	if (surf) {
		g_pOrbiter->ReleaseSurface (surf);
		surf = NULL;
	}
	if (tex) {
		g_pOrbiter->ReleaseSurface (tex);
		tex = NULL;
	}
}

void Instrument::ClearSurface ()
{
	if (gc) gc->clbkFillSurface (surf, 0x000000);
}

oapi::Sketchpad *Instrument::BeginDraw ()
{
	// get a device context and draw the border
	oapi::Sketchpad *skp;
	if (gc && (skp = gc->clbkGetSketchpad (surf))) {
		skp->SetTextColor (draw[0][0].col);
		skp->SetFont (mfdfont[0]);
		if (pane->GetPanelMode() == 1) {
			skp->SetPen (pane->hudpen);
			skp->Rectangle (0, 0, IW, IH);
		}
		skp->SetPen (draw[0][0].solidpen);
		return skp;
	} else {
		return 0;
	}
}

void Instrument::EndDraw (oapi::Sketchpad *skp)
{
	if (gc && skp)
		gc->clbkReleaseSketchpad (skp);
}

HDC Instrument::BeginDrawHDC ()
{
	HDC hDC;
	if (gc && (hDC = gc->clbkGetSurfaceDC (surf))) {
		SetTextColor (hDC, draw[0][0].col);
		SelectObject (hDC, mfdfont[0]->GetGDIFont());
		SetBkMode (hDC, TRANSPARENT);
		SelectObject (hDC, GetStockObject (NULL_BRUSH));
		SelectObject (hDC, hdefpen[0]);
		if (pane->GetPanelMode() == 1) {
			Rectangle (hDC, 0, 0, IW, IH);
		}
		return hDC;
	} else {
		return 0;
	}
}

void Instrument::EndDrawHDC (HDC hDC)
{
	if (gc && hDC) {
		SelectObject (hDC, GetStockObject (NULL_PEN));
		SelectObject (hDC, GetStockObject (NULL_BRUSH));
		gc->clbkReleaseSurfaceDC (surf, hDC);
	}
}

bool Instrument::Update (double upDTscale)
{
	if (!gc) return false;
	bool tstep = (td.SimT1 >= updT && td.SysT1 >= updSysT);
	if (tstep && !showmenu && modepage < 0) {
		dT = td.SimT1 - pT; // actual update interval
		pT = td.SimT1;
		blink = !blink;
		ClearSurface ();
		UpdateBlt ();
		if (use_skp_interface) {
			oapi::Sketchpad *skp = BeginDraw();
			if (skp) {
				UpdateDraw (skp);
				EndDraw (skp);
			}
		} else {
			HDC hDC = BeginDrawHDC();
			if (hDC) {
				UpdateDraw (hDC);
				EndDrawHDC (hDC);
			}
		}
		gc->clbkBlt (tex, 0, 0, surf);
		updT = td.SimT1 + instrDT * upDTscale;
		updSysT = td.SysT1 + 0.1; // don't exceed 10Hz update rate
		return true;
	} else return false;
}

void Instrument::Timejump ()
{
	Refresh ();
	pT = td.SimT0;
	lastkeytime = td.SysT0;
}

void Instrument::Refresh ()
{
	updT = td.SimT0-1.0;
	updSysT = td.SysT0-1.0;
}

void Instrument::RepaintButtons ()
{
	pane->RepaintMFDButtons (id, this);
}

oapi::Font *Instrument::GetDefaultFont (DWORD fontidx)
{
	return (fontidx < 4 ? mfdfont[fontidx] : 0);
}

HFONT Instrument::SelectDefaultFont (HDC hDC, DWORD i)
{
	// obsolete
	return (i < 4 ? (HFONT)SelectObject (hDC, mfdfont[i]->GetGDIFont()) : 0);
}

oapi::Pen *Instrument::GetDefaultPen (DWORD colidx, DWORD intens, DWORD style)
{
	if (colidx >= MAXDEFCOL || intens >= 2) return 0;
	switch (style) {
		case 1: // solid pen
			return draw[colidx][intens].solidpen;
		case 2: // dashed pen
			return draw[colidx][intens].dashpen;
		default:
			return 0;
	}
}

HPEN Instrument::SelectDefaultPen (HDC hDC, DWORD i)
{
	// obsolete
	return (i < 6 ? (HPEN)SelectObject (hDC, hdefpen[i]) : 0);
}

DWORD Instrument::GetDefaultColour (DWORD colidx, DWORD intens) const
{
	if (colidx < MAXDEFCOL && intens < 2)
		return draw[colidx][intens].col;
	else
		return 0;
}

void Instrument::DisplayTitle (oapi::Sketchpad *skp, const char *title) const
{
	skp->SetTextColor (col_grey1);
	skp->Text (cw/2, 0, title, strlen(title));
}

void Instrument::DisplayTitle (HDC hDC, const char *title) const
{
	// obsolete
	SetTextColor (hDC, col_grey1);
	TextOut (hDC, cw/2, 0, title, strlen(title));
}

void Instrument::DisplayModes (int page)
{
	if (!gc) return;
	char *name;
	int n, x, y, pg;
	int y0 = bt_y0 - (id <= MFD_RIGHT ? ch : ch/2);
	int dy = bt_dy;
	int slot;

	ClearSurface();
	oapi::Sketchpad *skp = BeginDraw();
	if (!skp) return;

	if (nGlobalModes-nDisabledModes <= nbt) {
		DisplayTitle (skp, "MODE SELECT");
	} else {
		char cbuf[256];
		sprintf (cbuf, "MODE SELECT [Page %d]", page+1);
		DisplayTitle (skp, cbuf);
	}

	// skip previous pages
	for (n = slot = pg = 0; n < nGlobalModes+nVesselModes && pg < page; n++) {
		const MFDMODE *m = GetMode(n);
		if (IsDisabledMode (m->Id())) continue;
		if (++slot == nbt) pg++, slot = 0;
	}

	// display page
	skp->SetTextAlign (oapi::Sketchpad::LEFT);
	x = cw/2;
	y = y0;
	for (slot = 0; n < nGlobalModes+nVesselModes && slot < nbt; n++) {
		const MFDMODE *m = GetMode(n);
		if (IsDisabledMode (m->Id())) continue;
		if (slot == nbtl) {
			skp->SetTextAlign (oapi::Sketchpad::RIGHT);
			x = IW-cw/2;
			y = y0;
		}
		skp->SetTextColor (col_green1);
		name = m->Spec()->name;
		skp->Text (x, y, name, strlen(name));
		if (id <= MFD_RIGHT) {
			KeyStr[1] = (id == MFD_LEFT ? 'L' : 'R');
			KeyStr[8] = Key2Char[m->Spec()->key];
			skp->SetTextColor (col_grey2);
			skp->Text (x, y+ch, KeyStr, 10);
		}
		y += dy;
		slot++;
	}
	EndDraw (skp);
	gc->clbkBlt (tex, 0, 0, surf);
}

char *Instrument::ModeLabel (int bt)
{
	static char label[4] = "===";
	char *c;
	int n, slot, pg;
	const MFDMODE *mode;

	// skip previous pages
	for (n = slot = pg = 0; n < nGlobalModes+nVesselModes; n++) {
		mode = (n < nGlobalModes ? GlobalMode+n : VesselMode+(n-nGlobalModes));
		if (IsDisabledMode (mode->id)) continue;
		if (pg == modepage && slot == bt) break;
		if (++slot == nbt) pg++, slot = 0;
	}
	if (n >= nGlobalModes+nVesselModes) return 0;
	strncpy (label, mode->spec->name, 3);
	for (c = label; *c; c++) *c = toupper (*c);
	return label;
}

void Instrument::DrawMenu ()
{
	const MFDBUTTONMENU *mnu;
	int side, i, n, x, y, item, itemofs, nmnu;
	int y0 = bt_y0 - (id <= MFD_RIGHT ? ch : ch/2);
	int dy = bt_dy;

	ClearSurface();
	oapi::Sketchpad *skp = BeginDraw ();
	if (!skp) return;

	DisplayTitle (skp, "MFD MENU");
	nmnu = BtnMenu (&mnu);
	for (side = 0; side < 2; side++) {
		skp->SetTextAlign (side ? oapi::Sketchpad::RIGHT : oapi::Sketchpad::LEFT);
		itemofs = btnpage*nbt + (side ? nbtl:0);
		n = (side ? nbtr : nbtl);
		x = (side ? IW-cw/2 : cw/2);
		for (i = 0; i < n; i++) {
			if ((item = itemofs+i) >= nmnu) break;
			y = y0+i*dy - (mnu[item].line2 ? ch/2:0);
			skp->SetTextColor (col_green1);
			if (mnu[item].line1) {
				skp->Text (x, y, mnu[item].line1, strlen(mnu[item].line1));
				y += ch;
			}
			if (mnu[item].line2) {
				skp->Text (x, y, mnu[item].line2, strlen(mnu[item].line2));
				y += ch;
			}
			if (id <= MFD_RIGHT && mnu[item].selchar != '\0') {
				KeyStr[1] = (id == MFD_LEFT ? 'L' : 'R');
				KeyStr[8] = mnu[item].selchar;
				skp->SetTextColor (col_grey2);
				skp->Text (x, y, KeyStr, 10);
			}
		}
	}
	EndDraw (skp);
	gc->clbkBlt (tex, 0, 0, surf);
}

void Instrument::OpenSelect_CelBody (char *title, Select::Callbk enter_cbk, DWORD flag)
{
	SelCelBodyFlag = flag;
	g_select->Open (title, ClbkSelect_CelBody, enter_cbk, (void*)this);
}

bool Instrument::ClbkSelect_CelBody (Select *menu, int item, char *str, void *data)
{
	int i, n;
	if (!str) { // root menu
		n = (SelCelBodyFlag & 1 ? 0 : g_psys->nStar());
		for (i = 0; i < n; i++) {
			Star *star = g_psys->GetStar (i);
			menu->Append (star->Name());
		}
		if (n) menu->AppendSeparator();
		n = g_psys->nPlanet();
		for (i = 0; i < n; i++) {
			Planet *planet = g_psys->GetPlanet (i);
			if (planet->isMoon()) continue; // only use primaries in root
			menu->Append (planet->Name(), planet->nSecondary() ? ITEM_SUBMENU : 0);
		}
		return true;
	} else { // submenu
		CelestialBody *cbody = g_psys->GetGravObj (str, true);
		if (cbody) {
			n = cbody->nSecondary();
			for (i = 0; i < n; i++) {
				const CelestialBody *moon = cbody->Secondary(i);
				menu->Append (moon->Name(), moon->nSecondary() ? ITEM_SUBMENU : 0);
			}
			return (n > 0);
		} else return false;
	}
	return false;
}

void Instrument::OpenSelect_Tgt (char *title, Select::Callbk enter_cbk, const CelestialBody *ref, DWORD flag)
{
	seltgtprm.ref  = ref;
	seltgtprm.flag = flag;
	seltgtprm.clbk = enter_cbk;
	strncpy (seltgtprm.title, title, 256);
	g_select->Open (title, ClbkSelect_Tgt, ClbkEnter_Tgt, (void*)this);
}

bool Instrument::ClbkSelect_Tgt (Select *menu, int item, char *str, void *data)
{
	DWORD i, j;
	char cbuf[256];
	Instrument *mfd = (Instrument*)data;
	const CelestialBody *ref = mfd->seltgtprm.ref;
	DWORD flag = mfd->seltgtprm.flag;

	if (!str) { // main menu
		if (!(flag & 1)) {
			menu->Append ("By name ...");
			menu->AppendSeparator ();
		}
		menu->Append ("Spacecraft", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("Celestial bodies", ITEM_SUBMENU | ITEM_NOHILIGHT);
		return true;
	} else {    // submenu
		if (!strcmp (str, "Spacecraft")) {
			// pick craft orbiting the reference body
			for (i = j = 0; i < g_psys->nVessel(); i++) {
				Vessel *v = g_psys->GetVessel (i);
				if (v == mfd->vessel || v->GetStatus() != FLIGHTSTATUS_FREEFLIGHT) continue;
				if (v->ElRef() == ref) menu->Append (v->Name()), j++;
			}
			if (!j && ref) {
				strcpy (cbuf, "None orbiting "); strcat (cbuf, ref->Name());
				menu->Append (cbuf, ITEM_NOHILIGHT);
			}
			if (i > j && !(flag & 2)) {
				menu->AppendSeparator();
				menu->Append ("More spacecraft", ITEM_SUBMENU | ITEM_NOHILIGHT);
			}
			return true;
		} else if (!strcmp (str, "More spacecraft")) {
			// pick all craft
			for (i = j = 0; i < g_psys->nVessel(); i++) {
				Vessel *v = g_psys->GetVessel (i);
				if (v == mfd->vessel || v->GetStatus() != FLIGHTSTATUS_FREEFLIGHT) continue;
				if (v->ElRef() != ref) menu->Append (v->Name()), j++;
			}
			return (j > 0);
		} else if (!strcmp (str, "Celestial bodies")) {
			// pick bodies orbiting the reference body
			for (i = j = 0; i < g_psys->nGrav(); i++) {
				CelestialBody *cbody = g_psys->GetGravObj (i);
				if (cbody->ElRef() == ref) menu->Append (cbody->Name()), j++;
			}
			if (!j && ref) {
				strcpy (cbuf, "None orbiting "); strcat (cbuf, ref->Name());
				menu->Append (cbuf, ITEM_NOHILIGHT);
			}
			if (i > j && !(flag & 2)) {
				menu->AppendSeparator();
				menu->Append ("More celestial bodies", ITEM_SUBMENU | ITEM_NOHILIGHT);
			}
			return true;
		} else if (!strcmp (str, "More celestial bodies")) {
			return ClbkSelect_CelBody (menu, 0, 0, data);
		} else {
			return ClbkSelect_CelBody (menu, item, str, data);
		}
	}
}

bool Instrument::ClbkEnter_Tgt (Select *menu, int item, char *str, void *data)
{
	Instrument* mfd = (Instrument*)data;
	if (!strcmp (str, "By name ...")) {
		g_input->Open (seltgtprm.title, 0, 25, ClbkName_Tgt, data);
		return true;
	} else return seltgtprm.clbk (menu, item, str, data);
}

bool Instrument::ClbkName_Tgt (InputBox*, char *str, void *data)
{
	return seltgtprm.clbk (g_select, 0, str, data);
}

void Instrument::Write (ostream &ofs) const
{
	ofs << "BEGIN_MFD ";
	switch (id) {
	case 0: ofs << "Left"; break;
	case 1: ofs << "Right"; break;
	default: ofs << id+1; break;
	}
	ofs << endl;
	WriteParams (ofs);
	ofs << "END_MFD" << endl;
}

bool Instrument::FindScnHeader (ifstream &ifs) const
{
	char header[32] = "BEGIN_MFD ";
	switch (id) {
	case 0: strcpy (header+10, "Left"); break;
	case 1: strcpy (header+10, "Right"); break;
	default: _itoa (id+1, header+10, 10); break;
	}
	return FindLine (ifs, header);
}

DWORD Instrument::SelCelBodyFlag = 0;
Instrument::SelTgtPrm Instrument::seltgtprm = {0,0};
DWORD Instrument::nDisabledModes = 0;
int *Instrument::DisabledModes = 0;

COLORREF Instrument::col_green1  = RGB(0,  230,  0);
COLORREF Instrument::col_green2  = RGB(0,  172,  0);
COLORREF Instrument::col_yellow1 = RGB(230,230,  0);
COLORREF Instrument::col_yellow2 = RGB(172,172,  0);
COLORREF Instrument::col_grey1   = RGB(224,224,224);
COLORREF Instrument::col_grey2   = RGB(140,140,140);
COLORREF Instrument::col_red1    = RGB(255,  0,  0);


// =======================================================================
// local utility functions

Matrix RotMatrix (double coso, double sino, double cosp, double sinp, double cosi, double sini)
{
	return Matrix (
		cosp*coso-sinp*cosi*sino, sinp*sini,-cosp*sino-sinp*cosi*coso,
		sini*sino,                cosi,      sini*coso,
		sinp*coso+cosp*cosi*sino,-cosp*sini,-sinp*sino+cosp*cosi*coso);
}

// =======================================================================

Matrix IRotMatrix (double cosp, double sinp, double cosi, double sini)
{
	return Matrix (
		cosp*cosp+sinp*sinp*cosi, -sinp*sini, cosp*sinp-sinp*cosp*cosi,
		sinp*sini,                 cosi,     -cosp*sini,
		sinp*cosp-cosp*sinp*cosi,  cosp*sini, sinp*sinp+cosp*cosp*cosi);
}

// =======================================================================

void UpdateEllipse (int cntx, int cnty, double scale,
	const Elements *el, const Matrix &rot, const Matrix &irot, oapi::IVECTOR2 *p)
{
	int i;
	int idx1 = ELN-1, idx2 = 2*ELNQ, idx3 = idx2-1;
	Vector tmp, v[ELN];
	double phi, sphi, cphi, r, x, y;
	double fac = Pi05/(double)ELNQ;
	double e2 = el->e * el->e;

	for (i = 0; i < ELNQ; i++) {
		phi = (i+0.5)*fac; sphi = sin(phi), cphi = cos(phi);
		r = el->SMi() / sqrt (1.0 - e2*cphi*cphi);
		x = r*cphi, y = r*sphi;
		v[i].x      = v[idx1-i].x =  x - el->LinEcc();
		v[i].z      = v[idx3-i].z =  y;
		v[idx3-i].x = v[idx2+i].x = -x - el->LinEcc();
		v[idx2+i].z = v[idx1-i].z = -y;
	}
	for (i = 0; i < ELN; i++)
		MapScreen (cntx, cnty, scale, mul (rot, v[i]), p+i);
	MapScreen (cntx, cnty, scale, mul (irot, el->RVec()), p+ELN);  // radius vector
	p[ELN+1].x = (p[0].x + p[ELN-1].x)/2;       // periapsis
	p[ELN+1].y = (p[0].y + p[ELN-1].y)/2;
	p[ELN+2].x = (p[ELNH-1].x + p[ELNH].x)/2;   // apoapsis
	p[ELN+2].y = (p[ELNH-1].y + p[ELNH].y)/2;
	el->AscendingNode (tmp);                    // ascending node
	MapScreen (cntx, cnty, scale, mul (irot, tmp), p+(ELN+3));
	el->DescendingNode (tmp);                   // descending node
	MapScreen (cntx, cnty, scale, mul (irot, tmp), p+(ELN+4));
}

// =======================================================================

void UpdateHyperbola (int cntx, int cnty, int IW, int IH, double scale,
	const Elements *el, const Matrix &rot, const Matrix &irot, oapi::IVECTOR2 *pt)
{
	int i;
	int idx = ELNH-1;
	Vector asc, desc, v[ELN-1];
	double phi, cphi, sphi, r, x, y, len;
	double p = el->PeDist()*(1.0+el->e); // parameter of polar equation
	double radmax = 1.5*cntx/scale;
	double phimax = acos ((p/radmax - 1.0)/el->e);
	double fac = phimax/(double)(ELNH-1);
	bool ascok, descok;

	// periapsis
	v[idx].x = el->PeDist(); v[idx].y = 0.0;

	for (i = 1; i < ELNH; i++) {
		phi = i*fac; sphi = sin(phi), cphi = cos(phi);
		r = p / (1.0 + el->e * cphi);
		x = r*cphi, y = r*sphi;
		v[idx-i].x =   v[idx+i].x = x;
		v[idx-i].z = -(v[idx+i].z = y);
	}
	for (i = 0; i < ELN-1; i++)
		MapScreen (cntx, cnty, scale, mul (rot, v[i]), pt+i);

	for (i = 1; i < ELNH; i++) {
		// do some clipping. Leaving this to the renderer can cause problems in extreme cases
		if (pt[idx+i].x < -10*IW || pt[idx+i].x >= 11*IW || pt[idx+i].y < -10*IH || pt[idx+i].y >= 11*IH)
			pt[idx+i].x = pt[idx+i-1].x,  pt[idx+i].y = pt[idx+i-1].y;
		if (pt[idx-i].x < -10*IW || pt[idx-i].x >= 11*IW || pt[idx-i].y < -10*IH || pt[idx-i].y >= 11*IH)
			pt[idx-i].x = pt[idx-i+1].x,  pt[idx-i].y = pt[idx-i+1].y;
	}

	MapScreen (cntx, cnty, scale, mul (irot, el->RVec()), pt+ELN);  // radius vector
	pt[ELN+1].x = pt[idx].x;               // periapsis
	pt[ELN+1].y = pt[idx].y;
	pt[ELN+2].x = -1;                      // apoapsis - mark invalid
	if (ascok = el->AscendingNode (asc)) {               // ascending node
		if ((len = asc.length()) > radmax) asc *= (radmax/len);
		MapScreen (cntx, cnty, scale, mul (irot, asc), pt+(ELN+3));
	}
	if (descok = el->DescendingNode (desc)) {              // descending node
		if ((len = desc.length()) > radmax) desc *= (radmax/len);
		MapScreen (cntx, cnty, scale, mul (irot, desc), pt+(ELN+4));
	}
	if (!ascok) {
		asc = desc * -(radmax/len);
		MapScreen (cntx, cnty, scale, mul (irot, asc), pt+(ELN+3));
	} else if (!descok) {
		desc = asc * -(radmax/len);
		MapScreen (cntx, cnty, scale, mul (irot, desc), pt+(ELN+4));
	}
}

// =======================================================================

void UpdateOrbitGraph (int cntx, int cnty, int IW, int IH, double scale,
	const Elements *el, const Matrix &rot, const Matrix &irot, oapi::IVECTOR2 *p)
{
	if (el->e < 1.0) UpdateEllipse (cntx, cnty, scale, el, rot, irot, p);
	else             UpdateHyperbola (cntx, cnty, IW, IH, scale, el, rot, irot, p);
}
