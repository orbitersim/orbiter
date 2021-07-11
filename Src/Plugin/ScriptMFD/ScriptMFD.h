// ==============================================================
//                 ORBITER MODULE: ScriptMFD
//                  Part of the ORBITER SDK
//          Copyright (C) 2010-2016 Martin Schweiger
//                   All rights reserved
//
// ScriptMFD.h
//
// This module loads MFD modes defined via Lua scripts.
// ==============================================================

#ifndef __SCRIPTMFD_H
#define __SCRIPTMFD_H


const int NCLBK = 13;
const int SETUP               =  0;
const int BUTTONLABEL         =  1;
const int BUTTONMENU          =  2;
const int CONSUMEBUTTON       =  3;
const int CONSUMEKEYBUFFERED  =  4;
const int CONSUMEKEYIMMEDIATE =  5;
const int UPDATE              =  6;
const int STORESTATUS         =  7;
const int RECALLSTATUS        =  8;
const int WRITESTATUS         =  9;
const int READSTATUS          = 10;
const int PRESTEP             = 11;
const int POSTSTEP            = 12;

const char *CLBKNAME[NCLBK] = {
	"setup", "buttonlabel", "buttonmenu", "consumebutton", "consumekeybuffered", "consumekeyimmediate",
	"update", "storestatus", "recallstatus", "writestatus", "readstatus", "prestep", "poststep"
};

typedef struct {
	int mode;
	DWORD key;
	char *name;
	char *script;
	int persist;
} SCRIPTMFDMODESPEC;


class ScriptMFD: public MFD2 {
public:
	ScriptMFD (DWORD w, DWORD h, VESSEL *vessel, const SCRIPTMFDMODESPEC *spec);
	~ScriptMFD ();
	bool ConsumeButton (int bt, int event);
	bool ConsumeKeyBuffered (DWORD key);
	bool ConsumeKeyImmediate (char *kstate);
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool Update (oapi::Sketchpad *skp);
	void StoreStatus() const;
	void RecallStatus();
	void WriteStatus (FILEHANDLE scn) const;
	void ReadStatus (FILEHANDLE scn);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	INTERPRETERHANDLE hInterp;
	lua_State *L;

	bool bclbk[NCLBK];
	int persist; // 0=delete with MFD object, 1=attach to vessel
};

#endif // !__SCRIPTMFD_H