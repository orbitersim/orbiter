#ifndef __LUAMFD_H
#define __LUAMFD_H

#include "MfdInterpreter.h"

// ==============================================================
// MFD class interface

class ScriptMFD: public MFD {
public:
	ScriptMFD (DWORD w, DWORD h, VESSEL *vessel);
	~ScriptMFD();
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool ConsumeKeyBuffered (DWORD key);
	bool ConsumeButton (int bt, int event);
	void Update (HDC hDC);
	bool Input (const char *line);
	void QueryCommand ();
	void CreateInterpreter ();
	void DeleteInterpreter ();
	void SetPage (DWORD newpg);
	static bool ScriptInput (void *id, char *str, void *data);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	void SetFontSize (double size);

private:
	InterpreterList::VesselInterp *vi;
	OBJHANDLE hVessel;      // vessel object handle
	HANDLE interpTh;        // interpreter thread handle
	HFONT hFont;            // font handle
	DWORD pg;               // current page   
	DWORD fw, fh;           // character width, height
	DWORD nchar;            // characters per line displayed
	DWORD nline;            // max number of lines displayed
};

#endif // !__LUAMFD_H