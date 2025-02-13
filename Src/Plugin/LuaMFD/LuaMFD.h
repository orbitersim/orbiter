// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LUAMFD_H
#define __LUAMFD_H

#include "MfdInterpreter.h"

// ==============================================================
// MFD class interface

class ScriptMFD: public MFD {
public:
	ScriptMFD (uint32_t w, uint32_t h, VESSEL *vessel);
	~ScriptMFD();
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool ConsumeKeyBuffered (uint32_t key);
	bool ConsumeButton (int bt, int event);
	void Update (HDC hDC);
	bool Input (const char *line);
	void QueryCommand ();
	void CreateInterpreter ();
	void DeleteInterpreter ();
	void SetPage (uint32_t newpg);
	static bool ScriptInput (void *id, char *str, void *data);
	static void* MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	void SetFontSize (double size);

private:
	InterpreterList::VesselInterp *vi;
	OBJHANDLE hVessel;      // vessel object handle
	HANDLE interpTh;        // interpreter thread handle
	HFONT hFont;            // font handle
	uint32_t pg;               // current page   
	uint32_t fw, fh;           // character width, height
	uint32_t nchar;            // characters per line displayed
	uint32_t nline;            // max number of lines displayed
};

#endif // !__LUAMFD_H