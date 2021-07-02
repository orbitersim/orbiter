// ==============================================================
//                 ORBITER MODULE: DialogTemplate
//                  Part of the ORBITER SDK
//            Copyright (C) 2003 Martin Schweiger
//                   All rights reserved
//
// MFDTemplate.h
//
// This module demonstrates how to build an Orbiter plugin which
// inserts a new MFD (multi-functional display) mode. The code
// is not very useful in itself, but it can be used as a starting
// point for your own MFD developments.
// ==============================================================

#ifndef __MFDTEMPLATE_H
#define __MFDTEMPLATE_H

class MFDTemplate: public MFD2 {
public:
	MFDTemplate (DWORD w, DWORD h, VESSEL *vessel);
	~MFDTemplate ();
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool Update (oapi::Sketchpad *skp);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	oapi::Font *font;
};

#endif // !__MFDTEMPLATE_H