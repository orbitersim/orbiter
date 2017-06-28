// ==============================================================
//                 ORBITER MODULE: GC API Info MFD
//                  Part of the gcAPI SDK
//            Copyright (C) 2017 Peter Schneider
//                   All rights reserved
//
// GCInfoMFD.h
//
// This module demonstrates how to access the gcAPI functionality.
// The MFD code itself just shows information, but it can be used as a
// starting point for own developments.
// ==============================================================

#ifndef __GCINFOMFD_H
#define __GCINFOMFD_H

class GCInfoMFD: public MFD2 {
public:
	GCInfoMFD (DWORD w, DWORD h, VESSEL *vessel);
	~GCInfoMFD ();
	bool Update (oapi::Sketchpad *skp);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	oapi::Font *font;
private:
	struct GCINFO {
		bool  initialized;       ///< result of gcInitialize()
		bool  enabled;           ///< result of gcEnabled()
		DWORD clientId;          ///< result of gcClientID()
		int   sketchpadVersion;  /// rsult of gcSketchpadVersion()
	} info;
};

#endif // !__GCINFOMFD_H