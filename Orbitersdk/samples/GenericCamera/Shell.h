// =================================================================================================================================
//
// Copyright (C) 2009 - 2018 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense
// copies of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) You do not remove or alter any copyright notices contained within the Software.
// d) This copyright notice must be included in all copies or substantial portions of the Software.
//
// If the Software is distributed in an object code form then in addition to conditions above:
// e) It must inform that the source code is available and how to obtain it.
// f) It must display "NO WARRANTY" and "DISCLAIMER OF LIABILITY" statements on behalf of all contributors like the one below.
//
// The accompanying materials such as artwork, if any, are provided under the terms of this license unless otherwise noted. 
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


// =================================================================================
//	ShellMFD class to provide 256 vessel specific instances of true MFD.
//	Only the shell class is deleted	when switching between panels and vessels
//	the true MFD remains unchanged.
// =================================================================================

#ifndef __SHELLMFD_H
#define __SHELLMFD_H

// ========================================================================
// Define class name of the true MFD
//
#define TRUE_MFD CameraMFD
class TRUE_MFD;

// ========================================================================

#include "OrbiterSDK.h"
#include "DrawAPI.h"

using namespace oapi;

struct mfd_list {
	VESSEL *	hVessel;
	UINT		idMFD;
	TRUE_MFD *	hTrue;
};
 
extern struct mfd_list *MFDList;
extern int mfdLast;
extern bool bExiting;

class ShellMFD: public MFD2 
{

public:
	
				ShellMFD (DWORD w, DWORD h, VESSEL *vessel, TRUE_MFD *TrueMFD, UINT id);
				~ShellMFD ();

	bool		ConsumeKeyImmediate(char *key);
	bool		ConsumeKeyBuffered (DWORD key);
	bool		ConsumeButton (int bt, int event);
	char *		ButtonLabel (int bt);
	int			ButtonMenu (const MFDBUTTONMENU **menu) const;
	void		Update (HDC hDC);
	bool		Update (Sketchpad *pSkp);
	void		WriteStatus (FILEHANDLE scn) const;
	void		ReadStatus (FILEHANDLE scn);
	void		StoreStatus (void) const;
	void		RecallStatus (void);	

	UINT		GetMFDId(void) const { return id; }
	TRUE_MFD *  GetTrueMFD(void) const { return hTrue; }

	// Call these functions from global InitModule and ExitModule callbacks for initialization and cleanup.
	// Like:
	// 
	// DLLCLBK void ExitModule(HINSTANCE hDLL)
	// {
	//		ShellMFD::ExitModule(hDLL);
	// }
	//
	static void	InitModule(HINSTANCE hDLL);
	static void ExitModule(HINSTANCE hDLL);
	static LONG_PTR MsgProc(UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

private:
	UINT id;			// Logical MFD identifier MFD_LEFT, MFD_RIGHT...
	TRUE_MFD *hTrue;	// Pointer to true MFD
};

#endif 