// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


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
	static OAPI_MSGTYPE MsgProc(UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

private:
	UINT id;			// Logical MFD identifier MFD_LEFT, MFD_RIGHT...
	TRUE_MFD *hTrue;	// Pointer to true MFD
};

#endif 
