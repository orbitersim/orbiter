
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


#include "mfdapi.h"
#include "Shell.h"

// Header of true MFD
//
#include "MFD.h"

struct mfd_list *MFDList = NULL;
int mfdLast = 0;
bool bExiting = false;

// =================================================================================
// Constructors and Destructors
//

ShellMFD::ShellMFD (DWORD w, DWORD h, VESSEL *vessel, TRUE_MFD *TrueMFD, UINT mfd)
: MFD2(w, h, vessel)
{
	id = mfd;
	hTrue = TrueMFD;
	hTrue->hShell = this;	// Bind true MFD into this shell
}

// ============================================================================================================
//
ShellMFD::~ShellMFD ()
{
	if (hTrue) hTrue->hShell = NULL;	// Break binding into this shell

	if (bExiting) {
		for (int i=0;i<256;i++) if (MFDList[i].hTrue) {
			delete MFDList[i].hTrue; MFDList[i].hTrue = NULL;	
		}
	}
}

// ============================================================================================================
//
void ShellMFD::InitModule(HINSTANCE hDLL)
{
	// Construct MFD List
	MFDList = new mfd_list[256]();
}

// ============================================================================================================
//
void ShellMFD::ExitModule(HINSTANCE hDLL)
{
	if (MFDList) {

		// Delete all true MFDs when exiting the module
		//
		for (int i=0;i<256;i++) {
			if (MFDList[i].hTrue) {

#ifdef NDEBUG  // Removed for DEBUG builds, as is causes access violation exceptions @ shutdown! No idea why :/
				delete MFDList[i].hTrue;
				MFDList[i].hTrue=NULL;
#endif
			}
		}

		delete[] MFDList; MFDList=NULL;
	}
}


// ============================================================================================================
//
OAPI_MSGTYPE ShellMFD::MsgProc(UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{

    if (msg==OAPI_MSG_MFD_OPENED && MFDList) {	
		
		// Search for existing true MFD ----------------------
		//
		for (int i=0;i<mfdLast;i++) {
			if (MFDList[i].hTrue) {
				if ((MFDList[i].hVessel==(VESSEL *)lparam) && (MFDList[i].idMFD==mfd)) {

					// Create new Shell MFD and bind allready existing true MFD into the shell
					// True MFD must define UpdateDimensions method in-order-to change scaling when
					// switching between cockpit modes.
					//
					MFDList[i].hTrue->UpdateDimensions(LOWORD(wparam), HIWORD(wparam));

					return (OAPI_MSGTYPE) new ShellMFD(LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam, MFDList[i].hTrue, mfd);
				}
			}
		}

		// Find empty slot and create new true MFD -----------
		//
		if (MFDList[mfdLast].hTrue) return 0;
		if (mfdLast>=254)			return 0;
		
		
		TRUE_MFD *TrMFD = new TRUE_MFD(LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam);
		
		MFDList[mfdLast].hVessel = (VESSEL *)lparam;
		MFDList[mfdLast].idMFD   = mfd;
		MFDList[mfdLast].hTrue   = TrMFD;
		
		mfdLast++;

		// Create new Shell MFD and bind true MFD into the shell
		//
		return (LONG_PTR) new ShellMFD(LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam, MFDList[mfdLast-1].hTrue, mfd);
	}

    return 0;
}


// =================================================================================
// Reroute calls to true MFD
//
bool ShellMFD::ConsumeKeyImmediate(char *key)
{
	if (hTrue) return hTrue->ConsumeKeyImmediate(key);
	return false;
}

bool ShellMFD::ConsumeKeyBuffered (DWORD key)
{
	if (hTrue) return hTrue->ConsumeKeyBuffered(key);
	return false;
}

bool ShellMFD::ConsumeButton (int bt, int event)
{
	if (hTrue) return hTrue->ConsumeButton(bt, event);
	return false;	
}

char *ShellMFD::ButtonLabel (int bt)
{
	if (hTrue) return hTrue->ButtonLabel(bt);
	return NULL;
}

int ShellMFD::ButtonMenu (const MFDBUTTONMENU **menu) const
{
	if (hTrue) return hTrue->ButtonMenu(menu);
	return 0;
}

bool ShellMFD::Update(Sketchpad *pSkp)
{
	if (hTrue) return hTrue->Update(pSkp);
	return true;
}

void ShellMFD::Update (HDC hDC)
{
		
}

void ShellMFD::WriteStatus (FILEHANDLE scn) const
{
	bExiting = true;
	if (hTrue) hTrue->WriteStatus(scn);	
}

void ShellMFD::ReadStatus (FILEHANDLE scn)
{
	if (hTrue) hTrue->ReadStatus(scn);	  
}

void ShellMFD::StoreStatus (void) const
{	
}

void ShellMFD::RecallStatus (void)
{
}