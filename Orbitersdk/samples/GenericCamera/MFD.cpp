// =================================================================================================================================
//
// Copyright (C) 2018 Jarmo Nikkanen
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

#define STRICT
#define ORBITER_MODULE

#include "windows.h"
#include "orbitersdk.h"
#include "MFD.h"
#include "gcConst.h"
#include "Sketchpad2.h"
#include "Shell.h"


#define ENABLE_OVERLAY false

// ============================================================================================================
// Global variables

int g_MFDmode; // identifier for new MFD mode


class GenericModule : public oapi::Module
{

public:

	GenericModule(HINSTANCE hInst) : Module(hInst) {	}

	~GenericModule() {	}

	void clbkSimulationStart(RenderMode rm) {	}

	void clbkSimulationEnd() {	}

	void clbkFocusChanged(OBJHANDLE hNew, OBJHANDLE hOld)
	{
	
		VESSEL *o = NULL;
		if (hOld) o = oapiGetVesselInterface(hOld);

		for (int i = 0; i < mfdLast; i++) {
			if (MFDList[i].hTrue) {
				if (o) if (MFDList[i].hVessel == o) MFDList[i].hTrue->FocusChanged(false);
			}
		}
	}
};


// ============================================================================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	GenericModule *pFly = new GenericModule(hDLL);
	oapiRegisterModule(pFly);

	ShellMFD::InitModule(hDLL);

	static char *name = "Generic Camera";   // MFD mode name
	MFDMODESPECEX spec;
	spec.name = name;
	spec.key = OAPI_KEY_C;                // MFD mode selection key
	spec.context = NULL;
	spec.msgproc = ShellMFD::MsgProc;  // MFD mode callback function

	// Register the new MFD mode with Orbiter
	g_MFDmode = oapiRegisterMFDMode (spec);
}

// ============================================================================================================
//
DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the custom MFD mode when the module is unloaded
	oapiUnregisterMFDMode (g_MFDmode);
	ShellMFD::ExitModule(hDLL);
}











// ============================================================================================================
// MFD class implementation

CameraMFD::CameraMFD(DWORD w, DWORD h, VESSEL *vessel)
	: MFD2 (w, h, vessel)
	, hVessel(vessel)
	, hFocus(vessel)
	, hRenderSrf(NULL)
	, hTexture(NULL)
	, hCamera(NULL)
	, hDock(NULL)
	, hAttach(NULL)
	, pMask(NULL)
	, type(Type::Dock)
	, index(0)
	, bParent(false)
	, bNightVis(false)
	, bCross(true)
	, fov(30.0)	  // fov (i.e. Aparture) which is 1/2 of the vertical fov see oapiCameraAperture()
	, offset(-0.2)
	, pCore(gcGetCoreInterface())
{

	font = oapiCreateFont (w/20, true, "Arial", (FontStyle)(FONT_BOLD | FONT_ITALIC), 450);
	
	hTexture = oapiLoadTexture("samples/Crosshairs.dds");

	// Create 3D render target
	hRenderSrf = oapiCreateSurfaceEx(w, h, OAPISURFACE_RENDER3D | OAPISURFACE_TEXTURE |
		                                    OAPISURFACE_RENDERTARGET | OAPISURFACE_NOMIPMAPS);

	// Clear the surface
	oapiClearSurface(hRenderSrf);	

	// Register camera overlay render proc to draw into the actual rendering surface
	if (ENABLE_OVERLAY)	pCore->RegisterRenderProc(CameraMFD::DrawOverlay, RENDERPROC_CUSTOMCAM_OVERLAY, this);
	

	SelectVessel(hVessel, Type::Dock);
}


// ============================================================================================================
//
CameraMFD::~CameraMFD()
{
	oapiReleaseFont(font);

	// Attention, Always delete the camera before the surface !!!
	if (hCamera) pCore->DeleteCustomCamera(hCamera);
	if (hRenderSrf) oapiDestroySurface(hRenderSrf);
	if (hTexture) oapiReleaseTexture(hTexture);
}


// ============================================================================================================
//
void CameraMFD::FocusChanged(bool bGained)
{
	//if (bGained) oapiWriteLogV("Starting custom camera for vessel 0x%X", pV);
	//else oapiWriteLogV("Shutting down custom camera for vessel 0x%X", pV);

	// Always delete the camera first
	if (hCamera) pCore->DeleteCustomCamera(hCamera);
	if (hRenderSrf) oapiDestroySurface(hRenderSrf);

	hCamera = NULL;
	hRenderSrf = NULL;

	if (bGained) {

		// Create 3D render target
		hRenderSrf = oapiCreateSurfaceEx(W, H, OAPISURFACE_RENDER3D | OAPISURFACE_TEXTURE |
			                                    OAPISURFACE_RENDERTARGET | OAPISURFACE_NOMIPMAPS);

		// Clear the surface
		oapiClearSurface(hRenderSrf);

		SelectVessel(hVessel, Type::Dock);
	}
}


// ============================================================================================================
//
void CameraMFD::UpdateDimensions(DWORD w, DWORD h)
{
	W = w; H = h;
	FocusChanged(true);	
}


// ============================================================================================================
//
void CameraMFD::SelectVessel(VESSEL *hVes, Type _type)
{
	VECTOR3 pos, dir, rot;


	pos = _V(0, 0, 0);
	dir = _V(1, 0, 0);
	rot = _V(0, 1, 0);

	type = _type;

	if (hVes != hVessel) {
		// New Vessel Selected
		offset = -0.2;
		index = 0;
		type = Type::Dock;
	}

	hVessel = hVes;



	int nDock = hVessel->DockCount();
	int nAtch = hVessel->AttachmentCount(bParent);

	if (nDock == 0 && nAtch == 0) return;

	if (type == Type::Atch && nAtch == 0) type = Type::Dock;
	if (type == Type::Dock && nDock == 0) type = Type::Atch;

	if (fov < 5.0) fov = 5.0;
	if (fov > 70.0) fov = 70.0;

	// Attachemnts
	if (type == Type::Atch) {

		if (index < 0) index = nAtch - 1;
		if (index >= nAtch) index = 0;

		hAttach = hVessel->GetAttachmentHandle(bParent, index);

		if (hAttach) {
			hVessel->GetAttachmentParams(hAttach, pos, dir, rot);
			pos += dir * offset;
		}
		else return;
	}

	// Docking ports
	if (type == Type::Dock) {

		if (index < 0) index = nDock - 1;
		if (index >= nDock) index = 0;

		hDock = hVessel->GetDockHandle(index);

		if (hDock) {
			hVessel->GetDockParams(hDock, pos, dir, rot);
			pos += dir * offset;
		}
		else return;
	}

	// Actual rendering of the camera view into hRenderSrf will occur when the client is ready for it and 
	// a lagg of a few frames may occur depending about graphics/performance options.
	// Update will continue untill the camera is turned off via ogciCustomCameraOnOff() or deleted via ogciDeleteCustomCamera()
	// Camera orientation can be changed by calling this function again with an existing camera handle instead of NULL.

	DWORD dwFlags = 0xFF;

	if (ENABLE_OVERLAY) dwFlags |= CUSTOMCAM_OVERLAY;	// Enable overlays for this camera

	hCamera = pCore->SetupCustomCamera(hCamera, hVessel->GetHandle(), pos, dir, rot, fov*PI / 180.0, hRenderSrf, dwFlags);
}

// ============================================================================================================
//
void CameraMFD::NextAttachment()
{
	if (pMask) {
		int nAtch = hVessel->AttachmentCount(bParent);
		for (int i = 1; i < nAtch; ++i) {
			auto h = hVessel->GetAttachmentHandle(bParent, (index+i)%nAtch);
			if (h) {
				auto attachmentId = hVessel->GetAttachmentId(h);
				if (!strcmp(attachmentId, pMask)) {
					index = i;
					return;
				}
			}
		}
	}
	else index++;
}
// ============================================================================================================
//
void CameraMFD::PreviousAttachment()
{
	if (pMask) {
		int nAtch = hVessel->AttachmentCount(bParent);
		for (int i = 1; i < nAtch; ++i) {
			auto h = hVessel->GetAttachmentHandle(bParent, (index + nAtch - i) % nAtch);
			if (h) {
				auto attachmentId = hVessel->GetAttachmentId(h);
				if (!strcmp(attachmentId, pMask)) {
					index = i;
					return;
				}
			}
		}
	}
	else index--;
}

// ============================================================================================================
//
char *CameraMFD::ButtonLabel (int bt)
{
	// The labels for the two buttons used by our MFD mode
	static char *label[] = {"NA", "PA", "ND", "PD", "FWD", "BWD", "VES", "NV", "ZM+", "ZM-", "PAR", "CRS"};
	return (bt < ARRAYSIZE(label) ? label[bt] : 0);
}


// ============================================================================================================
//
int CameraMFD::ButtonMenu (const MFDBUTTONMENU **menu) const
{
	// The menu descriptions for the two buttons
	static const MFDBUTTONMENU mnu[] = {
		{ "Next attachment", 0, '1' },
		{ "Prev attachment", 0, '2' },
		{ "Next dockport", 0, '3' },
		{ "Prev dockport", 0, '4' },
		{ "Move Forward", 0, '5' },
		{ "Move Backwards", 0, '6' },
		{ "Select Vessel", 0, '7' },
		{ "Night Vision", 0, '8' },
		{ "Zoom In", 0, '9' },
		{ "Zoom Out", 0, '0' },
		{ "Parent Mode", 0, 'B' },
		{ "Crosshairs", 0, 'C' }
	};

	if (menu) *menu = mnu;

	return ARRAYSIZE(mnu); // return the number of buttons used
}


// ============================================================================================================
//
bool CameraMFD::Update(oapi::Sketchpad *skp)
{
	
	hShell->InvalidateDisplay();
	
	// Call to update attachments
	SelectVessel(hVessel, type);

	int nDock = hVessel->DockCount();
	int nAtch = hVessel->AttachmentCount(bParent);

	int tbgh = 27;		 // Text backgound height
	int edge = tbgh + 2; // Minumum spacing between cross endpoints and MFD screen edge

	RECT sr = { 0, 0, long(W - 2), long(H - 3) };
	
	skp->SetTextAlign(Sketchpad::TAlign_horizontal::CENTER);

	if (hRenderSrf) {

		oapi::Sketchpad3 *pSkp3 = (oapi::Sketchpad3 *)skp;

		if (nDock != 0 || nAtch != 0) {

			if (bNightVis) {
				pSkp3->SetBrightness(&FVECTOR4(0.0, 4.0, 0.0, 1.0));
				pSkp3->SetRenderParam(SKP3_PRM_GAMMA, &FVECTOR4(0.5f, 0.5f, 0.5f, 1.0f));
				pSkp3->SetRenderParam(SKP3_PRM_NOISE, &FVECTOR4(0.0f, 0.3f, 0.0f, 0.0f));
			}


			// Blit the camera view into the sketchpad.
			pSkp3->CopyRect(hRenderSrf, &sr, 1, 1);


			if (bNightVis) {
				pSkp3->SetBrightness(NULL);
				pSkp3->SetRenderParam(SKP3_PRM_GAMMA, NULL);
				pSkp3->SetRenderParam(SKP3_PRM_NOISE, NULL);
			}
		}

		//pSkp3->TextW(50, H / 2, L"©®ΒΓΔαβγθΣΩ™Ӕ€", -1);


		// Draw the cross-hairs
		if (bCross) {

			IVECTOR2 rc = { long(W / 2), long(H / 2) };

			int y = H / 2 - 2;
			int x = W / 2 + 1;

			int nseg = ((H / 2) - edge) / 16;	 // Number of segments
			int len = min(248, (nseg * 16) - 8); // Lenght (start and end to a white segment)

			RECT ch = { 0, 0, len, 4 };
		
			pSkp3->CopyRect(hTexture, &ch, x - len, y);
			pSkp3->CopyRect(hTexture, &ch, x, y);

			pSkp3->SetWorldTransform2D(1.0f, float(PI05), &rc, NULL);

			pSkp3->CopyRect(hTexture, &ch, x - len, y);
			pSkp3->CopyRect(hTexture, &ch, x, y);

			// Back to defaults
			pSkp3->SetWorldTransform();
		}
	}
	else {
		static char *msg = { "No Graphics API" };
		skp->Text(W / 2, H / 2, msg, lstrlen(msg));
		return true;
	}
	

	if (!hCamera) {
		static char *msg = { "Custom Cameras Disabled" };
		skp->Text(W / 2, H / 2, msg, lstrlen(msg));
		return true;
	}

	if (nDock == 0 && nAtch == 0) {
		static char *msg = { "No Dock/Attachment points" };
		skp->Text(W / 2, H / 2, msg, lstrlen(msg));
		return true;
	}


	skp->SetTextAlign();

	char text[256];
	static const char* mode[] = { "Attach(", "Dock(" };
	static const char* paci[] = { "Child", "Parent" };

	auto atchId = (type == Atch)
	            ? std::string(" [ID:") + hVessel->GetAttachmentId(hVessel->GetAttachmentHandle(bParent, index)) + "]"
	            : "";

	sprintf_s(text, 256, "Viewing %s %s%d)%s", hVessel->GetName(), mode[type], index, atchId.c_str());

	
	oapi::Sketchpad2 *pSkp2 = (oapi::Sketchpad2 *)skp;
	pSkp2->QuickBrush(0xA0000000);
	pSkp2->QuickPen(0);
	pSkp2->Rectangle(1, 1, W - 1, tbgh);
	pSkp2->Rectangle(1, H - tbgh, W - 1, H - 1);
	

	hShell->Title (skp, text);

	sprintf_s(text, 256, "[%s] FOV=%0.0f° Ofs=%2.2f[m]", paci[bParent], fov*2.0, offset);

	skp->Text(10, H - tbgh, text, lstrlen(text));
	
	return true;
}


// ============================================================================================================
//
void CameraMFD::DrawOverlay(oapi::Sketchpad *pSkp)
{
	Sketchpad3 *pSkp3 = dynamic_cast<Sketchpad3*>(pSkp);

	// Must identify the surface, no pre-filtering exists in a caller application
	// This callback function may receive "render overlay" calls not intended for this CameraMFD
	if (pSkp3->GetSurface() == hRenderSrf) {
		pSkp3->QuickPen(0xFF0000FF, 3.0f);
		pSkp3->Line(0, 0, W, H);
		pSkp3->Line(0, H, W, 0);
	}
}


// ============================================================================================================
//
bool CameraMFD::ConsumeKeyBuffered(DWORD key)
{
	
	switch (key) {


	case OAPI_KEY_1:	// Next Attachment
		NextAttachment();
		SelectVessel(hVessel, Type::Atch);
		return true;


	case OAPI_KEY_2:	// Prev Attachment
		PreviousAttachment();
		SelectVessel(hVessel, Type::Atch);
		return true;


	case OAPI_KEY_3:	// Next dock
		index++;
		SelectVessel(hVessel, Type::Dock);
		return true;


	case OAPI_KEY_4:	// Prev dock
		index--;
		SelectVessel(hVessel, Type::Dock);
		return true;


	case OAPI_KEY_5:	// Move forward
		offset += 0.1;
		SelectVessel(hVessel, type);
		return true;


	case OAPI_KEY_6:	// Move backwards
		offset -= 0.1;
		SelectVessel(hVessel, type);
		return true;


	case OAPI_KEY_7:	// Select Vessel
		oapiOpenInputBox("Keyboard Input:", DataInput, 0, 32, (void*)this);
		return true;


	case OAPI_KEY_8:	// Night vision toggle
		bNightVis = !bNightVis;
		return true;


	case OAPI_KEY_9:	// Zoom in
		fov -= 5.0;
		SelectVessel(hVessel, type);
		return true;


	case OAPI_KEY_0:	// Zoom out
		fov += 5.0;
		SelectVessel(hVessel, type);
		return true;

	case OAPI_KEY_B:	// Parent/Child
		bParent = !bParent;
		SelectVessel(hVessel, type);
		return true;

	case OAPI_KEY_C:	// Crosshairs
		bCross = !bCross;
		return true;

	}

	return false;
}


// ============================================================================================================
//
bool CameraMFD::ConsumeButton(int bt, int event)
{
	static const DWORD btkey[12] = { OAPI_KEY_1, OAPI_KEY_2, OAPI_KEY_3, OAPI_KEY_4, OAPI_KEY_5, OAPI_KEY_6,
		OAPI_KEY_7, OAPI_KEY_8, OAPI_KEY_9, OAPI_KEY_0, OAPI_KEY_B, OAPI_KEY_C };

	if (event&PANEL_MOUSE_LBDOWN) {					
		return ConsumeKeyBuffered(btkey[bt]);
	}

	return false;
}


// ============================================================================================================
//
bool CameraMFD::DataInput(void *id, char *str)
{
	OBJHANDLE hObj = oapiGetVesselByName(str);

	if (hObj) {
		SelectVessel(oapiGetVesselInterface(hObj), type);
		return true;
	}

	return false;
}


// ============================================================================================================
//
bool CameraMFD::DataInput(void *id, char *str, void *data)
{
	return ((CameraMFD*)data)->DataInput(id, str);
}

// ============================================================================================================
//
void CameraMFD::DrawOverlay(oapi::Sketchpad *pSkp, void *pParam)
{
	((CameraMFD*)pParam)->DrawOverlay(pSkp);
}

// ============================================================================================================
//
void CameraMFD::WriteStatus(FILEHANDLE scn) const
{
	if (pMask) {
		oapiWriteScenario_string(scn, "ATCH_MASK", pMask);
	}
}


// ============================================================================================================
//
void CameraMFD::ReadStatus(FILEHANDLE scn)
{
	char *line;

	char mask[256] = "";

	while (oapiReadScenario_nextline(scn, line)) {
		if (1 == sscanf_s(line, "ATCH_MASK %s", mask, (int)_countof(mask))) {

			if (pMask) delete[] pMask; // <= can this really happen?

			pMask = new char[strlen(mask) + 1];
			strcpy_s(pMask, strlen(mask) + 1, mask);

			type = Type::Atch;
			NextAttachment();
			PreviousAttachment();

			return; // we've got what we've been looking for!
		}
	}
}

