// =================================================================================================================================
//
// Copyright (C) 2016 Jarmo Nikkanen
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


#ifndef __CAMERA_H
#define __CAMERA_H

#include "gcAPI.h"

class CameraMFD : public MFD2 {
public:
	CameraMFD(DWORD w, DWORD h, VESSEL *vessel);
	~CameraMFD();
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool Update (oapi::Sketchpad *skp);
	bool DataInput(void *id, char *str);
	void SelectVessel(VESSEL *hVes, int type);
	
	bool ConsumeKeyBuffered(DWORD key);
	bool ConsumeButton(int bt, int event);
	
	static bool DataInput(void *id, char *str, void *data);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

protected:
	oapi::Font *		font;
	SURFHANDLE			hRenderSrf;
	SURFHANDLE			hTexture;
	DOCKHANDLE			hDock;
	ATTACHMENTHANDLE	hAttach;
	CAMERAHANDLE		hCamera;
	SKETCHMESH			hMesh;
	bool				bNightVis;
	bool				bParent;
	int					index, type;
	VESSEL *			hVessel;
	VESSEL *			hFocus;
	double				offset, fov;
};

#endif