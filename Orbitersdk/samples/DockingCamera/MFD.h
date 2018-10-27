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


#ifndef __CAMERA_H
#define __CAMERA_H

#include "gcAPI.h"

class CameraMFD : public MFD2 {

public:
	CameraMFD(DWORD w, DWORD h, VESSEL *vessel);
	~CameraMFD();

	void FocusChanged(bool bGained);

private:
	enum Type { Atch = 0, Dock = 1 };

	void UpdateDimensions(DWORD w, DWORD h);
	char *ButtonLabel (int bt);
	int ButtonMenu (const MFDBUTTONMENU **menu) const;
	bool Update (oapi::Sketchpad *skp);
	bool DataInput(void *id, char *str);
	void SelectVessel(VESSEL *hVes, Type type);
	
	bool ConsumeKeyBuffered(DWORD key);
	bool ConsumeButton(int bt, int event);

	void WriteStatus(FILEHANDLE scn) const;
	void ReadStatus(FILEHANDLE scn);

	static bool DataInput(void *id, char *str, void *data);
	//static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

	oapi::Font *		font;
	SURFHANDLE			hRenderSrf;
	SURFHANDLE			hTexture;
	DOCKHANDLE			hDock;
	ATTACHMENTHANDLE	hAttach;
	CAMERAHANDLE		hCamera;
	SKETCHMESH			hMesh;
	bool				bNightVis;
	bool				bParent;
	bool				bCross;
	Type                type;
	int					index;
	VESSEL *			hVessel;
	VESSEL *			hFocus;
	double				offset, fov;

	class ShellMFD	*hShell;

	friend ShellMFD;
};

#endif