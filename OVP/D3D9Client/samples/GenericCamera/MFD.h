// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


#ifndef __CAMERA_H
#define __CAMERA_H

#include "gcCoreAPI.h"

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
	void NextAttachment();
	void PreviousAttachment();
	void DrawOverlay(oapi::Sketchpad *pSkp);

	bool ConsumeKeyBuffered(DWORD key);
	bool ConsumeButton(int bt, int event);

	void WriteStatus(FILEHANDLE scn) const;
	void ReadStatus(FILEHANDLE scn);

	static bool DataInput(void *id, char *str, void *data);
	static void DrawOverlay(oapi::Sketchpad *pSkp, void *pParam);

	oapi::Font *		font;
	SURFHANDLE			hRenderSrf;
	SURFHANDLE			hTexture;
	DOCKHANDLE			hDock;
	ATTACHMENTHANDLE	hAttach;
	CAMERAHANDLE		hCamera;
	bool				bNightVis;
	bool				bParent;
	bool				bCross;
	Type                type;
	int					index;
	VESSEL *			hVessel;
	VESSEL *			hFocus;
	double				offset, fov;

	char *              pMask;
	gcCore *			pCore;

	class ShellMFD	*hShell;

	friend ShellMFD;
};

#endif
