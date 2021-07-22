// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION
#include "CamAPI.h"
#include "Camera.h"

extern Camera *g_camera;

// ======================================================================
// class ExternalCameraControl

ExternalCameraControl::ExternalCameraControl (DWORD dmode, DWORD cmode)
{
	SetDataMode (dmode);
	SetCameraMode (cmode);
	// set default VC parameters
	vcmode.trackrotation = true;
	vcmode.trackposition = true;
	vcmode.freezeonmouse = false;
	vcmode.rotationrange = PI;
	vcmode.positionrange = 0.5;
	vcmode.freeze_t = 1.0;
	// set default track parameters
	trkmode.trackrotation = true;
	trkmode.trackzoom = true;
	trkmode.rotationdata = TrackMode::BYROTATION;
	trkmode.deadzone = 0.2;
	trkmode.speed = 2.0;
}

void ExternalCameraControl::SetDataMode (DWORD mode)
{
	datamode = mode & 0x2F;
}

DWORD ExternalCameraControl::GetDataMode () const
{
	return datamode;
}

void ExternalCameraControl::SetCameraMode (DWORD mode)
{
	cameramode = mode & 0x1F;
}

DWORD ExternalCameraControl::GetCameraMode () const
{
	return cameramode;
}

// ======================================================================
// External control registration

DLLEXPORT bool oapiRegisterExternalCameraControl (ExternalCameraControl *ecc)
{
	return (g_camera ? g_camera->RegisterExternalControl (ecc) : false);
}

DLLEXPORT bool oapiUnregisterExternalCameraControl ()
{
	return (g_camera ? g_camera->UnregisterExternalControl () : false);
}