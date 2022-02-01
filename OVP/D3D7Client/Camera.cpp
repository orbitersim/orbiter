// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Camera.cpp
// Class Camera (implementation)
//
// The camera defines the observer position in the 3D world.
// Each scene consists of a camera and a collection of vObjects
// within visual range around it.
// The "render space" (i.e. the coordinate system in which the
// camera and visual objects live) is translated against the
// "global frame" in which orbiter's logical objects live, such
// that the camera is always at the origin. Global and render
// space have however the same orientation.
// ==============================================================

#include "Camera.h"
#include "OrbiterAPI.h"
#include "D3D7Util.h"

Camera::Camera (LPDIRECT3DDEVICE7 _dev, DWORD w, DWORD h)
{
	dev = _dev;
	viewW = w, viewH = h;
	aspect = (double)h/(double)w;
	SetAperture (RAD*50.0);
	hObj_proxy = 0;

	D3DMAT_Identity (&mView);
	bProjView_valid = false;
	SetFrustumLimits (2.5f, 5e6f); // initial limits
}

MATRIX4 Camera::ProjectionMatrix() const
{
	MATRIX4 mat = {aspect/tan_ap,0,0,0,
		           0,1.0/tan_ap,0,0,
				   0,0,farplane/(farplane-nearplane),1.0,
				   0,0,-nearplane*farplane/(farplane-nearplane),0};
	return mat;
}

MATRIX4 Camera::ViewMatrix() const
{
	MATRIX4 mat = {grot.m11,grot.m12,grot.m13,0,
		           grot.m21,grot.m22,grot.m23,0,
				   grot.m31,grot.m32,grot.m33,0,
				   0,0,0,1};
	return mat;
}

const D3DMATRIX *Camera::GetProjectionViewMatrix () const
{
	if (!bProjView_valid) {
		D3DMAT_MatrixMultiply (&mProjView, &mProj, &mView);
		bProjView_valid = true;
	}
	return &mProjView;
}

void Camera::UpdateProjectionMatrix ()
{
	ZeroMemory (&mProj, sizeof(D3DMATRIX));
	mProj._11 = (float)(aspect / tan_ap);
	mProj._22 = (float)(1.0    / tan_ap);
	mProj._43 = (mProj._33 = farplane / (farplane-nearplane)) * (-nearplane);
	mProj._34 = 1.0f;

	// register new projection matrix
	dev->SetTransform (D3DTRANSFORMSTATE_PROJECTION, &mProj);
}

void Camera::SetAperture (double _ap)
{
	ap = _ap;
	tan_ap = tan (ap);
	UpdateProjectionMatrix ();
}

void Camera::SetFrustumLimits (double nearlimit, double farlimit)
{
	nearplane = (float)nearlimit;
	farplane = (float)farlimit;
	UpdateProjectionMatrix ();
}

bool Camera::Direction2Viewport(const VECTOR3 &dir, int &x, int &y)
{
	D3DVECTOR homog;
	D3DVECTOR idir = {(D3DVALUE)-dir.x, (D3DVALUE)-dir.y, (D3DVALUE)-dir.z};
	D3DMAT_VectorMatrixMultiply (&homog, &idir, &mProjView);
	if (homog.x >= -1.0f && homog.y <= 1.0f && homog.z >= 0.0) {
		if (_hypot(homog.x, homog.y) < 1e-6) {
			x = viewW/2, y = viewH/2;
		} else {
			x = (int)(viewW*0.5f*(1.0f+homog.x));
			y = (int)(viewH*0.5f*(1.0f-homog.y));
		}
		return true;
	} else
		return false;
}

void Camera::Update ()
{
	if (ap != oapiCameraAperture()) // check aperture
		SetAperture (oapiCameraAperture());

	oapiCameraGlobalPos (&gpos);
	oapiCameraGlobalDir (&gdir);
	oapiCameraRotationMatrix (&grot);
	D3DMAT_SetRotation (&mView, &grot);
	dev->SetTransform (D3DTRANSFORMSTATE_VIEW, &mView);
	bProjView_valid = false;

	// note: in render space, the camera is always placed at the origin,
	// so that render coordinates are precise in the vicinity of the
	// observer (before they are translated into D3D single-precision
	// format). However, the orientation of the render space is the same
	// as orbiter's global coordinate system. Therefore there is a
	// translational transformation between orbiter global coordinates
	// and render coordinates.

	// find the planet closest to the current camera position
	double d, r, alt, ralt, ralt_proxy = 1e100;
	int i, n = oapiGetGbodyCount();
	VECTOR3 ppos;
	for (i = 0; i < n; i++) {
		OBJHANDLE hObj = oapiGetGbodyByIndex (i);
		oapiGetGlobalPos (hObj, &ppos);
		r = oapiGetSize(hObj);
		d = dist(gpos, ppos);
		alt = d - r;
		ralt = alt/r;
		if (ralt < ralt_proxy) {
			ralt_proxy = ralt;
			alt_proxy = alt;
			hObj_proxy = hObj;
		}
	}
}
