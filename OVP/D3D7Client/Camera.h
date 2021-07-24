// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// --------------------------------------------------------------
// Camera.h
// Class Camera (interface)
//
// The camera defines the observer position in the 3D world.
// Each scene consists of a camera and a collection of vObjects
// within visual range around it.
// The "render space" (i.e. the coordinate system in which the
// camera and visual objects live) is translated against the
// "global frame" in which orbiter's logical objects live, such
// that the camera is always at the origin. Global and render
// space have however the same orientation.
// --------------------------------------------------------------

#ifndef __CAMERA_H
#define __CAMERA_H

#include "Scene.h"

class Camera {
	friend class Scene;

public:
	Camera (LPDIRECT3DDEVICE7 _dev, DWORD w, DWORD h);
	void Update ();

	inline const D3DMATRIX *GetViewMatrix () const { return &mView; }
	inline const D3DMATRIX *GetProjectionMatrix () const { return &mProj; }
	const D3DMATRIX *GetProjectionViewMatrix () const;

	MATRIX4 ProjectionMatrix() const;
	MATRIX4 ViewMatrix() const;

	inline DWORD GetViewportW () const { return viewW; }
	inline DWORD GetViewportH () const { return viewH; }

	inline const VECTOR3 *GetGPos () const { return &gpos; }
	inline const VECTOR3 *GetGDir () const { return &gdir; }
	inline const MATRIX3 *GetGRot () const { return &grot; }

	bool Direction2Viewport(const VECTOR3 &dir, int &x, int &y);

	inline double GetTanAp () const { return tan_ap; }
	inline double GetNearlimit () const { return nearplane; }
	inline double GetFarlimit () const { return farplane; }

	inline OBJHANDLE GetProxyBody () const { return hObj_proxy; }

	void SetFrustumLimits (double nearlimit, double farlimit);

protected:
	void SetAperture (double _ap);
	void UpdateProjectionMatrix ();

private:
	LPDIRECT3DDEVICE7 dev;
	DWORD viewW, viewH;

	// camera status parameters
	VECTOR3 gpos;           // current camera position (global frame)
	VECTOR3 gdir;           // current camera direction (global frame)
	MATRIX3 grot;           // current camera rotation matrix (global frame)

	// camera fustrum parameters
	double ap;              // aperture [rad]
	double tan_ap;          // tan(aperture)
	double aspect;          // aspect ratio
	float nearplane;        // fustrum nearplane distance
	float farplane;         // fustrum farplane distance

	D3DMATRIX mView;        // D3D view matrix for current camera state
	D3DMATRIX mProj;        // D3D projection matrix for current camera state
	mutable D3DMATRIX mProjView;  // product of projection and view matrix
	mutable bool bProjView_valid; // flag for valid mProjView

	OBJHANDLE hObj_proxy;   // closest celestial body
	double alt_proxy;       // camera distance to surface of hObj_proxy
};

#endif // !__CAMERA_H