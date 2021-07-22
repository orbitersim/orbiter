// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class Camera
// Contains information about current observer position and direction,
// aperture, etc.
//
// Note that in (visual) world coordinates the camera is always in the
// origin to avoid float precision problems (DirectX uses single precision)
// Therefore the camera (logical) global coordinates define the transform
// from the logical to the visual coordinate system:
// p(visual) = p(logical)-p_camera(logical)

#ifndef __CAMERA_H
#define __CAMERA_H

#include <d3d.h>
#include <fstream>
#include "Vecmat.h"
#include "elevmgr.h"
#include "CamAPI.h"

class Body;
class Planet;
class VObject;
class CameraMode;
#ifdef NETCONNECT
class OrbiterConnect;
#endif

typedef enum {
	CAMERA_TARGETRELATIVE,
	CAMERA_ABSDIRECTION,
	CAMERA_GLOBALFRAME,
	CAMERA_TARGETTOOBJECT,
	CAMERA_TARGETFROMOBJECT,
	CAMERA_GROUNDOBSERVER
} ExtCamMode;

typedef enum {
	CAMERA_GENERICCOCKPIT,
	CAMERA_2DPANELCOCKPIT,
	CAMERA_VIRTUALCOCKPIT
} IntCamMode;

typedef enum {
	CAMERA_NORMAL,
	CAMERA_RECENTER,
	CAMERA_RECENTER_CONT,
	CAMERA_MANUAL
} CamAction;

class Camera {
	friend class Orbiter;

public:
	Camera (double _nearplane = 1.0, double _farplane = 1e8);
	// Create a camera with given y-aperture [rad] and viewing fustrum limits
	
	~Camera();

	void SetCMode (const CameraMode *cm);
	CameraMode *GetCMode () const;
	// Set/get camera mode from parameters in cm

	void Attach (Body *_target, int mode);
	// Attach camera to body "target" either as an external or
	// internal camera. mode=0:internal, 1:external, 2:don't change
	// (external is forced if target type != OBJTP_VESSEL)

	inline Body *Target () { return target; }
	// Return current camera target

	inline bool IsExternal () const { return external_view; }
	inline bool IsInternal () const { return !external_view; }
	
	bool IsCockpitForward () const;
	// Return true if we are in cockpit mode and the camera points forward (+z)

	inline bool IsCockpitDefaultDir () const
	{ return (!external_view && cphi == 0 && ctheta == 0); }
	// Return true if we are in cockpit mode and the camera points in the default direction

	inline const Vector *GPosPtr () const { return &gpos; }
	inline const VECTOR3 *GPOSPtr () const { return &GPOS; }
	// reference to global camera position

	inline const Vector *GSPosPtr () const { return &gspos; }
	// reference to target-relative camera position in global orientation

	inline Vector CockpitPos () const { return *rofs + rpos + eyeofs; }
	// camera position inside cockpit, including eye-rotation offset
	// only for internal modes

	inline Vector Direction() const
	{ return Vector (grot.m13, grot.m23, grot.m33); }
	// Return direction (in global coords) in which the camera is looking
	// (last column of GRot)

	bool Direction2Viewport(const Vector &dir, int &x, int &y);

	double Distance() const;
	double Phi() const;
	double Theta() const;
	// camera distance and rotation angles from target in external view

	inline double Apprad (double dist, double rad) const
	{ return (h05*rad)/(dist*tan_ap); }
	// Return apparent radius [pixels] of an object of radius rad
	// at distance dist

	inline double Scale () const
	{ return h05/tan_ap; }

	inline double Apprad_dist (double apprad, double rad) const
	{ return (h05*rad)/(apprad*tan_ap); }
	// Return the distance at which an object of radius rad will
	// appear at an apparent radius of apprad [pixels]

	//void SetPhi (double _phi)
	//{ if (external_view) SetRelPos (dist, _phi, theta); }
	void AddPhi (double dphi);
	// Set or increment azimuth angle of camera position rel. to ref. object
	// phi=0: camera behind object
	// Ignored if camera is internal or target == 0

	//void SetTheta (double _theta)
	//{ if (external_view) SetRelPos (dist, phi, _theta); }
	void AddTheta (double dtheta);
	// Set or increment polar angle of camera position rel. to ref. object
	// theta=0: camera level with object
	// Ignored if camera is internal or target == 0

	void Rotate (double dphi, double dtheta, bool smooth = false);
	// "Rotate" camera direction. This is currently only used to
	// change the view direction in free CAMERA_GROUNDOBSERVER mode, but
	// may be extended to perform actions in other modes

	void MoveTo (const Vector &p);
	// Slew camera position to p. Only used in cockpit view
	// (coordinates are in local vessel coordinates)

	void MoveToDirect (const Vector &p);
	// Set camera position to p (no transition). Only used in cockpit view
	// (coordinates are in local vessel coordinates)

	void ShiftPhi (double shift);
	void ShiftTheta (double shift);
	void ShiftDist (double shift);

	void ChangeDist (double fact);
	// Change the distance to the object by factor "fact"
	// (External views only)

	void SetDefaultCockpitDir (const Vector &dir, double tilt = 0.0);
	// set the default camera direction for cockpit modes
	// 'dir' must be normalised to 1

	void SetCockpitDir (double ph, double th);
	// Set camera direction in (internal) cockpit mode
	// ph and th are relative to default view direction

	void ResetCockpitPos ();
	// Reset cockpit camera to default position

	void ResetCockpitDir (bool smooth=true);
	void ResetCockpitDir (double ph, double th, bool smooth=true);
	// Reset cockpit camera to current default or specified direction

	void SetCatchAngle (double cangle);
	// Set the angle [rad] over which the camera auto-centers to its default direction

	void Drag (const Vector &gshift);
	// Displace camera by 'gshift' (global coords) from its
	// 'natural' position. The camera will automatically 
	// gradually move back (external camera mode only)

	void GroundObserverShift (double dx, double dz, double dh);
	// Move camera position on a planet surface
	// Only used if mode is CAMERA_GROUNDOBSERVER

	void GroundObserverTilt (double dphi, double dtht);
	// Rotate camera direction in free CAMERA_GROUNDOBSERVER mode
	// dphi and dtht are incremental tilts in local horizon coordinates

	inline const Vector &GPos() const { return gpos; }
	inline const Matrix &GRot() const { return grot; }

	double GroundAltitude() const { return go.alt; }

	inline double TanAperture() const { return tan_ap; }
	double Aperture() const { return *ap; }
	double SetAperture (double _ap, bool limit_range = true, bool force = false);
	// Read/set camera aperture [rad]
	// Return value of Set Aperture is actual setting of aperture
	// (different from input argument if outside limits)

	inline double IncrAperture (double dap)
	{ return SetAperture (*ap + dap); }
	// Zoom in or out by changing field of view (FOV) by 'dap' rad
	// returns resulting FOV (rad)

	inline double Nearplane() const { return nearplane; }
	inline double Farplane() const { return farplane; }
	inline float HomogZlimit () const { return homog_zlimit; }
	void SetFrustumLimits (double _nearplane, double _farplane);
	// Get/Reset near and far plane distances of the viewing frustum

	void ResizeViewport (int w, int h);
	// Called whenever the render viewport is resized

	struct GroundObserverSite {
		char planet[64];
		char site[64];
		char addr[64];
	} gos;

	int GetMode () const;
	inline ExtCamMode GetExtMode () const { return extmode; }
	inline IntCamMode GetIntMode () const { return intmode; }
	inline const Body *GetDirRef () const { return dirref; }

	void SetIntMode (IntCamMode mode);
	void SetTrackMode (ExtCamMode mode, const Body *ref = 0);
	void SetGroundMode (ExtCamMode mode, const Body *ref, double lng, double lat, double alt,
		const GroundObserverSite *_gos = 0, bool alt_above_ground = true);
	const GroundObserverSite *GetGroundObserverSite() const { return &gos; }
	// get/set camera tracking mode for external views

	double GroundObserver_PanSpeed() const { return go.panspeed; }
	void SetGroundObserver_PanSpeed (double speed);
	bool GroundObserver_TargetLock() const { return go.tgtlock; }
	void SetGroundObserver_TargetLock (bool lock);
	double GroundObserver_TerrainLimit() const { return go.terrain_limit; }
	void SetGroundObserver_TerrainLimit (double alt);
	void OutputGroundObserverParams () const;

	bool ProcessMouse (UINT event, DWORD state, DWORD x, DWORD y, const char *kstate);
	void UpdateMouse ();

	void ClearPresets ();
	// clear list of preset modes

	DWORD nPreset() const { return npreset; }
	// number of preset modes

	DWORD AddPreset (CameraMode *mode = 0);
	// add entry to list (if mode==0, add current active camera mode)
	// return value is index of mode in the list

	bool DelPreset (DWORD idx);
	// removes a preset mode from the list

	void RecallPreset (DWORD idx);
	// set camera mode to a preset mode from the list

	CameraMode *GetPreset (DWORD idx);
	// retrieve a camera mode from the preset list

	void Update ();
	// Propagate camera state from S0 to S1. Note: This is called after world state has
	// been propagated to S1, but before S1 is copied back to S0.

	void SendDlgMessage (int msgid, void *msg) const;
	// Notify Camera dialog box of a change

	//inline D3DMATRIX *D3D_ViewMatrix ()
	//{ return &view_mat; }

	MATRIX4 ProjectionMatrix() const;
	// Return projection matrix in full resolution

	MATRIX4 ViewMatrix() const;
	// Return view matrix in full resolution

	D3DMATRIX *D3D_ProjViewMatrix ();
	// Return product ProjectionMatrix * ViewMatrix

	void ViewportToGlobalDir (double sx, double sy, Vector &gdir) const;
	// converts viewport coordinates (pixels) into a direction vector
	// in the global frame

	inline const Planet *ProxyPlanet () const { return planet_proxy; }
	inline double ProxyAlt () const { return alt_proxy; }

	void InitState (const char *scn, Body *default_target);
	// init camera state from scenario scn

	bool Read (std::ifstream &ifs);
	void Write (std::ostream &ofs) const;
	// read/write camera status from/to stream

	// Interface for external camera control
	bool RegisterExternalControl (ExternalCameraControl *ecc);
	bool UnregisterExternalControl ();

private:
	void SetViewInternal ();
	void SetViewExternal ();

	void SetRelPos (double r, double ph, double th);
	// Set camera position rel. to ref. object, where r, ph, th are polar coords

	double GroundElevation (const Planet *ref, double lng, double lat, double alt) const;
	// ground elevation at specified point

	void UpdateProjectionMatrix ();
	// recalculate proj_mat

	void StoreVCParams();
	bool RecallVCParams();

	DWORD UpdateExternalControl (ExternalCameraControl *ecc);
	// update camera from an external control source

	ExtCamMode extmode;    // external camara mode
	IntCamMode intmode;    // internal camera mode
	CamAction action;      // current camera auto-action
	DWORD ExtCtrlMode;     // if camera is externally controlled, this contains bitflags for data types
	                       // (see CAMDATA_xxx constants in CamAPI.h)

	POINT pm;              // last cursor position
	double mmoveT;         // time of last mouse move

	Vector gpos;           // current camera pos in global coords
	VECTOR3 GPOS;          // gpos in VECTOR3 format
	Vector gspos;          // current camera pos relative to target, in global orientation
	Matrix grot;           // current camera rotation in global coords
	Vector *rofs;          // cockpit position offset
	Vector  rpos;          // current camera pos in target system
	Matrix  rrot0;         // camera rotation of default cockpit direction
	Matrix  rrot;          // current camera rotation in target system (rel to default direction in cockpit modes)
	bool isStdDir;         // true if default cockpit camera direction is +z
	Vector  eyeofs0, eyeofs;// offset between eyes and head rotation point (standard and rotated coords)
	Vector  tref;          // camera target point offset from target origin (in target coords)
	double tref_t0, tref_d0, tref_r0, tref_r1; // auxiliary variables for camera dragging
	bool has_tref, rshift; // use tref?

	Vector  gdir;          // camera direction in global coords (for 'absolute direction' modes)
	Body   *target;        // Reference body. Relative coordinates (RPos) refer to this.
	const Body *dirref;    // body used as direction reference for CAMERA_TARGETTOOBJECT and
						   // CAMERA_TARGETFROMOBJECT external modes
	double  rdist;         // distance of camera from target in units of body size
	                       // (only for external views)
	const Planet *planet_proxy; // closest planet
	double alt_proxy;      // camera altitude and relative altitude over planet
	double ephi, etheta;   // azimuth and polar angles of camera rel to target (external view)
	double *ap;            // aperture (semi-field-of-view) in y
	double ap_int, ap_ext; // aperture for internal and external views
	double tan_ap;         // tangens of aperture
	double aspect;         // height/width of viewport
	double w05, h05;       // semi-width,height of viewport in pixels
	float  farplane;       // far limit of viewing fustrum
	float  nearplane;      // near plane of viewing fustrum
	float  homog_zlimit;   // fustrum z-limit for homogeneous coords
	bool external_view;    // flag for external camera view
	double dphi, dtht;     // cockpit camera rotation request
	double vphi, vtht;     // camera rotation speed (used only by cockpit camera)

	// cockpit camera parameters
	struct {
		Body *target;      // target vessel
		Vector rpos;       // camera position offset
		double cphi,ctheta;// camera azimuth and polar angles
	} cockpitprm;
	double cphi, ctheta;   // current azimuth and polar angles of cockpit camera direction
	double cphi0, ctheta0; // default cockpit camera direction
	double cntr_phi, cntr_tht;
	double catchangle;     // off-center angle for camera auto-center

	bool rot_smooth;       // flag for smooth rotation (acceleration/deceleration)
	bool movehead;         // move head in cockpit mode
	Vector tgtp;           // cockpit camera target position (to implement 'leaning')

	struct GroundObserver {// ground observer camera parameters
		double lng, lat;       // position
		double alt;            // altitude above terrain (if below threshold)
		double alt0;           // target altitude above mean radius (if above threshold)
		double terrain_limit;  // altitude limit for terrain-following mode
		double phi, tht;	   // camera direction in local horizon frame (for free mode)
		Matrix R;              // rotation matrix local horizon at ground observer pos -> local planet
		double panspeed;       // speed at which the ground observer moves with arrow keys (m/s)
		bool tgtlock;          // flag for ground observer locked to target
	} go;

	CameraMode **preset;           // list of preset camera modes
	DWORD npreset;                 // list length

	// mouse parameters
	bool mbdown[2];         // mouse buttons down?
	int mx, my;             // mouse position

	D3DMATRIX view_mat;     // D3D view matrix for current camera state
	D3DMATRIX proj_mat;     // D3D projection matrix for current camera state
	D3DMATRIX pv_mat;       // projection * view matrix
	bool pv_mat_valid;      // flag for validity of pv_mat

	mutable std::vector<ElevationTile> etile;

	ExternalCameraControl *ECC;

#ifdef NETCONNECT
public:
	bool Send (OrbiterConnect *oc);
	bool Recv (OrbiterConnect *oc);
	void InitState (OrbiterConnect *oc);
#endif // NETCONNECT
};

#endif // !__CAMERA_H