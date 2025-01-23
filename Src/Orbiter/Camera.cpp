// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include <stdio.h>
#include <iomanip>

#include "resource.h"
#include "D3dmath.h"
#include "Camera.h"
#include "Config.h"
#include "Body.h"
#include "Planet.h"
#include "Vessel.h"
#include "Psys.h"
#include "Pane.h"
#include "Dialogs.h"
#include "DlgMgr.h"
#include "Orbiter.h"
#include "Orbitersdk.h"
#include "Util.h"
#include "Log.h"
#include "OrbiterAPI.h"
#include "Vobject.h"
#include <zmouse.h>

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern PlanetarySystem *g_psys;
extern Pane *g_pane;
extern Vessel *g_focusobj;
extern char DBG_MSG[256];

static Vector nullvec;
static bool brot = false;
const double dragT = 3.0;
static const double vmax = 2.0;  // max rotation velocity (rad/s)
static const double amax = 8.0;  // max rotation acceleration (rad/s^2)

Camera::Camera (double _nearplane, double _farplane)
{
	extmode = CAMERA_TARGETRELATIVE;
	intmode = CAMERA_GENERICCOCKPIT;
	action  = CAMERA_NORMAL;
	gdir.Set (1,0,0);
	rofs = &nullvec;
	target = 0;
	rdist  = 4.0;
	dirref = 0;
	ephi = etheta = 0.0;
	dphi = dtht = 0.0;
	eyeofs0. Set (0,0.1,0.08);
	SetDefaultCockpitDir (Vector (0,0,1));
	SetCockpitDir (0,0);
	SetCatchAngle (RAD*5.0);
	memset (&cockpitprm, 0, sizeof(cockpitprm));
	go.panspeed = g_pOrbiter->Cfg()->CfgCameraPrm.Panspeed;
	go.tgtlock = true;
	go.terrain_limit = g_pOrbiter->Cfg()->CfgCameraPrm.TerrainLimit;
	gos.planet[0] = gos.site[0] = gos.addr[0] = '\0';
	planet_proxy = 0;
	npreset = 0;
	external_view = true;//false;
	pv_mat_valid = false;
	has_tref = false;
	movehead = false;
	ExtCtrlMode = 0;
	GetCursorPos (&pm);
	mmoveT = -1000.0;
	ap_int = ap_ext = RAD*25.0;
	ap = &ap_ext;
	SetAperture (*ap, true, true);
	nearplane = (float)_nearplane;
	farplane = (float)_farplane;
	aspect = 1.0; // arbitrary default - reset with ResizeViewport
	w05 = h05 = 100;
	mbdown[0] = mbdown[1] = false;
	VMAT_identity (view_mat);
	SetFrustumLimits (nearplane, farplane);
	ECC = 0;
	etile.resize(8);
}

Camera::~Camera ()
{
	ClearPresets();
}

bool Camera::ProcessMouse (UINT event, DWORD state, DWORD x, DWORD y, const char *kstate)
{
	if (event != WM_MOUSEWHEEL)
		mx = (int)x, my = (int)y;

	switch (event) {
	case WM_LBUTTONDOWN:
		mbdown[0] = true;
		return true;
	case WM_RBUTTONDOWN:
		mbdown[1] = true;
		g_pOrbiter->InitRotationMode();
		return true;
	case WM_LBUTTONUP:
		if (mbdown[0]) {
			mbdown[0] = false;
			return true;
		}
		break;
	case WM_RBUTTONUP:
		if (mbdown[1]) {
			mbdown[1] = false;
			g_pOrbiter->ExitRotationMode();
			return true;
		}
		break;
	case WM_MOUSEWHEEL:
		if (KEYMOD_CONTROL(kstate)) {
		}
		else {
			short zDelta = (short)HIWORD(state);
			if (external_view)
				ShiftDist(-zDelta*0.001);
			else
				g_pOrbiter->IncFOV(zDelta*(-2.0 / 120.0*RAD));
		} return true;
		break;
	}
	return false;
}

void Camera::UpdateMouse ()
{
	POINT pt;
	GetCursorPos (&pt);
	if (pt.x != pm.x || pt.y != pm.y) {
		pm.x = pt.x, pm.y = pt.y;
		mmoveT = td.SysT0;
	}

	if (mbdown[1]) {
		int dx, dy, x0, y0;
		x0 = pt.x, y0 = pt.y;
		if (!g_pOrbiter->IsFullscreen())
			ScreenToClient (g_pOrbiter->GetRenderWnd(), &pt);
		dx = pt.x - mx;
		dy = pt.y - my;
		SetCursorPos (x0-dx, y0-dy);
		if (!(dx || dy)) return;

		if (external_view) {
			if (extmode == CAMERA_GROUNDOBSERVER) {
				if (go.tgtlock) {
					GroundObserverShift (dx*go.panspeed * -0.005, dy*go.panspeed * -0.005, 0.0);
				} else
					GroundObserverTilt (dx * -0.005, dy * -0.005);
			} else {
				if (dx) ShiftPhi (dx * -0.005);
				if (dy) ShiftTheta (dy * -0.005);
			}
		} else {
			Rotate (dx * -0.001, dy * -0.001);
		}
	}
}

void Camera::SetViewInternal ()
{
	external_view = false;
	if (ap != &ap_int) {
		ap = &ap_int;
		double a = ap_int;
		ap_int = 0.0; // force update
		g_pOrbiter->SetFOV(a);
	}
}

void Camera::SetViewExternal ()
{
	external_view = true;
	if (ap != &ap_ext) {
		ap = &ap_ext;
		double a = ap_ext;
		ap_ext = 0.0; // force update
		g_pOrbiter->SetFOV(a);
	}
}

void Camera::SetIntMode (IntCamMode mode)
{
	if (mode == intmode) return; // nothing to do
	if (!external_view && intmode == CAMERA_VIRTUALCOCKPIT)
		StoreVCParams();
	intmode = mode;
	if (!external_view) {
		if (intmode != CAMERA_VIRTUALCOCKPIT || !RecallVCParams()) {
			rpos.Set (nullvec);
			SetCockpitDir (0,0);
		}
	}
}

bool Camera::IsCockpitForward () const
{
	return (!external_view && cphi == 0.0 && ctheta == 0.0);
}

void Camera::StoreVCParams()
{
	cockpitprm.target = target;
	cockpitprm.rpos   = rpos;
	cockpitprm.cphi   = cphi;
	cockpitprm.ctheta = ctheta;
}

bool Camera::RecallVCParams()
{
	if (target != cockpitprm.target) return false; // don't restore across different vessels
	rpos.Set (cockpitprm.rpos);
	SetCockpitDir (cockpitprm.cphi, cockpitprm.ctheta);
	return true;
}

void Camera::SetCMode (const CameraMode *cm)
{
	bool ext = (cm->GetMode() != CameraMode::CM_COCKPIT);
	Body *tgt = (Body*)cm->GetTarget();

	if ((tgt && tgt != target) || ext != external_view) {
		if (!tgt) tgt = target;
		Attach (tgt, ext ? 1:0);
	}
	if (tgt && !ext && tgt != g_focusobj)
		g_pOrbiter->SetFocusObject ((Vessel*)tgt);

	switch (cm->GetMode()) {
	case CameraMode::CM_COCKPIT: {
		const CameraMode_Cockpit *cmc = (const CameraMode_Cockpit*)cm;
		int pos = cmc->GetPosition();
		switch (cmc->GetCockpitMode()) {
		case CameraMode_Cockpit::CM_GENERIC:
			g_pane->SetPanelMode(1);
			break;
		case CameraMode_Cockpit::CM_PANEL2D:
			if (g_pane->SetPanelMode(2))
				if (pos >= 0) g_pane->SelectPanel(pos);
			break;
		case CameraMode_Cockpit::CM_VC:
			if (g_pane->SetPanelMode(3)) {
				if (pos >= 0) g_pane->SelectVC(pos);
				int lean = cmc->GetLeaning();
				if (lean >= 0 && lean < 4) {
					g_focusobj->LeanCamera (lean, cmc->GetLeaningSmooth());
				}
			}
			break;
		}
		} break;
	case CameraMode::CM_TRACK: {
		const CameraMode_Track *cmt = (const CameraMode_Track*)cm;
		if (cmt->GetTrackMode() != CameraMode_Track::TM_CURRENT) {
			ExtCamMode mode;
			switch (cmt->GetTrackMode()) {
			case CameraMode_Track::TM_RELATIVE:      mode = CAMERA_TARGETRELATIVE;   break;
			case CameraMode_Track::TM_ABSDIR:        mode = CAMERA_ABSDIRECTION;     break;
			case CameraMode_Track::TM_GLOBAL:        mode = CAMERA_GLOBALFRAME;      break;
			case CameraMode_Track::TM_TARGETTOREF:   mode = CAMERA_TARGETTOOBJECT;   break;
			case CameraMode_Track::TM_TARGETFROMREF: mode = CAMERA_TARGETFROMOBJECT; break;
			default: mode = CAMERA_GLOBALFRAME;
			}
			SetTrackMode (mode, (const Body*)cmt->GetRef());
		}
		double rd, ph, th;
		cmt->GetPosition (&rd, &ph, &th);
		if (rd) SetRelPos (rd, ph, th);
		} break;
	case CameraMode::CM_GROUND: {
		const CameraMode_Ground *cmg = (const CameraMode_Ground*)cm;
		const Body *ref;
		double lng, lat, alt;
		cmg->GetPosition (&lng, &lat, &alt, (OBJHANDLE*)&ref);
		if (!ref) ref = dirref;
		SetGroundMode (CAMERA_GROUNDOBSERVER, ref, lng, lat, alt, 0, cmg->GetAltMode());
		SetGroundObserver_TargetLock (cmg->GetTgtLock());
		if (!go.tgtlock) {
			double ph, th;
			cmg->GetOrientation (&ph, &th);
			go.phi = ph, go.tht = th;
		}
		} break;
	}

	double newap = cm->GetFOV()*0.5*RAD;
	if (newap && fabs(newap - *ap) > 1e-6) g_pOrbiter->SetFOV (newap);
	SendDlgMessage (3, 0);
}

CameraMode *Camera::GetCMode () const
{
	CameraMode *cm;

	if (external_view) {
		switch (extmode) {
		case CAMERA_TARGETRELATIVE: {
			cm = new CameraMode_Track; TRACENEW
			CameraMode_Track *cmt = (CameraMode_Track*)cm;
			cmt->SetTrackMode (CameraMode_Track::TM_RELATIVE);
			cmt->SetPosition (rdist, ephi, etheta);
			} break;
		case CAMERA_ABSDIRECTION: {
			cm = new CameraMode_Track; TRACENEW
			CameraMode_Track *cmt = (CameraMode_Track*)cm;
			cmt->SetTrackMode (CameraMode_Track::TM_ABSDIR);
			cmt->SetPosition (rdist, ephi, etheta);
			} break;
		case CAMERA_GLOBALFRAME: {
			cm = new CameraMode_Track; TRACENEW
			CameraMode_Track *cmt = (CameraMode_Track*)cm;
			cmt->SetTrackMode (CameraMode_Track::TM_GLOBAL);
			cmt->SetPosition (rdist, ephi, etheta);
			} break;
		case CAMERA_TARGETTOOBJECT: {
			cm = new CameraMode_Track; TRACENEW
			CameraMode_Track *cmt = (CameraMode_Track*)cm;
			cmt->SetTrackMode (CameraMode_Track::TM_TARGETTOREF, (OBJHANDLE)dirref);
			cmt->SetPosition (rdist, ephi, etheta);
			} break;
		case CAMERA_TARGETFROMOBJECT: {
			cm = new CameraMode_Track; TRACENEW
			CameraMode_Track *cmt = (CameraMode_Track*)cm;
			cmt->SetTrackMode (CameraMode_Track::TM_TARGETFROMREF, (OBJHANDLE)dirref);
			cmt->SetPosition (rdist, ephi, etheta);
			} break;
		case CAMERA_GROUNDOBSERVER: {
			cm = new CameraMode_Ground; TRACENEW
			CameraMode_Ground *cmg = (CameraMode_Ground*)cm;
			cmg->SetPosition (go.lng, go.lat, go.alt, (OBJHANDLE)dirref);
			if (!go.tgtlock) cmg->SetOrientation (go.phi, go.tht);
			} break;
		default: cm = new CameraMode_Cockpit; TRACENEW
		}
	} else {
		cm = new CameraMode_Cockpit; TRACENEW
	}
	cm->SetTarget (target);
	cm->SetFOV (2*DEG * *ap);
	return cm;
}

int Camera::GetMode () const
{
	if (external_view) {
		switch (extmode) {
		case CAMERA_TARGETRELATIVE:   return CAM_TARGETRELATIVE;
		case CAMERA_ABSDIRECTION:     return CAM_ABSDIRECTION;
		case CAMERA_GLOBALFRAME:      return CAM_GLOBALFRAME;
		case CAMERA_TARGETTOOBJECT:   return CAM_TARGETTOOBJECT;
		case CAMERA_TARGETFROMOBJECT: return CAM_TARGETFROMOBJECT;
		case CAMERA_GROUNDOBSERVER:   return CAM_GROUNDOBSERVER;
		}
	}
	return CAM_COCKPIT;
}

void Camera::Attach (Body *_target, int mode)
{
	// sanity check
	if (_target->Type() != OBJTP_VESSEL) mode = 1;

	// save current cockpit parameters for later restore
	if (!external_view && intmode == CAMERA_VIRTUALCOCKPIT)
		StoreVCParams();

	target = _target;
	if (mode != 2) external_view = (mode != 0); // otherwise don't change

	if (external_view) {
		SetViewExternal();
		SetRelPos (rdist, ephi, etheta);
		gspos.Set (mul (target->GRot(), rpos));
		gpos.Set (gspos + target->GPos());
		GPOS = MakeVECTOR3 (gpos);
		grot.Set (target->GRot());
		if (extmode == CAMERA_ABSDIRECTION)
			gdir = mul (target->GRot(), mul (rrot, Vector (0,0,1)));
		if ((extmode == CAMERA_TARGETTOOBJECT || extmode == CAMERA_TARGETFROMOBJECT)
			&& dirref == target) extmode = CAMERA_TARGETRELATIVE; // sanity check
	} else {
		SetViewInternal();
		Vessel* vessel = (Vessel*)target;
		rofs = vessel->CamPos();
		SetDefaultCockpitDir (*vessel->CamDir0(), vessel->CamTilt0());
		if (intmode != CAMERA_VIRTUALCOCKPIT || !RecallVCParams()) {
			rpos.Set (nullvec);
			SetCockpitDir (0,0);
		}
		SetCatchAngle (vessel->CamCatchRange());
		grot.Set (target->GRot() * rrot);
		gspos.Set (mul (target->GRot(), *rofs));
		gpos.Set (gspos + target->GPos());
		GPOS = MakeVECTOR3 (gpos);
	}
	tref.Set(0,0,0);
	vphi = vtht = dphi = dtht = 0.0;
	has_tref = false;
	SendDlgMessage (3, 0);
}

bool Camera::Direction2Viewport(const Vector &dir, int &x, int &y)
{
	D3DVECTOR homog;
	D3DMath_VectorMatrixMultiply (homog, D3DMath_Vector(dir.x, dir.y, dir.z), *D3D_ProjViewMatrix());
	if (homog.x >= -1.0f && homog.y <= 1.0f && homog.z <= 1.0f) {
		if (std::hypot(homog.x, homog.y) < 1e-6) {
			x = (int)w05, y = (int)h05;
		} else {
			x = (int)(w05*(1.0f+homog.x));
			y = (int)(h05*(1.0f-homog.y));
		}
		return true;
	} else
		return false;
}

double Camera::Distance () const
{
	if (external_view && target) {
		if (extmode == CAMERA_GROUNDOBSERVER)
			return gspos.length();
			//return gpos.dist (target->GPos());
		else return rdist*target->Size();
	} else return 0.0;
}

double Camera::Phi () const
{
	if (!target) return 0.0;
	return (external_view ? ephi : cphi);
}

double Camera::Theta () const
{
	if (!target) return 0.0;
	return (external_view ? etheta : ctheta);
}

void Camera::AddPhi (double dphi)
{
	if (external_view) {
		if (extmode == CAMERA_GROUNDOBSERVER) {
			GroundObserverShift (dphi*go.panspeed, 0, 0);
		} else {
			SetRelPos (rdist, ephi+dphi*0.5, etheta);
			if (extmode == CAMERA_ABSDIRECTION)
				gdir = mul (target->GRot(), mul (rrot, Vector (0,0,1)));
		}
	}
}

void Camera::AddTheta (double dtheta)
{
	if (external_view) {
		if (extmode == CAMERA_GROUNDOBSERVER) {
			GroundObserverShift (0, -dtheta*go.panspeed, 0);
		} else {
			SetRelPos (rdist, ephi, etheta+dtheta*0.5);
			if (extmode == CAMERA_ABSDIRECTION)
				gdir = mul (target->GRot(), mul (rrot, Vector (0,0,1)));
		}
	}
}

void Camera::Rotate (double _dphi, double _dtht, bool smooth)
{
	// change the camera direction
	if (external_view) {
		if (extmode == CAMERA_GROUNDOBSERVER && !go.tgtlock)
			GroundObserverTilt (_dphi*0.5, _dtht*0.5);
	} else { // cockpit view
		dphi = _dphi*vmax, dtht = _dtht*vmax;
	}
	rot_smooth = smooth;
}

void Camera::MoveTo (const Vector &p)
{
	if (ExtCtrlMode & CAMDATA_POS) return; // inhibit transition moves if camera is externally controlled
	if (external_view) {
	} else {
		movehead = true;
		tgtp = p;
	}
}

void Camera::MoveToDirect (const Vector &p)
{
	if (external_view) {
	} else {
		rpos = p;
	}
}

void Camera::ShiftPhi (double shift)
{
	if (external_view && extmode != CAMERA_GROUNDOBSERVER && target->Type() == OBJTP_PLANET) {
		shift *= max (1e-6, (rdist-1.0)/rdist);
	}
	AddPhi (shift);
}

void Camera::ShiftTheta (double shift)
{
	if (external_view && extmode != CAMERA_GROUNDOBSERVER && target->Type() == OBJTP_PLANET) {
		shift *= max (1e-6, (rdist-1.0)/rdist);
	}
	AddTheta (shift);
}

void Camera::ShiftDist (double shift)
{
	double fact;
	if (external_view && extmode != CAMERA_GROUNDOBSERVER) {
		double scale = max (1e-4, (rdist-1.0)/rdist); // slow down when approaching target radius
		scale = max(scale, 1.0/target->Size());       // move at least 1m/s
		shift *= scale;
	}
	if (shift > 0) fact = 1.0 + shift;
	else fact = max(1.0/rdist, 1.0/(1.0 - shift));
	ChangeDist (fact);
}

void Camera::ChangeDist (double fact)
{
	if (external_view) {
		if (extmode == CAMERA_GROUNDOBSERVER) {
			GroundObserverShift (0, 0, (fact > 1 ? fact-1 : -1/fact+1)*go.panspeed);
		} else {
			SetRelPos (rdist*fact, ephi, etheta);
			rshift = false;
		}
	}
}

void Camera::Drag (const Vector &gshift)
{
	if (external_view) {
		tref.Set (has_tref ? tref+gshift: gshift);
		tref_t0  = td.SysT1;
		tref_d0  = tref.length();
		tref_r0  = rpos.length();
		tref_r1  = (has_tref && rshift ? tref_r1/tref_r0*rdist : rdist)*target->Size();
		rdist    = tref_r0/target->Size();
		has_tref = true;
		rshift   = (fabs ((tref_r0-tref_r1)/tref_r1) > 0.01);
	}
}

double Camera::SetAperture (double _ap, bool limit_range, bool force)
{
	const double ap_limit_lo = Rad(5.0);
	const double ap_limit_hi = Rad(45.0);

	if (force || (fabs (*ap-_ap) > 1e-5)) {
		*ap = _ap;
		if (limit_range) {
			if      (*ap < ap_limit_lo) *ap = ap_limit_lo;
			else if (*ap > ap_limit_hi) *ap = ap_limit_hi;
		}
		tan_ap = tan (*ap);
		UpdateProjectionMatrix ();
	}
	return *ap;
}

void Camera::SetFrustumLimits (double _nearplane, double _farplane)
{
	nearplane = (float)_nearplane;
	farplane  = (float)_farplane;
	homog_zlimit = farplane / (farplane-nearplane);
	UpdateProjectionMatrix ();
}

void Camera::ViewportToGlobalDir (double sx, double sy, Vector &gdir) const
{
	Vector s((sx-w05+0.5)*tan_ap/h05, (h05-sy-0.5)*tan_ap/h05, 1.0);
	gdir.Set (mul (grot, s.unit()));
}

void Camera::SetDefaultCockpitDir (const Vector &dir, double tilt)
{
	isStdDir = (dir.x == 0 && dir.y == 0 && dir.z > 0 && tilt == 0.0);
	Matrix rrot0_old(rrot0);
	if (isStdDir) {
		cphi0 = ctheta0 = 0.0;
		rrot0.Set (1,0,0,  0,1,0,  0,0,1);
	} else {
		cphi0   = atan2 (-dir.x, dir.z);
		ctheta0 = asin (dir.y);
		double sinph = sin(cphi0),   cosph = cos(cphi0);
		double sinth = sin(ctheta0), costh = cos(ctheta0);
		rrot0.Set (cosph,  sinph*sinth, -sinph*costh,
				   0.0,    costh,        sinth,
		 		   sinph, -cosph*sinth,  cosph*costh);
		if (tilt) {
			double sint = sin(tilt), cost = cos(tilt);
			rrot0.postmul (Matrix (cost, -sint, 0,
				                   sint, cost, 0,
								   0, 0, 1));
		}
	}
	// reset relative camera direction so that absolute direction
	// (in vessel frame) remains the same
	if (!external_view) {
		rrot.premul(transp(rrot0)*rrot0_old);
		cphi = atan2(rrot.m31,rrot.m11);
		ctheta = asin(rrot.m23);
	}
}

void Camera::SetCockpitDir (double ph, double th)
{
	// ph and th are polar and azimuth angles relative to default direction
	if (!external_view) {
		cphi   = normangle (ph);
		ctheta = normangle (th);
		double sinph = sin(cphi),   cosph = cos(cphi);
		double sinth = sin(ctheta), costh = cos(ctheta);
		rrot.Set (cosph,  sinph*sinth, -sinph*costh,
			      0.0,    costh,        sinth,
				  sinph, -cosph*sinth,  cosph*costh);
	}
	eyeofs = mul (rrot, Vector (0,0.1,0.08)) - eyeofs0;
	if (!isStdDir) eyeofs = mul (rrot0, eyeofs);
}

void Camera::SetDistance(double rd)
{
	rdist = rd;
}

void Camera::ResetCockpitDir (bool smooth)
{
	if (!external_view) {
		cntr_phi = 0;
		cntr_tht = 0;
		if (smooth) {
			action = CAMERA_RECENTER;
			rot_smooth = true;
		} else
			SetCockpitDir (0.0, 0.0);
	}
}

void Camera::ResetCockpitDir (double ph, double th, bool smooth)
{
	if (!external_view) {
		cntr_phi = ph;
		cntr_tht = th;
		if (smooth) {
			action = CAMERA_RECENTER;
			rot_smooth = true;
		} else
			SetCockpitDir (ph, th);
	}
}

void Camera::SetCatchAngle (double cangle)
{
	catchangle = cangle;
}

void Camera::ResetCockpitPos ()
{
	if (!external_view) {
		rpos.Set (0,0,0);
	}
}

void Camera::SetRelPos (double r, double ph, double th)
{
	// set relative position rpos and relative orientation rrot
	// w.r.t. reference object from distance, azimuth and polar angles.
	// rrot=identity means camera looks in pos. z-axis of object
	// ph is polar angle from pos z-axis

	rdist  = r;
	ephi   = normangle (ph);
	etheta = normangle (th);

	double sinph = sin(ephi), cosph = cos(ephi);
	double sinth = sin(etheta), costh = cos(etheta);
	// rrot rotates camera from (0,0,-r) to current position in ref. object coords
	//        | cosph 0 -sinph |   | 1   0     0    |
	// rrot = |  0    1    0   | * | 0  costh sinth |
	//        | sinph 0  cosph |   | 0 -sinth costh |
	rrot.Set (cosph,  sinph*sinth, -sinph*costh,
		      0.0,    costh,        sinth,
			  sinph, -cosph*sinth,  cosph*costh);
	double dist = -rdist*target->Size();
	rpos.Set (rrot.m13*dist, rrot.m23*dist, rrot.m33*dist);
}

void Camera::SetTrackMode (ExtCamMode mode, const Body *ref)
{
	if (!external_view || extmode > CAMERA_TARGETFROMOBJECT)
		Attach (target, 1);

	if (extmode == CAMERA_GLOBALFRAME) {
		Vector rdir (-tmul (target->GRot(), rpos).unit());
		SetRelPos (rdist, atan2 (-rdir.x, rdir.z), asin (rdir.y));
	}

	switch (mode) {
	case CAMERA_ABSDIRECTION:
		gdir.Set (-mul (target->GRot(), rpos).unit());
		break;
	case CAMERA_TARGETRELATIVE:
		// nothing to do
		break;
	case CAMERA_GLOBALFRAME:
		gdir.Set (-mul (target->GRot(), rpos).unit());
		SetRelPos (rdist, atan2 (-gdir.x, gdir.z), asin (gdir.y));
		break;
	case CAMERA_TARGETTOOBJECT:
	case CAMERA_TARGETFROMOBJECT:
		if (!ref) return; // bail out
		dirref = ref;
		break;
	}
	extmode = mode;
	SendDlgMessage (3, 0);
}

double Camera::GroundElevation (const Planet *ref, double lng, double lat, double alt) const
{
	double elev = 0.0;
	ElevationManager *emgr = ref->ElevMgr();
	if (emgr) {
		int reslvl = (int)(32.0-log(max(go.alt,100.0))*LOG2);
		elev = emgr->Elevation (lat, lng, reslvl, &etile);
	}
	return elev;
}

void Camera::SetGroundMode (ExtCamMode mode, const Body *ref, double lng, double lat, double alt, const GroundObserverSite *_gos, bool alt_above_ground)
{
	if (!ref) return;
	SetViewExternal();
	if (ref != dirref) {
		dirref = ref;
		for (int i = 0; i < etile.size(); i++)
			etile[i].Clear();
	}
	go.lng = lng;
	go.lat = lat;
	double elev = GroundElevation ((const Planet*)ref, lng, lat, alt);
	if (alt_above_ground) { // measure altitude from elevated ground
		go.alt = alt;
		go.alt0 = alt+elev;
	} else {				// measure altitude from mean radius
		go.alt0 = alt;
		go.alt = alt-elev;
	}
	double clng = cos(go.lng), slng = sin(go.lng);
	double clat = cos(go.lat), slat = sin(go.lat);
	go.R.Set ( clng*slat, clng*clat, -slng,   // rotate from local
	          -clat,      slat,       0,      // observer to local
		       slng*slat, slng*clat,  clng);  // planet coords
	extmode = mode;

	if (!_gos) gos.planet[0] = gos.site[0] = gos.addr[0] = '\0';
	else {
		strcpy (gos.planet, _gos->planet);
		strcpy (gos.site, _gos->site);
		strcpy (gos.addr, _gos->addr);
	}
	SendDlgMessage (3, 0);
}

//bool Camera::GetGroundMode (const Body **ref, double *lng, double *lat, double *alt)
//{
//	if (!external_view || extmode != CAMERA_GROUNDOBSERVER)
//		return false;
//
//	*ref = dirref;
//	*lng = go_lng;
//	*lat = go_lat;
//	*alt = go_alt;
//}

void Camera::SetGroundObserver_PanSpeed (double speed)
{
	go.panspeed = max (0.1, min (1e5, speed));
	g_pOrbiter->Cfg()->CfgCameraPrm.Panspeed = go.panspeed;
}

void Camera::SetGroundObserver_TargetLock (bool lock)
{
	if (lock != go.tgtlock) {
		go.tgtlock = lock;

		if (go.tgtlock && external_view && extmode == CAMERA_GROUNDOBSERVER) {
			gdir.Set ((target->GPos()-gpos).unit());
			Vector hdir = tmul (go.R, tmul (dirref->GRot(), gdir));
			go.tht = asin (hdir.y);
			go.phi = atan2 (-hdir.x, hdir.z);
			OutputGroundObserverParams();
		}
		
		SendDlgMessage (2, this);
	}
}

void Camera::SetGroundObserver_TerrainLimit (double altlimit)
{
	if (altlimit != go.terrain_limit) {
		if (go.alt < go.terrain_limit && go.alt >= altlimit) { // switching from terrain mode to flat mode
			double elev = GroundElevation (planet_proxy, go.lng, go.lat, go.alt);
			go.alt0 = elev + go.alt; // recalibrate target alt0 to current pos
		}
		go.terrain_limit = altlimit;
		g_pOrbiter->Cfg()->CfgCameraPrm.TerrainLimit = go.terrain_limit;
	}
}

void Camera::GroundObserverShift (double dx, double dz, double dh)
{
	if (!external_view || extmode != CAMERA_GROUNDOBSERVER) return;
	double r;
	
	Vector dsz (grot.m13, grot.m23, grot.m33); // dz: go forward/backward w.r.t. camera view direction
	Vector dsx (grot.m11, grot.m21, grot.m31); // dx: go sideways w.r.t. camera view direction
	dirref->GlobalToEquatorial (gpos + dsz*dz + dsx*dx, go.lng, go.lat, r);
	double new_alt = max (1.0, go.alt+dh);
	go.alt0 += new_alt-go.alt;
	go.alt = new_alt;
	double clng = cos(go.lng), slng = sin(go.lng);
	double clat = cos(go.lat), slat = sin(go.lat);
	go.R.Set ( clng*slat, clng*clat, -slng,   // rotate from local
	          -clat,      slat,       0,      // observer to local
		       slng*slat, slng*clat,  clng);  // planet coords
	OutputGroundObserverParams();
}

void Camera::GroundObserverTilt (double dphi, double dtht)
{
	go.phi += dphi;
	go.tht += dtht;
	OutputGroundObserverParams();
}

void Camera::SendDlgMessage (int msgid, void *msg) const
{
	DialogManager *dlgmgr = g_pOrbiter->DlgMgr();
	if (dlgmgr) {
		HWND dlg = dlgmgr->IsEntry (g_pOrbiter->GetInstance(), IDD_CAMERA);
		if (dlg)
			PostMessage (dlg, WM_APP, msgid, (LPARAM)msg);
	}
}

void Camera::OutputGroundObserverParams () const
{
	DialogManager *dlgmgr = g_pOrbiter->DlgMgr();
	if (dlgmgr) {
		HWND dlg = dlgmgr->IsEntry (g_pOrbiter->GetInstance(), IDD_CAMERA);
		if (dlg) {
			char cbuf[256];
			sprintf (cbuf, "Lng = %+0.6f°\r\nLat = %+0.6f°\r\nAlt = %0.2fm\r\nPhi = %0.2f°\r\nTheta = %0.2f°",
				DEG*go.lng, DEG*go.lat, go.alt, DEG*go.phi, DEG*go.tht);
			SendDlgMessage (1, cbuf);
		}
	}
}

void Camera::ResizeViewport (int w, int h)
{
	w05 = 0.5*w;
	h05 = 0.5*h;
	aspect = h05/w05;
	UpdateProjectionMatrix ();
}

DWORD Camera::UpdateExternalControl (ExternalCameraControl *ecc)
{
	DWORD dmode = 0;
	DWORD cmode = ECC->GetCameraMode();

	// camera control via external module
	if (external_view) {
		if (extmode == CAMERA_GROUNDOBSERVER) { // ground mode
			if (cmode & CAMMODE_GROUND) {
				ExternalCameraControl::CamData data;
				if (ECC->clbkPoll (&data)) {
					go.phi = data.yaw;
					go.tht = data.pitch;
				}	
			}
		} else { // track mode
			if (cmode & CAMMODE_TRACK) {
				ExternalCameraControl::CamData data;
				if (ECC->clbkPoll (&data)) {
					const ExternalCameraControl::TrackMode *trkmode = ECC->GetTrackMode();
					double vel, datax, datay;
					double dt = td.SysDT*trkmode->speed;
					double dz = trkmode->deadzone;
					if (trkmode->trackrotation) {
						if (trkmode->rotationdata == ExternalCameraControl::TrackMode::BYPOSITION) {
							datax = data.x, datay = data.y;
						} else {
							datax = data.yaw/PI, datay = data.pitch/PI;
						}
						vel = 2.0*(fabs(datax)-dz);
						if (vel > 0) AddPhi ((datax > 0 ? vel:-vel) * dt);
						vel = 2.0*(fabs(datay)-0.6*dz);
						if (vel > 0) AddTheta ((datay > 0 ? -vel:vel)*dt);
					}
					if (trkmode->trackzoom) {
						vel = 2.0*(fabs(data.z)-dz);
						if (vel > 0) ChangeDist (data.z > 0 ? 1.0/(1.0+vel*dt) : 1.0+vel*dt);
					}
				}
			}
		}
	} else { // cockpit camera
		DWORD extctrl = 0;
		const ExternalCameraControl::VCMode *vcmode = 0;
		bool dorot = true, dopos = false;
		double rotscale = 1.0;
		double posrange = 0.5;
		static bool frozen = false;
		static bool dragrot = false, dragpos = false;
		switch (intmode) {
		case CAMERA_VIRTUALCOCKPIT:
			extctrl = (cmode & CAMMODE_VC);
			if (extctrl) {
				vcmode = ECC->GetVCMode();
				dorot = vcmode->trackrotation;
				dopos = vcmode->trackposition;
				rotscale = vcmode->rotationrange/PI;
				posrange = vcmode->positionrange;
				if (frozen) {
					if (td.SysT0-mmoveT > vcmode->freeze_t)
						frozen = false, dragrot = dorot, dragpos = dopos, action = CAMERA_RECENTER;
				} else {
					if (vcmode->freezeonmouse && td.SysT0-mmoveT < vcmode->freeze_t)
						frozen = true;
					else if (dragrot && action == CAMERA_NORMAL)
						dragrot = false;
				}
				extctrl = !frozen;
			}
			break;
		case CAMERA_2DPANELCOCKPIT:
			extctrl = (cmode & CAMMODE_PANELCOCKPIT);
			break;
		case CAMERA_GENERICCOCKPIT:
			extctrl = (cmode & CAMMODE_GENERICCOCKPIT);
			break;
		}
		if (extctrl) {
			ExternalCameraControl::CamData data;
			if (ECC->clbkPoll (&data)) {
				if (dorot) {
					double yaw = data.yaw*rotscale;
					double pitch = data.pitch*rotscale;
					if (dragrot) {
						cntr_phi = yaw;
						cntr_tht = pitch;
					} else {
						SetCockpitDir (yaw, pitch);
						action = CAMERA_MANUAL;
						dphi = dtht = 0;
						vphi = vtht = 0;
					}
					dmode |= CAMDATA_DIR;
				}
				if (dopos) {
					double x = min (posrange, max (-posrange, data.x));
					double y = min (posrange, max (-posrange, data.y));
					double z = min (posrange, max (-posrange, data.z));
					Vector p(x,y,z);
					Vector pr(mul (isStdDir ? rrot : rrot0*rrot, p));
					if (dragpos) {
						Vector D0(pr-rpos);
						double d0 = D0.length();
						double d = td.SysDT*1.0;
						if (d > d0) dragpos = false;
						else pr = rpos + D0*(d/d0);
					}
					MoveToDirect (pr);
					dmode |= CAMDATA_POS;
				}
			}
		}
	}
	return dmode;
}

void Camera::Update ()
{
	double dt = td.SysDT;
	double t1 = td.SysT1;

	// NOTE: Camera is updated after world state has been updated, but before state S1
	// is copied back to state S0

	ExtCtrlMode = 0;
	if (ECC) ExtCtrlMode = UpdateExternalControl (ECC);

	// get a list of current visuals
	double dist, calt, dist_proxy = 1e100;
	VObject **vobj, *vo;
	const Body *bd;
	int i, nobj;
	nobj = 0; vobj = 0;
	for (i = 0; i < nobj; i++) {
		bd = (vo = vobj[i])->GetBody();
		if (!external_view && bd == (Body*)g_focusobj &&
			!g_focusobj->HasExtpassMeshes()) continue;
		dist = vo->CDist() - bd->ClipRadius();
		if (bd->Type() == OBJTP_PLANET) {
			if (dist < 50e3) {
				dist = 0; // suppress dynamic nearplane if in proximity of the surface
			} else if (((Planet*)bd)->CloudParam (calt)) {
				double lowc;
				if (dist > calt) dist -= calt;
				else if (dist < (lowc = 0.99985*(calt+bd->Size())-bd->Size()))
					dist = lowc-dist;
				else dist = 4.0;
			}
		}
		if (dist < dist_proxy) dist_proxy = dist;
	}

	// find the largest apparent planet
	double ralt_proxy = 1e100;
	for (i = 0; i < g_psys->nPlanet(); i++) {
		Planet *planet = g_psys->GetPlanet(i);
		dist = gpos.dist (planet->GPos());
		double alt = dist - planet->Size();
		double ralt = alt/planet->Size();
		if (ralt < ralt_proxy) {
			ralt_proxy = ralt;
			alt_proxy  = alt;
			planet_proxy = planet;
		}
	}

	if (external_view) {
		bool allow_invert = true;
		switch (extmode) {
		case CAMERA_TARGETRELATIVE:
			gspos.Set (mul (target->GRot(), rpos));
			gpos.Set (gspos + target->GPos());
			GPOS = MakeVECTOR3 (gpos);
			grot.Set (target->GRot() * rrot);
			break;
		case CAMERA_TARGETTOOBJECT:
		case CAMERA_TARGETFROMOBJECT:
			gdir.Set ((dirref->GPos()-target->GPos()).unit());
			if (extmode == CAMERA_TARGETFROMOBJECT) gdir = -gdir;
			allow_invert = false;
			// fall through
		case CAMERA_ABSDIRECTION: {
			Vector rdir (tmul (target->GRot(), gdir));
			double th = asin (rdir.y);
			double ph = atan2 (-rdir.x, rdir.z);
			double dph = fabs(ph-ephi);
			if (allow_invert && dph >= Pi05 && dph < 3.0*Pi05)
				ph += Pi, th = Pi-th; // camera is upside-down - a bit hacky!
			SetRelPos (rdist, ph, th);
			gspos.Set (gdir * (-rdist*target->Size()));
			gpos.Set (gspos + target->GPos());
			GPOS = MakeVECTOR3 (gpos);
			grot.Set (target->GRot() * rrot);
			} break;
		case CAMERA_GLOBALFRAME:
			gspos.Set (rpos);
			gpos.Set (gspos + target->GPos());
			GPOS = MakeVECTOR3 (gpos);
			grot.Set (rrot);
			break;
		case CAMERA_GROUNDOBSERVER: {
			double rad, elev = 0.0;
			if (dirref->Type() == OBJTP_PLANET)
				elev = GroundElevation((Planet*)dirref, go.lng, go.lat, go.alt);
			if (go.alt0-elev >= go.terrain_limit) { // flat mode
				rad = dirref->Size() + go.alt0;
				go.alt = go.alt0 - elev;
			} else { // terrain-hugging mode
				rad = dirref->Size() + elev + go.alt;
			}
			dirref->EquatorialToGlobal (go.lng, go.lat, rad, gpos);
			GPOS = MakeVECTOR3 (gpos);
			gspos.Set (gpos - target->GPos());
			if (go.tgtlock) {
				gdir.Set ((target->GPos()-gpos).unit());
				Vector hdir = tmul (go.R, tmul (dirref->GRot(), gdir));
				if (fabs (hdir.y) < 0.999999) {
					go.tht = asin (hdir.y);
					go.phi = atan2 (-hdir.x, hdir.z);
				} else {
					go.tht = (hdir.y > 0 ? Pi05 : -Pi05);
					go.phi = 0.0;
				}
				//OutputGroundObserverParams();
			}
			double sinph = sin(go.phi), cosph = cos(go.phi);
			double sinth = sin(go.tht), costh = cos(go.tht);
			rrot.Set (cosph,  sinph*sinth, -sinph*costh,
				      0.0,    costh,        sinth,
					  sinph, -cosph*sinth,  cosph*costh);
			grot.Set (dirref->GRot() * go.R * rrot);
			} break;
		}
		if (has_tref) {
			double t = t1 - tref_t0;
			if (t >= dragT) {
				tref.Set(0,0,0);
				has_tref = false;
				rshift = false;
			} else {
				double scale = (1.0 - t*t/(dragT*dragT)*(3.0-2.0*t/dragT));
				double tlen = tref.length();
				if (tlen) {
					tref *= (tref_d0*scale)/tref.length();
					gpos += tref; gspos += tref;
					GPOS = MakeVECTOR3 (gpos);
				}
				if (rshift) {
					rpos *= ((tref_r0-tref_r1)*scale+tref_r1)/rpos.length();
					rdist = rpos.length()/target->Size();
				}
			}
		}
	} else { // cockpit mode

		double vp, vt;
		static const double dfac = 2.0*amax/vmax; // max rotation deceleration factor
		static double pf = 1e10;
		static double idleT = 0;

		switch (action) {
		case CAMERA_NORMAL:
			if (!mbdown[1] && !brot && !IsCockpitDefaultDir() && (fabs (normangle (cphi)) < catchangle) && (fabs (normangle (ctheta)) < catchangle))
				ResetCockpitDir();
			break;
		case CAMERA_RECENTER:
			pf = 1e10; // init stopping test
			action = CAMERA_RECENTER_CONT;
			// fall through
		case CAMERA_RECENTER_CONT: {
			double a = normangle (cntr_phi-cphi);
			double b = normangle (cntr_tht-ctheta);
			double fa = fabs(a), fb = fabs(b), f = fa+fb;
			if (f < 0.001 || (f < 0.01 && f > pf)) {
				SetCockpitDir (cntr_phi, cntr_tht);
				dphi = dtht = vphi = vtht = 0.0;
			  	action = CAMERA_NORMAL;
			} else {
				if (fa > fb) {
					if (a >= 0.0) dphi = min (min (min (vmax, vphi+amax*dt), a*dfac) * dt, a);
					else          dphi = max (max (max (-vmax, vphi-amax*dt), a*dfac) * dt, a);
					dtht = dphi*b/a;
				} else {
					if (b >= 0.0) dtht = min (min (min (vmax, vtht+amax*dt), b*dfac) * dt, b);
					else          dtht = max (max (max (-vmax, vtht-amax*dt), b*dfac) * dt, b);
					dphi = dtht*a/b;
				}
				vphi = dphi*td.iSysDT;
				vtht = dtht*td.iSysDT;
				pf = fa+fb;
			}
			} break;
		case CAMERA_MANUAL:
			break;
		}

		if (dphi || dtht) {
			double dp_left, dp_right, dt_up, dt_down;
			g_focusobj->CamRange (dp_left, dp_right, dt_up, dt_down);
			const double phirange = 0.8*Pi;   // make panel-dependent!
			const double thtrange = 0.8*Pi05; // make panel-dependent!

			vp = dphi*td.iSysDT;
			vt = dtht*td.iSysDT;

			double p = normangle (cphi);
			if (vp > 0) vp = min (vp,  (dp_left-p)*dfac);
			else        vp = max (vp, (-dp_right-p)*dfac);
			double t = normangle (ctheta);
			if (vt > 0) vt = min (vt,  (dt_up-t)*dfac);
			else        vt = max (vt, (-dt_down-t)*dfac);
			dphi = dtht = 0.0;
			idleT = t1;
		} else {
			vp = vt = 0.0;
		}
		if (rot_smooth) {
			if (fabs (vp-vphi)*td.iSysDT > amax) vp = (vp > vphi ? amax:-amax)*dt + vphi;
			if (fabs (vt-vtht)*td.iSysDT > amax) vt = (vt > vtht ? amax:-amax)*dt + vtht;
		}
		if (vp || vt) {
			SetCockpitDir (cphi + vp*dt, ctheta + vt*dt);
			vphi = vp, vtht = vt;
			brot = true;
		} else {
			vphi = vtht = 0.0;
			brot = (t1-idleT < 0.1);
			//brot = false;
		}

		// 'lean'
		if (movehead) {
			static double v = 0;
			const double a = 1.5, vmax = 2.0;
			Vector dr = tgtp - rpos;
			double d, dst = dr.length();
			if (dst > 1e-4) {
				if (v < vmax) v = min (vmax, v + dt*a);
				v = min (v, sqrt (0.5*a*dst));
				d = min (dst, dt * v);
				rpos += dr*(d/dst);
			} else {
				rpos = tgtp;
				movehead = false;
				v = 0;
			}
		}

		grot.Set (target->GRot() * (isStdDir ? rrot : rrot0*rrot));
		gspos.Set (mul (target->GRot(), *rofs + rpos + eyeofs));
		gpos.Set (gspos + target->GPos());
		GPOS = MakeVECTOR3 (gpos);
	}

	// dynamic nearplane calculation
	const double np_min = (external_view ? 2.5 : 1.0);
	double np_max = 1e-4*farplane;
	double np = dist_proxy;  // near-plane no further than closest object
	if (planet_proxy && alt_proxy < 0.1*planet_proxy->Size()) {
		// make sure the near-plane doesn't cut into the planet surface
		Vector gd(grot.m13,grot.m23,grot.m33);
		Vector cp(planet_proxy->GPos()-gpos);
		double alt = cp.length()-planet_proxy->Size();
		double az = acos (dotp (gd, cp.unit()));
		double a = atan (tan_ap*std::hypot(w05,h05)/h05);
		double tht = az-a;
		if (tht < Pi05)
			np = min (np, alt*cos(a)/cos(tht));
	}
	if (np < np_min) {
		if (nearplane > np_min) SetFrustumLimits (np_min, farplane);
	} else if (np > np_max) {
		if (nearplane < np_max) SetFrustumLimits (np_max, farplane);
	} else {
		if (nearplane > np) SetFrustumLimits (max (np_min, 0.5*np), farplane);
		else if (nearplane < 0.25*np) SetFrustumLimits (min (np_max, 0.5*np), farplane);
	}
#ifdef OLD_NEARPLANE
	const double minp_min = 5.0; // 2*smallest near plane
	double minp_max = 1e-3*farplane;
	if (dist_proxy < minp_min) dist_proxy = minp_min;
	else if (dist_proxy > minp_max) dist_proxy = minp_max;
	if (nearplane < 0.5*dist_proxy || nearplane > 0.9*dist_proxy)
		SetFrustumLimits (0.75*dist_proxy, farplane);
#endif

	// Update D3D view transform matrix. Note this is only a rotation,
	// no translation, since our camera is always at the origin in
	// visual world coordinates
	// Note that we need inv(inv(grot)) = grot since
	// 1. grot is rotation of camera in world coordinates, whereas D3D wants
	//    rotation from world to view coordinates
	// 2. rotations in logical interface are counterclockwise, but clockwise
	//    in D3D
	SetD3DRotation (view_mat, grot);
	pv_mat_valid = false;

	//UpdateMouse ();

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

D3DMATRIX *Camera::D3D_ProjViewMatrix ()
{
	if (!pv_mat_valid) {
		D3DMath_MatrixMultiply (pv_mat, proj_mat, view_mat);
		pv_mat_valid = true;
	}
	return &pv_mat;
}

void Camera::UpdateProjectionMatrix ()
{
	ZeroMemory (&proj_mat, sizeof (D3DMATRIX));
	proj_mat._11 = (FLOAT)(aspect / tan_ap);
	proj_mat._22 = (FLOAT)(1.0    / tan_ap);
	if (farplane >= 1e20) {
		proj_mat._33 = 1.0f;
		proj_mat._43 = -nearplane;
	} else {
		proj_mat._43 = (proj_mat._33 = farplane / (farplane-nearplane)) * (-nearplane);
	}
	proj_mat._34 = 1.0f;

	// register new projection matrix with device
    //pDev->SetTransform (D3DTRANSFORMSTATE_PROJECTION, &proj_mat);
}

void Camera::InitState (const char *scn, Body *default_target)
{
	// set defaults
	target = default_target;
	external_view = (target->Type() != OBJTP_VESSEL);
	ap_int = ap_ext = RAD*25.0;
	ap = (external_view ? &ap_ext : &ap_int);

	// read state from scenario file
	if (scn) {
		ifstream ifs (g_pOrbiter->ScnPath(scn));
		if (ifs) Read (ifs);
	}
	Body *newtgt = target; target = NULL;
	int mode = (external_view ? 1:0); external_view = true;
	Attach (newtgt, mode);
	if (extmode == CAMERA_GROUNDOBSERVER)
		SetGroundMode (CAMERA_GROUNDOBSERVER, dirref, go.lng, go.lat, go.alt);
	SetAperture (*ap, true, true);
}

void Camera::ClearPresets ()
{
	if (npreset) {
		for (DWORD i = 0; i < npreset; i++)
			delete preset[i];
		delete []preset;
		preset = NULL;
		npreset = 0;
	}
}

DWORD Camera::AddPreset (CameraMode *mode)
{
	if (!mode) mode = GetCMode();
	CameraMode **tmp = new CameraMode*[npreset+1]; TRACENEW
	if (npreset) {
		memcpy (tmp, preset, npreset*sizeof(CameraMode*));
		delete []preset;
	}
	preset = tmp;
	preset[npreset] = mode;
	return npreset++;
}

bool Camera::DelPreset (DWORD idx)
{
	DWORD i, j;
	if (idx >= npreset) return false;
	CameraMode **tmp = 0;
	if (npreset > 1) {
		tmp = new CameraMode*[npreset-1]; TRACENEW
		for (i = j = 0; i < npreset; i++)
			if (i != idx) tmp[j++] = preset[i];
	}
	delete []preset;
	preset = tmp;
	if (npreset > 0) npreset--;
	return true;
}

void Camera::RecallPreset (DWORD idx)
{
	if (idx >= npreset) return; // out of range
	SetCMode (preset[idx]);
}

CameraMode *Camera::GetPreset (DWORD idx)
{
	if (idx >= npreset) return 0;
	return preset[idx];
}

bool Camera::Read (ifstream &ifs)
{
	char cbuf[256] = "", ctrackmode[64] = "", cdirref[64] = "", * pc = NULL;
	Body *tg = 0;
	double rd = 4.0, ph = 0.0, th = 0.0;
	int n = 0;
	go.tgtlock = true;

	if (!FindLine (ifs, "BEGIN_CAMERA")) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) break;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_CAMERA", 10)) break;
		if (!_strnicmp (pc, "TARGET", 6)) {
			pc = trim_string (pc+6);
			if (!(tg = g_psys->GetObj (pc, true)))
				tg = g_psys->GetBase (pc, true);
		} else if (!_strnicmp (pc, "MODE", 4)) {
			pc = trim_string (pc+4);
			if (!_strnicmp (pc, "Extern", 6)) external_view = true;
			else                             external_view = false;
		} else if (!_strnicmp (pc, "POS", 3)) {
			n = sscanf (pc+3, "%lf%lf%lf", &rd, &ph, &th);
			ph *= RAD, th *= RAD;
		} else if (!_strnicmp (pc, "FOV", 3)) {
			double a;
			n = sscanf (pc+3, "%lf", &a);
			if (a < 10.0) a = 10.0;
			else if (a > 160.0) a = 160.0;
			a *= RAD*0.5;
			ap_int = ap_ext = a;
		} else if (!_strnicmp (pc, "TRACKMODE", 9)) {
			n = sscanf (pc+9, "%s%s", ctrackmode, cdirref);
		} else if (!_strnicmp (pc, "GROUNDLOCATION", 14)) {
			n = sscanf (pc+14, "%lf%lf%lf", &go.lng, &go.lat, &go.alt);
			go.lng *= RAD, go.lat *= RAD;
		} else if (!_strnicmp (pc, "GROUNDDIRECTION", 15)) {
			n = sscanf (pc+15, "%lf%lf", &go.phi, &go.tht);
			go.phi *= RAD, go.tht *= RAD;
			go.tgtlock = false;
		} else if (!_strnicmp (pc, "BEGIN_PRESET", 12)) {
			for (;;) {
				if (!ifs.getline (cbuf, 256)) break;
				pc = trim_string (cbuf);
				if (!_strnicmp (pc, "END_PRESET", 10)) break;
				AddPreset (CameraMode::Create (pc));
			}
		}
	}
	if (tg && external_view) target = tg;
	if (external_view) {
		rdist = rd, ephi = ph, etheta = th;
		if (!_stricmp (ctrackmode, "AbsoluteDirection"))
			extmode = CAMERA_ABSDIRECTION;
		else if (!_stricmp (ctrackmode, "GlobalFrame"))
			extmode = CAMERA_GLOBALFRAME;
		else if (!_stricmp (ctrackmode, "TargetTo") && (dirref = g_psys->GetObj (cdirref, true)))
			extmode = CAMERA_TARGETTOOBJECT;
		else if (!_stricmp (ctrackmode, "TargetFrom") && (dirref = g_psys->GetObj (cdirref, true)))
			extmode = CAMERA_TARGETFROMOBJECT;
		else if (!_stricmp (ctrackmode, "Ground") && (dirref = g_psys->GetObj (cdirref, true)))
			extmode = CAMERA_GROUNDOBSERVER;
		else 
			extmode = CAMERA_TARGETRELATIVE;
	}
	return true;
}

void Camera::Write (ostream &ofs) const
{
	ofs << "BEGIN_CAMERA" << endl;
	ofs << setprecision(2);
	ofs << "  TARGET " << target->Name() << endl;
	ofs << "  MODE " << (external_view ? "Extern":"Cockpit") << endl;
	if (external_view) {
		ofs << "  POS " << setprecision(6) << rdist << ' ' << DEG*ephi << ' ' << DEG*etheta << setprecision(2) << endl;
		ofs << "  TRACKMODE ";
		switch (extmode) {
		case CAMERA_ABSDIRECTION:
			ofs << "AbsoluteDirection\n";
			break;
		case CAMERA_GLOBALFRAME:
			ofs << "GlobalFrame\n";
			break;
		case CAMERA_TARGETTOOBJECT:
			ofs << "TargetTo " << dirref->Name() << endl;
			break;
		case CAMERA_TARGETFROMOBJECT:
			ofs << "TargetFrom " << dirref->Name() << endl;
			break;
		case CAMERA_GROUNDOBSERVER:
			ofs << "Ground " << dirref->Name() << endl;
			ofs << setprecision(5) << "  GROUNDLOCATION "
				<< go.lng*DEG << ' ' << go.lat*DEG << ' '
				<< setprecision(2) << go.alt << endl;
			if (!go.tgtlock)
				ofs << "  GROUNDDIRECTION " << go.phi*DEG << ' ' << go.tht*DEG << endl;
			break;
		default:
			ofs << "TargetRelative\n";
			break;
		}
	}
	ofs << "  FOV " << 2.0*DEG* *ap << endl;
	if (npreset) {
		char cbuf[256] = "    ";
		ofs << "  BEGIN_PRESET" << endl;
		for (DWORD i = 0; i < npreset; i++) {
			preset[i]->Store (cbuf+4);
			ofs << cbuf << endl;
		}
		ofs << "  END_PRESET" << endl;
	}
	ofs << "END_CAMERA" << endl << endl;
}


// ============================================================
// Implementation of external camera control interface

bool Camera::RegisterExternalControl (ExternalCameraControl *ecc)
{
	if (ECC) {
		return false; // external control already registered
	} else {
		ECC = ecc;
		return true;
	}
}

bool Camera::UnregisterExternalControl ()
{
	if (!ECC) {
		return false; // no external control registered
	} else {
		ECC = 0;
		return true;
	}
}


// ============================================================
// ============================================================
// Implementation of CameraMode_XXX API interface classes
// ============================================================
// ============================================================

// ============================================================

CameraMode::CameraMode ()
{
	target = 0;  // use current target by default
	fov = 0;     // use current field of view by default
}

void CameraMode::SetTarget (OBJHANDLE hTgt)
{
	target = hTgt;
}

void CameraMode::SetFOV (double FOV)
{
	fov = FOV;
}

CameraMode *CameraMode::Create (char *str)
{
	CameraMode *cm;
	char *pc, *tc;

	if (!(pc = strtok (str, ":"))) return 0;
	tc = trim_string (pc);
	if (!_stricmp (tc, "Cockpit")) {
		cm = new CameraMode_Cockpit; TRACENEW
	} else if (!_stricmp (tc, "Track")) {
		cm = new CameraMode_Track; TRACENEW
	} else if (!_stricmp (tc, "Ground")) {
		cm = new CameraMode_Ground; TRACENEW
	} else {
		cm = new CameraMode_Cockpit; TRACENEW
	}
	
	if (!(pc = strtok (NULL, ":")) || !(cm->target = (OBJHANDLE)g_psys->GetObj (trim_string (pc), true))) {
		delete cm;
		return 0;
	}

	pc = strtok (NULL, ":");
	if (pc) sscanf (pc, "%lf", &cm->fov);

	pc = strtok (NULL, "");
	if (pc) cm->Init (trim_string (pc));

	return cm;
}

// ============================================================

CameraMode_Cockpit::CameraMode_Cockpit (): CameraMode ()
{
	cmode = CM_CURRENT;
	pos = -1;
	lean = -1;
	lean_smooth = false;
}

void CameraMode_Cockpit::Init (char *str)
{
	if (!str || str[0] == '\0') return;

	if (!strnicmp(str, "generic", 7)) {
		cmode = CM_GENERIC;
		str += 7;
	} else if (!strnicmp(str, "panel2d", 7)) {
		cmode = CM_PANEL2D;
		str += 7;
		if (str[0] == ':' && sscanf(++str, "%d", &pos))
			while (*str != ' ' && *str != '\0') str++;
	} else if (!strnicmp(str, "vc", 2)) {
		cmode = CM_VC;
		str += 2;
		if (str[0] == ':' && sscanf(++str, "%d", &pos)) {
			while (*str != ' ' && *str != '\0' && *str != ':') str++;
			if (*str == ':' && sscanf(++str, "%d", &lean)) {
				lean_smooth = false;
				while (*str != ' ' && *str != '\0' && toupper(*str) != 'S') str++;
				if (toupper(*str) == 'S') {
					lean_smooth = true;
					str++;
				}
			}
		}
	} else if (!strnicmp(str, "current", 7)) {
		cmode = CM_CURRENT;
		str += 7;
	}
	
}

void CameraMode_Cockpit::GetDescr (char *str, int len)
{
	char cbuf[256] = "";
	if (target) strcat (cbuf, ((Body*)target)->Name()), strcat (cbuf, " ");
	strcat (cbuf, "Cockpit ");
	strncpy (str, cbuf, len-1);
}

void CameraMode_Cockpit::Store (char *str)
{
	sprintf (str, "Cockpit:%s:%0.2f", target ? ((Body*)target)->Name() : "-", fov);
}

// ============================================================

CameraMode_Track::CameraMode_Track (): CameraMode ()
{
	reldist = 0;     // use current distance and position
	tmode = TM_CURRENT; // use current track mode
}

void CameraMode_Track::Init (char *str)
{
	char tm[64], rf[256];
	sscanf (str, "%s%lf%lf%lf%s", tm, &reldist, &phi, &theta, rf);
	if (!_stricmp (tm, "RELATIVE"))
		tmode = TM_RELATIVE;
	else if (!_stricmp (tm, "ABSDIR"))
		tmode = TM_ABSDIR;
	else if (!_stricmp (tm, "GLOBAL"))
		tmode = TM_GLOBAL;
	else if (!_stricmp (tm, "TARGETTOREF")) {
		tmode = TM_TARGETTOREF;
		Body *r = g_psys->GetObj (rf, true);
		if (r) ref = (OBJHANDLE)r;
		else tmode = TM_CURRENT;
	} else if (!_stricmp (tm, "TARGETFROMREF")) {
		tmode = TM_TARGETFROMREF;
		Body *r = g_psys->GetObj (rf, true);
		if (r) ref = (OBJHANDLE)r;
		else tmode = TM_CURRENT;
	}
}

void CameraMode_Track::Store (char *str)
{
	static const char *tmstr[6] = {"CURRENT","RELATIVE", "ABSDIR", "GLOBAL", "TARGETTOREF", "TARGETFROMREF"};
	sprintf (str, "Track:%s%:%0.2f:%s %0.3f %0.3f %0.3f", 
		target ? ((Body*)target)->Name() : "-", fov,
		tmstr[tmode], reldist, phi, theta);
	if (tmode == TM_TARGETTOREF || tmode == TM_TARGETFROMREF) {
		strcat (str, " ");
		strcat (str, ((Body*)ref)->Name());
	}
}

void CameraMode_Track::GetDescr (char *str, int len)
{
	char cbuf[256] = "";
	if (target) strcat (cbuf, ((Body*)target)->Name()), strcat (cbuf, " ");
	strcat (cbuf, "Track ");
	switch (tmode) {
	case TM_RELATIVE: strcat (cbuf, "relative "); break;
	case TM_ABSDIR:   strcat (cbuf, "fixed "); break;
	case TM_GLOBAL:   strcat (cbuf, "global "); break;
	}
	strncpy (str, cbuf, len-1);
}

void CameraMode_Track::SetTrackMode (TrackMode trackmode, OBJHANDLE refobj)
{
	tmode = trackmode;
	ref   = refobj;
}

void CameraMode_Track::SetPosition (double rd, double ph, double th)
{
	reldist = rd;
	phi     = ph;
	theta   = th;
}

void CameraMode_Track::GetPosition (double *rd, double *ph, double *th) const
{
	*rd = reldist;
	*ph = phi;
	*th = theta;
}

// ============================================================

CameraMode_Ground::CameraMode_Ground (): CameraMode ()
{
	ref = 0;    // use current planet
	alt = 0;    // use current position
	alt_above_ground = true;
	tgtlock = true;
}

void CameraMode_Ground::Init (char *str)
{
	char rf[256], alt_mode;
	int i = sscanf (str, "%s%lf%lf%lf%c%lf%lf", rf, &lng, &lat, &alt, &alt_mode, &phi, &theta);
	lng *= RAD, lat *= RAD;
	alt_above_ground = (toupper(alt_mode) != 'M');
	tgtlock = (i < 7);
	ref = (OBJHANDLE)g_psys->GetObj (rf, true);
}

void CameraMode_Ground::Store (char *str)
{
	sprintf (str, "Ground:%s:%0.2f:%s %0.5f %0.5f %0.2f%s",
		target ? ((Body*)target)->Name() : "-", fov,
		((Body*)ref)->Name(), lng*DEG, lat*DEG, alt, alt_above_ground ? "" : "M");
	if (!tgtlock) sprintf (str+strlen(str), " %0.2f %0.2f",
		phi, theta);
}

void CameraMode_Ground::GetDescr (char *str, int len)
{
	char cbuf[256] = "";
	if (target) strcat (cbuf, ((Body*)target)->Name()), strcat (cbuf, " ");
	strcat (cbuf, "Ground ");
	strncpy (str, cbuf, len-1);
}

void CameraMode_Ground::SetPosition (double longitude, double latitude, double altitude, OBJHANDLE hRef)
{
	lng = longitude;
	lat = latitude;
	alt = altitude;
	if (hRef) ref = hRef;
}

void CameraMode_Ground::GetPosition (double *longitude, double *latitude, double *altitude, OBJHANDLE *hRef) const
{
	*longitude = lng;
	*latitude  = lat;
	*altitude  = alt;
	*hRef      = ref;
}

void CameraMode_Ground::SetOrientation (double ph, double th)
{
	phi = ph;
	theta = th;
	tgtlock = false;
}

void CameraMode_Ground::GetOrientation (double *ph, double *th) const
{
	*ph = phi;
	*th = theta;
}
