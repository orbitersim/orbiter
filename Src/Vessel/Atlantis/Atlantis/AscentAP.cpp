// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//
// AscentAP.cpp
// Class implementation for Atlantis ascent autopilot
// Automatic control of ascent profile from liftoff to
// ET separation using engine gimballing of SSME and SRB engines
// ==============================================================

#include "Atlantis.h"
#include "AscentAP.h"
#include "resource.h"
#include "Common\Dialog\Graph.h"

extern GDIParams g_Param;

// ==============================================================
// class AscentAP: ascent autopilot
// ==============================================================

AscentAP::AscentAP (Atlantis *atlantis)
{
	vessel = atlantis;
	n_pitch_profile = 0;
	active = false;
	met_active = false;
	do_oms2 = true;
	met = met_meco = met_oms_start = met_oms_end = 0.0;
	SetDefaultProfiles();
	launch_lat = launch_lng = 0.0;
	pt = -1.0; pspd = acc = 0.0;
	pacc_valid = false;
}

// --------------------------------------------------------------

AscentAP::~AscentAP ()
{
	if (n_pitch_profile) delete []pitch_profile;
}

// --------------------------------------------------------------

void AscentAP::Update (double simt)
{
	const double eps=1e-5;

	tgt.az = CalcTargetAzimuth();
	tgt.pitch = CalcTargetPitch();

	if (met_active)
		met = simt-t_launch;

	if (active) {
		if (vessel->status == 0) {
			if (met < 0.0) {
				vessel->SetEngineLevel (ENGINE_MAIN, min (1.0, (SRB_STABILISATION_TIME+met)*0.4));
			} else {
				t_launch = vessel->t0 = simt;
				vessel->pET->IgniteSRBs ();
				vessel->status = 1;
			}
		} else if (vessel->status < 3) {
			if (met_meco < 0.0) {
				double apalt;
				OBJHANDLE hRef = vessel->GetApDist(apalt);
				bool fuel_down = (vessel->pET ? vessel->pET->GetMainPropellantMass() < 10.0 : false);
				apalt -= oapiGetSize (hRef);
				if (apalt >= tgt_alt || fuel_down) {
					vessel->SetThrusterGroupLevel (THGROUP_MAIN, 0.0); // MECO
					met_meco = met;
				} else {
					double th = SSMEThrustProfile(met);
					if (th >= 0.0)
						vessel->SetThrusterGroupLevel (THGROUP_MAIN, th);
				}
			} else if (met-met_meco >= 10.0) {
				vessel->SeparateTank();
				double apalt;
				OBJHANDLE hRef = vessel->GetApDist(apalt);
				apalt -= oapiGetSize (hRef);
				if (apalt + 1e3 < tgt_alt) {
					schedule_oms1 = met+20.0;
				}
			}
		} else if (schedule_oms1 > 0.0) {
			if (met >= schedule_oms1) {
				vessel->SetThrusterGroupLevel (THGROUP_MAIN, 1.0);
				schedule_oms1 = -1.0;
				met_oms1_start = met;
			}
		} else if (met_oms1_start > 0.0) {
			double apalt;
			OBJHANDLE hRef = vessel->GetApDist(apalt);
			apalt -= oapiGetSize (hRef);
			if (apalt >= tgt_alt) {
				vessel->SetThrusterGroupLevel (THGROUP_MAIN, 0.0); // OMS1 end
				met_oms1_start = -1.0;
			}
		} else if (do_oms2) {
			if (met_oms_start < 0.0) {
				OBJHANDLE hRef = vessel->GetSurfaceRef();
				ELEMENTS el;
				ORBITPARAM prm;
				vessel->GetElements(hRef, el, &prm);
				schedule_oms = prm.ApT - 70.0;
				if (schedule_oms < 0.0) {
					vessel->SetThrusterGroupLevel (THGROUP_MAIN, 1.0); // OMS ignition
					met_oms_start = met;
				}
			} else if (met_oms_end < 0.0) {
				double perad, aprad, pealt, ecc;
				OBJHANDLE hRef = vessel->GetPeDist(perad);
				hRef = vessel->GetApDist(aprad);
				pealt = perad-oapiGetSize(hRef);
				ecc = (aprad-perad)/(aprad+perad);
				if (ecc < ecc_min) ecc_min = ecc;
				if (ecc > ecc_min+eps) {
					vessel->SetThrusterGroupLevel (THGROUP_MAIN, 0.0); // OMS cut off
					met_oms_end = met;
					active = false; // turn off ascent autopilot
				}
			}
		} else
			active = false;
		if (!active) {
			for (int i = 0; i < vessel->GetThrusterCount(); i++)
				vessel->SetThrusterLevel (vessel->GetThrusterHandleByIndex(i), 0);
		}
			
	}
}

// --------------------------------------------------------------

void AscentAP::Launch ()
{
	if (!active && vessel->status == 0 && vessel->pET) {
		double r;
		vessel->GetEquPos(launch_lng, launch_lat, r);
		t_launch = oapiGetSimTime()+SRB_STABILISATION_TIME;
		met_meco = met_oms_start = met_oms_end = -1.0;
		met_oms1_start = schedule_oms1 = -1.0;
		ecc_min = 1e10;
		vessel->SetAttitudeMode (RCS_NONE);
		active = true;
		met_active = true;
	}
}

// --------------------------------------------------------------

void AscentAP::Disengage ()
{
	active = false;
	for (THGROUP_TYPE thg = THGROUP_ATT_PITCHUP; thg <= THGROUP_ATT_BACK; thg = (THGROUP_TYPE)(thg+1))
		vessel->SetThrusterGroupLevel(thg, 0.0);
}

// --------------------------------------------------------------

double AscentAP::StartMissionTime (double simt)
{
	t_launch = simt+SRB_STABILISATION_TIME;
	met_active = true;
	return t_launch;
}

// --------------------------------------------------------------

double AscentAP::GetMET (double simt) const
{
	return (vessel->status == 0 ? 0.0 : simt - t_launch);
}

// --------------------------------------------------------------

void AscentAP::SetDefaultProfiles ()
{
	int i;
	const int n_pitch = 20;
	double p_met[n_pitch] = { 0,  5,   10,   20,   30,   40,   50,   60,   70,   80,   90, 100,  120,  140,  164, 195, 250, 300,  420,  530};
	double p_val[n_pitch] = { 90, 90, 85.8, 76.8, 69.3, 62.8, 56.8, 51.8, 47.5, 42.7, 38.3, 34.8, 28.6, 22.7, 17.0, 12.3, 7.2, 3.8, -1.0, -5.0 };

	if (n_pitch_profile) delete []pitch_profile;
	n_pitch_profile = n_pitch;
	pitch_profile = new ProfSample[n_pitch];
	for (i = 0; i < n_pitch; i++) {
		pitch_profile[i].t = p_met[i];
		pitch_profile[i].v = p_val[i]*RAD;
	}

	launch_azimuth = PI05;
	tgt_alt = 350e3;
	t_roll_upright = 345.0;
}

// --------------------------------------------------------------

double AscentAP::SSMEThrustProfile(double met)
{
	const int nsample = 5;
	static const double ts[nsample] = {
		0, 35, 42, 70, 77
	};
	static const double lvls[nsample] = {
		1, 1, 0.67, 0.67, 1
	};

	if (vessel->status == 0) { // pre-launch
		if (met < 0) {
			return min(1.0, (SRB_STABILISATION_TIME + met)*0.4); // power up for liftoff
		}
	}
	else if (vessel->status < 3) {
		int i;
		if (met < ts[nsample - 1]) {
			for (i = nsample - 1; i >= 0; i--)
				if (met >= ts[i])
					return (met - ts[i]) / (ts[i + 1] - ts[i]) * (lvls[i + 1] - lvls[i]) + lvls[i];
			return lvls[0];
		}
		else if (met < 400) {
			return 1.0;
		}
		else {
			const double acc_tgt = 29.5;
			const double a = -0.05;
			const double b = -0.01;
			double th = -1.0;
			double spd = vessel->GetGroundspeed();
			double t = oapiGetSimTime();
			if (pt > 0.0) {
				double dt = t - pt;
				if (dt > 0.0)
					acc = (spd - pspd) / dt;
				if (pacc_valid) {
					double dacc = acc - pacc;
					if (dt > 0.0)
						dacc_dt = dacc / dt;
					double dth = a * (acc - acc_tgt) + b * dacc_dt;
					th = max(0.0, min(1.0, vessel->GetThrusterGroupLevel(THGROUP_MAIN) + dth*dt));
				}
			}
			pt = t;
			pspd = spd;
			pacc = acc;
			pacc_valid = true;
			return th;
		}
	}
	return 0.0;
}

// --------------------------------------------------------------

void AscentAP::SetLaunchAzimuth (double azimuth)
{
	launch_azimuth = azimuth;

	// current launch location in local planet frame
	VECTOR3 pos, equ, dir, nml, ne, nd;
	double lng, lat, rad;
	double slng, clng, slat, clat;
	double saz = sin(azimuth), caz = cos(azimuth);
	OBJHANDLE hRef = vessel->GetGravityRef();
	vessel->GetGlobalPos(pos);
	oapiGlobalToLocal (hRef, &pos, &equ);
	oapiLocalToEqu (hRef, equ, &lng, &lat, &rad);
	slng = sin(lng), clng = cos(lng), slat = sin(lat), clat = cos(lat);
	normalise(equ); // unit radius vector
	
	// launch direction in local planet frame
	dir = _V(-clng*slat*caz - slng*saz, clat*caz, -slng*slat*caz + clng*saz);

	// normal of orbital plane in local planet frame
	nml = crossp(dir, equ);

	// normal of equator plane in local planet frame
	ne = _V(0,1,0);

	// direction of ascending node
	nd = unit (crossp(nml, ne));

	// orbit inclination
	tgt.inc = acos(dotp(nml, ne));

	// longitude of ascending node
	tgt.lan = atan2(nd.z, nd.x);

	// rotation matrix from equator plane to target orbit plane
	double sinc = sin(tgt.inc), cinc = cos(tgt.inc);
	double slan = sin(tgt.lan), clan = cos(tgt.lan);
	MATRIX3 R1 = _M(1,0,0, 0,cinc,sinc, 0,-sinc,cinc);
	MATRIX3 R2 = _M(clan,0,-slan, 0,1,0, slan,0,clan);
	tgt.R = mul(R2,R1);
}

// --------------------------------------------------------------

double AscentAP::CalcTargetAzimuth () const
{
	if (!vessel->status) return launch_azimuth;

	VECTOR3 pos, equ, ep, dir, hdir;
	MATRIX3 pR, vR;
	const OBJHANDLE hRef = vessel->GetGravityRef();
	oapiGetRotationMatrix (hRef, &pR);
	vessel->GetGlobalPos(pos);
	oapiGlobalToLocal (hRef, &pos, &equ); // vessel position in planet frame
	normalise(equ);
	ep = tmul(tgt.R,equ);               // rotate to equator plane
	double elng = atan2(ep.z, ep.x);    // longitude of rotated position
	dir = _V(-sin(elng),0,cos(elng));   // rotated target direction
	dir = mul(tgt.R,dir);               // target direction in planet frame
	dir = mul(pR, dir);                 // target direction in global frame
	vessel->GetRotationMatrix (vR);
	dir = tmul (vR, dir);               // target direction in vessel frame
	vessel->HorizonRot (dir, hdir);     // target direction in local horizon frame
	double az = atan2 (hdir.x,hdir.z);  // target azimuth

	if (vessel->status < 3 && met >= t_roll_upright) { // compensate for SSME tilt during roll to avoid azimuth deviation
		const double pitch_ofs = 15.1*RAD;
		double bank = vessel->GetBank();
		az -= sin(bank)*pitch_ofs;
	}
	return az;
}

// --------------------------------------------------------------

double AscentAP::CalcTargetPitch () const
{
	if (!vessel->status) return PI05;

	double tgt_pitch;
	if (met > pitch_profile[n_pitch_profile-1].t) {
		tgt_pitch = pitch_profile[n_pitch_profile-1].v;
	} else {
		int i;
		for (i = 0; i < n_pitch_profile-1 && pitch_profile[i+1].t < met; i++);
		tgt_pitch = pitch_profile[i].v +
			(pitch_profile[i+1].v - pitch_profile[i].v) * (met-pitch_profile[i].t) / (pitch_profile[i+1].t - pitch_profile[i].t);
	}
	if (met >= t_roll_upright) {
		const double pitch_ofs = 15.1*RAD;
		double bank = vessel->GetBank();
		tgt_pitch += (cos(bank)+1)*pitch_ofs;
	}
	return tgt_pitch;
}

// --------------------------------------------------------------

double AscentAP::GetInclination (double lat, double az) const
{
	double a = PI05-lat;
	double B = az;
	return PI05 - asin(sin(a)*sin(B));
}

// --------------------------------------------------------------

double AscentAP::GetTargetInclination ()
{
	double a=0.0, B=0.0;
	if (vessel->status == 0) {
		if (!launch_lat && !launch_lng) {
			double r;
			vessel->GetEquPos(launch_lng, launch_lat, r);
		}
		a = PI05-launch_lat;

		// correct launch azimuth for surface rotation
		const OBJHANDLE hRef = vessel->GetGravityRef();
		double R = oapiGetSize(hRef);           // planet mean radius
		double r = R + tgt_alt;                 // target orbit radius
		double M = oapiGetMass (hRef);          // reference body mass
		double v0 = sqrt(GGRAV*M/r);            // target orbit speed
		double vg = PI2*R/oapiGetPlanetPeriod(hRef)*cos(launch_lat);
		                                        // surface speed at launch position
		double vx0 = v0*sin(launch_azimuth);    // longitudinal velocity component
		double vx1 = vx0 + vg;                  // corrected for planet rotation
		double vy  = v0*cos(launch_azimuth);    // latitudinal velocity component
		B = atan2(vx1,vy);                      // effective launch azimuth
	}
	return PI05 - asin(sin(a)*sin(B));
}

// --------------------------------------------------------------

void AscentAP::GetTargetDirection (double met, VECTOR3 &dir, double &tgt_hdg) const
{
	tgt_hdg = tgt.az;
	double tgt_pitch = tgt.pitch;
	double xz = cos(tgt_pitch);

	vessel->HorizonInvRot(_V(xz*sin(tgt_hdg), sin(tgt_pitch), xz*cos(tgt_hdg)), dir);
}

// --------------------------------------------------------------

void AscentAP::GetTargetRate (double met, VECTOR3 &rate) const
{
	if (active) {
		const double pitch_ofs = 15.1*RAD;
		double tgt_hdg;

		rate.x = rate.y = rate.z = 0.0;
		if (met <= 5.0) return;

		VECTOR3 tgtdir, avel;
		GetTargetDirection (met, tgtdir, tgt_hdg);
		vessel->GetAngularVel (avel);

		double dpitch = -asin(tgtdir.y);
		double dyaw   = -atan2(tgtdir.x, tgtdir.z);
		rate.x = GetTargetPitchRate (dpitch, avel.x);
		rate.y = (met < 35.0 ? 0.0 : GetTargetYawRate (dyaw, avel.y));
		rate.z = (met <= 35.0 ? GetTargetRollRate (tgt_hdg, true) :
				                GetTargetRollRate (met <= t_roll_upright ? PI : 0, false));
	} else {
		rate.x = rate.y = rate.z = 0.0;
	}
}

// --------------------------------------------------------------

void AscentAP::ToggleOMS2()
{
	do_oms2 = !do_oms2;
}

// --------------------------------------------------------------

double AscentAP::GetTargetPitchRate (double dpitch, double vpitch) const
{
	const double a = -0.15;
	const double b =  0.15;
	if      (dpitch >= PI) dpitch -= PI2;
	else if (dpitch < -PI) dpitch += PI2;
	double bank = vessel->GetBank();
	return a*dpitch + b*vpitch;

}

// --------------------------------------------------------------

double AscentAP::GetTargetYawRate (double dyaw, double vyaw) const
{
	const double a = 0.10;
	const double b = 0.10;
	if      (dyaw >= PI) dyaw -= PI2;
	else if (dyaw < -PI) dyaw += PI2;
	return a*dyaw + b*vyaw;
}

// --------------------------------------------------------------

double AscentAP::GetTargetRollRate (double tgt, bool tgt_is_heading) const
{
	double a, b, maxrate;
	if (tgt_is_heading) { // launch roll
		a = 0.60;
		b = 0.30;
		maxrate = 0.25;
	} else {              // post launch roll
		a = 0.15;
		b = 0.075;
		maxrate = 0.15;
	}

	VECTOR3 avel, yh;
	vessel->GetAngularVel (avel);
	double dh, droll = avel.z;

	if (tgt_is_heading) {
		vessel->HorizonRot (_V(0,1,0), yh);
		double yhdg = atan2(yh.x, yh.z);
		dh = yhdg-tgt;
		if (dh > PI) dh -= PI2;
		else if (dh < -PI) dh += PI2;
	} else {
		double bank = vessel->GetBank();
		dh = bank-tgt;
		if (dh >= PI) dh -= PI2;
		else if (dh < -PI) dh += PI2;
	}

	double rate = min (maxrate, max (-maxrate, a*dh + b*droll));
	
	return rate;
}

// --------------------------------------------------------------

void AscentAP::SaveState (FILEHANDLE scn)
{
	char cbuf[256];
	sprintf (cbuf, "%0.3f %0.3f %0.3f %0.3f",
		met, met_meco, met_oms_start, met_oms_end);
	oapiWriteScenario_string (scn, "MET", cbuf);
	sprintf (cbuf, "%d %d %d %0.0f %0.4f %0.5f %0.5f",
		(int)active, (int)met_active, (int)do_oms2, tgt_alt, launch_azimuth, launch_lng, launch_lat);
	oapiWriteScenario_string (scn, "ASCENTAP", cbuf);
}

// --------------------------------------------------------------

bool AscentAP::ParseScenarioLine (const char *line)
{
	if (!_strnicmp(line, "MET ", 4)) {
		sscanf(line+4, "%lf%lf%lf%lf", &met, &met_meco, &met_oms_start, &met_oms_end);
		t_launch = oapiGetSimTime()-met;
		return true;
	} else if (!_strnicmp(line, "ASCENTAP", 8)) {
		int i1, i2, i3;
		sscanf(line+9, "%d%d%d%lf%lf%lf%lf", &i1, &i2, &i3, &tgt_alt, &launch_azimuth, &launch_lng, &launch_lat);
		active = (bool)i1;
		met_active = (bool)i2;
		do_oms2 = (bool)i3;
		return true;
	}
	return false;
}


// ==============================================================
// class AscentApMfd: MFD interface for ascent autopilot
// ==============================================================

AscentApMfd::AscentApMfd (DWORD w, DWORD h, VESSEL *v)
: MFD2 (w, h, v)
{
	ap = ((Atlantis*)v)->AscentAutopilot();
	ap->SetLaunchAzimuth (ap->GetLaunchAzimuth());
	cpg = 0;
	pen[0] = oapiCreatePen(1, 1, RGB(96,96,96));
}

// --------------------------------------------------------------

AscentApMfd::~AscentApMfd ()
{
	oapiReleasePen (pen[0]);
}

// --------------------------------------------------------------

bool AscentApMfd::Update (oapi::Sketchpad *skp)
{
	char cbuf[256];
	sprintf (cbuf, "Ascent P%d/4       MET:%s", cpg+1, MetStr(ap->met));
	Title (skp, cbuf);

	if (ap->Active()) {
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor(0xffffff);
		skp->SetTextColor(0x000000);
		skp->Text ((27*cw)/2, 0, "ACT", 3);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
	}
	skp->SetTextColor(0x00ff00);

	switch (cpg) {
		case 0: UpdatePg_Prm(skp); break;
		case 1: UpdatePg_Gbl(skp); break;
	}

	return true;
}

// --------------------------------------------------------------

void AscentApMfd::UpdatePg_Prm (oapi::Sketchpad *skp)
{
	char cbuf[256];

	if (!ap->GetVessel()->status) {
		sprintf (cbuf, "Launch azimuth: %0.1fº", ap->GetLaunchAzimuth()*DEG);
		skp->Text (cw/2, (ch*3)/2, cbuf, strlen(cbuf));
		sprintf (cbuf, "Orbit inc:      %0.1fº", ap->GetTargetInclination()*DEG);
		skp->Text (cw/2, (ch*5)/2, cbuf, strlen(cbuf));
		sprintf (cbuf, "Orbit altitude: %0.1fkm", ap->GetOrbitAltitude()*1e-3);
		skp->Text (cw/2, (ch*7)/2, cbuf, strlen(cbuf));
		sprintf (cbuf, "OMS2 scheduled: %s", ap->GetOMS2Schedule() ? "yes" : "no");
		skp->Text (cw/2, (ch*9)/2, cbuf, strlen(cbuf));
	} else {
		OBJHANDLE hRef = ap->GetVessel()->GetGravityRef();
		double R = oapiGetSize(hRef);
		double az_tgt = ap->GetTargetAzimuth();
		double az_cur = ap->GetVessel()->GetYaw();
		double az_err = fabs(az_cur-az_tgt);
		if (az_err > PI) az_err = PI2-az_err;
		if (az_cur < az_tgt) az_err = -az_err;
		double pt_tgt = ap->GetTargetPitch();
		double pt_cur = ap->GetVessel()->GetPitch();
		double pt_err = pt_cur-pt_tgt;
		double alt_tgt = ap->GetOrbitAltitude();
		double alt_ap_cur, alt_pe_cur;
		ap->GetVessel()->GetApDist(alt_ap_cur);
		ap->GetVessel()->GetPeDist(alt_pe_cur);
		alt_ap_cur -= R; alt_pe_cur -= R;
		skp->Text (cw*13, ch*2, "Cur    Tgt    D", 15);
		skp->Text (cw/2, ch*3, "Azimuth [º]", 11);
		sprintf (cbuf, "%0.1lf", az_cur*DEG);
		skp->Text (cw*13, ch*3, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1lf", az_tgt*DEG);
		skp->Text (cw*20, ch*3, cbuf, strlen(cbuf));
		sprintf (cbuf, "%+0.2f", az_err*DEG);
		skp->Text (cw*27, ch*3, cbuf, strlen(cbuf));
		skp->Text (cw/2, ch*4, "Pitch [º]", 9);
		sprintf (cbuf, "%0.1lf", pt_cur*DEG);
		skp->Text (cw*13, ch*4, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1lf", pt_tgt*DEG);
		skp->Text (cw*20, ch*4, cbuf, strlen(cbuf));
		sprintf (cbuf, "%+0.2lf", pt_err*DEG);
		skp->Text (cw*27, ch*4, cbuf, strlen(cbuf));
		skp->Text (cw/2, ch*5, "Ap.Alt [km]", 11);
		sprintf (cbuf, "% 0.1lf", alt_ap_cur*1e-3);
		skp->Text (cw*12, ch*5, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1lf", alt_tgt*1e-3);
		skp->Text (cw*20, ch*5, cbuf, strlen(cbuf));
		sprintf (cbuf, "%+0.2lf", (alt_ap_cur-alt_tgt)*1e-3);
		skp->Text (cw*27, ch*5, cbuf, strlen(cbuf));
		skp->Text (cw/2, ch*6, "Pe.Alt [km]", 11);
		sprintf (cbuf, "% 0.1lf", alt_pe_cur*1e-3);
		skp->Text (cw*12, ch*6, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1lf", alt_tgt*1e-3);
		skp->Text (cw*20, ch*6, cbuf, strlen(cbuf));
		sprintf (cbuf, "%+0.2lf", (alt_pe_cur-alt_tgt)*1e-3);
		skp->Text (cw*27, ch*6, cbuf, strlen(cbuf));

		if (ap->GetVessel()->status == 3) {
			if (ap->schedule_oms1 > 0.0) {
			} else if (ap->met_oms1_start > 0.0) {
				skp->Text(cw / 2, ch * 8, "OMS-1", 5);
			} else if (ap->do_oms2) {
				if (ap->met_oms_start < 0.0) {
					double dt = ap->schedule_oms;
					sprintf(cbuf, "OMS-2: MET%+0.0lf", dt);
					skp->Text(cw / 2, ch * 8, cbuf, strlen(cbuf));
				}
				else if (ap->met_oms_end < 0.0)
					skp->Text(cw / 2, ch * 8, "OMS-2", 5);
			}
		}
	}
}

// --------------------------------------------------------------

void AscentApMfd::UpdatePg_Gbl (oapi::Sketchpad *skp)
{
	double pitch, yaw;
	int i;
	int iW = (int)W;
	int s2 = iW/16;
	int s1 = s2*2;
	int cx = iW/2;
	int cy = ch*3+s1;
	int ssme_cx[3] = {(2*iW)/10, (8*iW)/10, cx};
	int ssme_cy[3] = {cy+(3*s1)/2, cy+(3*s1)/2, cy};

	skp->SetTextAlign (oapi::Sketchpad::CENTER);
	skp->Text (W/2, cy-s1-(3*ch)/2, "Gimbal SSME", 11);
	for (i = 0; i < 3; i++) {
		ap->GetVessel()->GetSSMEGimbalPos (i, pitch, yaw);
		DrawGimbal (skp, ssme_cx[i], ssme_cy[i], pitch, yaw);
	}

	if (ap->GetVessel()->status < 2) {
		int srb_cy = H-s1-ch;
		int srb_cx[2] = {(2*iW)/10, (8*iW)/10};
		skp->Line (0, srb_cy-s1-ch*2, W, srb_cy-s1-ch*2);
		skp->Text (W/2, srb_cy-s1-(3*ch)/2, "Gimbal SRB", 10);
		for (i = 0; i < 2; i++) {
			ap->GetVessel()->GetSRBGimbalPos (i, pitch, yaw);
			DrawGimbal (skp, srb_cx[i], srb_cy, pitch, yaw);
		}
	}
}

// --------------------------------------------------------------

void AscentApMfd::DrawGimbal (oapi::Sketchpad *skp, int cx, int cy, double pitch, double yaw)
{
	const int s2 = W/16;
	const int s1 = s2*2;
	const int s = s2/2;

	const double range = 10.5*RAD;
	int x, y;
	x = (int)(yaw/range * s1 + 0.5);
	y = (int)(pitch/range * s1 + 0.5);

	oapi::Pen *ppen = skp->SetPen (pen[0]);
	skp->Rectangle (cx-s1, cy-s1, cx+s1+1, cy+s1+1);
	skp->Rectangle (cx-s2, cy-s2, cx+s2+1, cy+s2+1);
	skp->Line (cx-s1, cy, cx+s1, cy);
	skp->Line (cx, cy-s1, cx, cy+s1);
	skp->SetPen (ppen);

	skp->Line (cx+x-s, cy+y, cx+x+s+1, cy+y);
	skp->Line (cx+x, cy+y-s, cx+x, cy+y+s+1);
}

// --------------------------------------------------------------

char *AscentApMfd::ButtonLabel (int bt)
{
	if (!bt)
		return (ap->Active() ? "DA" : ap->GetVessel()->status == 0 ? "L" : "EA");

	if (bt <= 2) {
		static char *label[2] = {"PG-", "PG+"};
		return label[bt-1];
	}

	switch (cpg) {
		case 0: {
			if (ap->Active() || ap->GetVessel()->status) return 0;
			static char *label[5] = {"AZ-", "AZ+", "AL-", "AL+", "OM2"};
			return (bt < 8 ? label[bt-3] : 0);
		}
	}

	return 0;
}

// --------------------------------------------------------------

int AscentApMfd::ButtonMenu (const MFDBUTTONMENU **menu) const
{
	static MFDBUTTONMENU mnu[8] = {
		{0,                 0, 'L'},
		{"Prev page"      , 0, ','},
		{"Next page"      , 0, '.'},
		{"Decrease Launch", "azimuth", ';'},
		{"Increase Launch", "azimuth", '\''},
		{"Decrease Target", "altitude", '-'},
		{"Increase Target", "altitude", '='},
		{"Schedule OMS2", "", 'O'}
	};
	if (!ap->Active()) {
		if (ap->GetVessel()->status == 0) {
			static const char *line = "Launch";
			mnu[0].line1 = line;
			mnu[0].selchar = 'L';
		} else {
			static const char *line = "Engage AP";
			mnu[0].line1 = line;
			mnu[0].selchar = 'E';
		}
	} else {
		static const char *line = "Disengage AP";
		mnu[0].line1 = line;
		mnu[0].selchar = 'D';
	}
	if (menu) *menu = mnu;
	return (cpg == 0 && !ap->Active() && ap->GetVessel()->status == 0 ? 8 : 3);
}

// --------------------------------------------------------------

bool AscentApMfd::ConsumeKeyBuffered (DWORD key)
{
	switch (key) {
	case OAPI_KEY_L:
		return OnLaunch();
	case OAPI_KEY_D:
		return OnDisengage();
	case OAPI_KEY_E:
		return OnEngage();
	case OAPI_KEY_COMMA:
		DecPage();
		return true;
	case OAPI_KEY_PERIOD:
		IncPage();
		return true;
	case OAPI_KEY_SEMICOLON:
		InitDecAzimuth();
		return true;
	case OAPI_KEY_APOSTROPHE:
		InitIncAzimuth();
		return true;
	case OAPI_KEY_MINUS:
		InitDecAltitude();
		return true;
	case OAPI_KEY_EQUALS:
		InitIncAltitude();
		return true;
	case OAPI_KEY_O:
		ToggleOMS2Schedule();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool AscentApMfd::ConsumeButton (int bt, int event)
{
	if (!bt) {
		if (event & PANEL_MOUSE_LBDOWN) {
			DWORD btkey = (ap->Active() ? OAPI_KEY_D : ap->GetVessel()->status == 0 ? OAPI_KEY_L : OAPI_KEY_E);
			return ConsumeKeyBuffered (btkey);
		}
	}

	if (bt < 3) {
		if (event & PANEL_MOUSE_LBDOWN) {
			static const DWORD btkey[2] = {OAPI_KEY_COMMA, OAPI_KEY_PERIOD};
			return ConsumeKeyBuffered (btkey[bt-1]);
		}
	}

	if (bt < 8 && cpg == 0 && !ap->Active() && ap->GetVessel()->status == 0) {
		static const DWORD btkey[5] = {OAPI_KEY_SEMICOLON, OAPI_KEY_APOSTROPHE, OAPI_KEY_MINUS, OAPI_KEY_EQUALS, OAPI_KEY_O};
		if (event & PANEL_MOUSE_LBDOWN) {
			return ConsumeKeyBuffered(btkey[bt-3]);
		} else if (event & PANEL_MOUSE_LBUP) {
			if (set_mode != MODE_NONE) {
				set_mode = MODE_NONE;
				return true;
			}
		} else if (event & PANEL_MOUSE_LBPRESSED) {
			switch(bt-3) {
			case 0: DecAzimuth(); return true;
			case 1: IncAzimuth(); return true;
			case 2: DecAltitude(); return true;
			case 3: IncAltitude(); return true;
			}
		}
	}

	return false;
}

// --------------------------------------------------------------

void AscentApMfd::DecPage ()
{
	const DWORD npage = 4;
	cpg = (cpg == 0 ? npage-3 : cpg-1);
	InvalidateButtons();
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::IncPage ()
{
	const DWORD npage = 4;
	cpg = (cpg == npage-1 ? 0 : cpg+1);
	InvalidateButtons();
	InvalidateDisplay();
}

// --------------------------------------------------------------

bool AscentApMfd::OnLaunch ()
{
	if (!ap->Active()) {
		if (ap->GetVessel()->status == 0) {
			ap->Launch();
			InvalidateButtons();
		}
	}
	return true;
}

// --------------------------------------------------------------

bool AscentApMfd::OnEngage ()
{
	if (!ap->Active()) {
		ap->Engage();
		InvalidateButtons();
	}
	return true;
}

// --------------------------------------------------------------

bool AscentApMfd::OnDisengage ()
{
	if (ap->Active()) {
		ap->Disengage();
		InvalidateButtons();
	}
	return true;
}

// --------------------------------------------------------------

void AscentApMfd::InitDecAzimuth ()
{
	set_mode = MODE_AZIMUTH_DEC;
	ref_t = oapiGetSysTime();
	ref_val = max(ap->GetLaunchAzimuth()-RAD*0.1, 0);
	ap->SetLaunchAzimuth(ref_val);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::InitIncAzimuth ()
{
	set_mode = MODE_AZIMUTH_INC;
	ref_t = oapiGetSysTime();
	ref_val = min(ap->GetLaunchAzimuth()+RAD*0.1, PI2);
	ap->SetLaunchAzimuth(ref_val);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::DecAzimuth()
{
	double dt = oapiGetSysTime()-ref_t;
	if (dt < 0.2) return;
	double da = -min(3.0,dt)*RAD*0.2;
	if (dt > 3.0)
		da -= min(dt-3.0,3.0)*RAD*2.0;
	if (dt > 6.0)
		da -= (dt-6.0)*RAD*20.0;
	double az = max(ref_val + da, 0);
	ap->SetLaunchAzimuth (az);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::IncAzimuth()
{
	double dt = oapiGetSysTime()-ref_t;
	if (dt < 0.2) return;
	double da = min(3.0,dt)*RAD*0.2;
	if (dt > 3.0)
		da += min(dt-3.0,3.0)*RAD*2.0;
	if (dt > 6.0)
		da += (dt-6.0)*RAD*20.0;
	double az = min(ref_val + da, PI2);
	ap->SetLaunchAzimuth (az);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::InitDecAltitude ()
{
	set_mode = MODE_AZIMUTH_DEC;
	ref_t = oapiGetSysTime();
	ref_val = max(ap->GetOrbitAltitude()-100, 0);
	ap->SetOrbitAltitude(ref_val);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::InitIncAltitude ()
{
	set_mode = MODE_AZIMUTH_INC;
	ref_t = oapiGetSysTime();
	ref_val = ap->GetOrbitAltitude()+100;
	ap->SetOrbitAltitude(ref_val);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::DecAltitude()
{
	double dt = oapiGetSysTime()-ref_t;
	if (dt < 0.2) return;
	double da = -min(3.0,dt)*1e3*0.2;
	if (dt > 3.0)
		da -= min(dt-3.0,3.0)*1e3*2.0;
	if (dt > 6.0)
		da -= (dt-6.0)*1e3*20.0;
	double alt = max(ref_val + da, 0);
	ap->SetOrbitAltitude (alt);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::IncAltitude()
{
	double dt = oapiGetSysTime()-ref_t;
	if (dt < 0.2) return;
	double da = min(3.0,dt)*1e3*0.2;
	if (dt > 3.0)
		da += min(dt-3.0,3.0)*1e3*2.0;
	if (dt > 6.0)
		da += (dt-6.0)*1e3*20.0;
	double alt = ref_val + da;
	ap->SetOrbitAltitude (alt);
	InvalidateDisplay();
}

// --------------------------------------------------------------

void AscentApMfd::ToggleOMS2Schedule ()
{
	ap->ToggleOMS2();
	InvalidateDisplay();
}

// --------------------------------------------------------------

OAPI_MSGTYPE AscentApMfd::MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case OAPI_MSG_MFD_OPENED:
		return (OAPI_MSGTYPE)(new AscentApMfd (LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam));
	}
	return 0;
}


// ==============================================================
// class AscentAPDlg: dialog interface for ascent autopilot
// ==============================================================

AscentAPDlg::AscentAPDlg (AscentAP *_ap): TabbedDialog (IDD_ASCENTAP, IDC_TAB1)
{
	ap = _ap;
}

// --------------------------------------------------------------

AscentAPDlg::~AscentAPDlg ()
{
	Close();
}

// --------------------------------------------------------------

void AscentAPDlg::Update (double simt)
{
	if (DlgHandle()) {
		static char title[64] = "Atlantis Ascent Autopilot | MET ";
		strcpy (title+32, MetStr (ap->met)); 
		SetWindowText (DlgHandle(), title);
		for (int i = 0; i < TabCount(); i++)
			Tab(i)->Update (simt);
	}
}

// --------------------------------------------------------------

int AscentAPDlg::OnInitDialog (WPARAM wParam)
{
	AddTab (new AscentAPDlgTabControl (this), "Control");
	AddTab (new AscentAPDlgTabGimbal (this), "Gimbal");
	AddTab (new AscentAPDlgTabThrust (this), "Thrust");
	AddTab (new AscentAPDlgTabAltitude (this), "Altitude");
	return TabbedDialog::OnInitDialog (wParam);
}

// --------------------------------------------------------------

int AscentAPDlg::Closed ()
{
	ap->vessel->DestroyAscentAPDlg();
	return TRUE;
}


// ==============================================================
// class AscentAPDlgTab: base class for dialog tabs
// ==============================================================

AscentAPDlgTab::AscentAPDlgTab (AscentAPDlg *frame, int dlgId)
: TabPage (frame, dlgId)
{
	ap = frame->AP();
}


// ==============================================================
// class AscentAPDlgTabControl: AP control tab
// ==============================================================

AscentAPDlgTabControl::AscentAPDlgTabControl (AscentAPDlg *frame)
: AscentAPDlgTab (frame, IDD_ASCENTAP_CTRL)
{
}

// --------------------------------------------------------------

int AscentAPDlgTabControl::OnInitTab (WPARAM wParam)
{
	char cbuf[256];
	sprintf (cbuf, "%0.1f", ap->GetLaunchAzimuth()*DEG);
	SetWindowText (GetDlgItem (TabHandle(), IDC_AZIMUTH), cbuf);
	sprintf (cbuf, "%0.1f", ap->GetOrbitAltitude()*1e-3);
	SetWindowText (GetDlgItem (TabHandle(), IDC_ALT), cbuf);
	if (ap->Active())
		SetWindowText (GetDlgItem (TabHandle(), IDC_LAUNCH), "Disengage AP");
	else if (ap->GetVessel()->status == 0)
		SetWindowText (GetDlgItem (TabHandle(), IDC_LAUNCH), "Launch");
	else
		SetWindowText (GetDlgItem (TabHandle(), IDC_LAUNCH), "Engage AP");

	return TRUE;
}

// --------------------------------------------------------------

int AscentAPDlgTabControl::OnLaunch ()
{
	if (!ap->Active()) {
		if (ap->GetVessel()->status == 0) {
			char cbuf[256];
			double azimuth, alt;
			GetWindowText (GetDlgItem (TabHandle(), IDC_AZIMUTH), cbuf, 256);
			sscanf (cbuf, "%lf", &azimuth);
			azimuth *= RAD;
			GetWindowText (GetDlgItem (TabHandle(), IDC_ALT), cbuf, 256);
			EnableWindow(GetDlgItem (TabHandle(), IDC_AZIMUTH), FALSE);
			EnableWindow(GetDlgItem (TabHandle(), IDC_ALT), FALSE);
			sscanf (cbuf, "%lf", &alt);
			alt *= 1e3;
			ap->SetLaunchAzimuth(azimuth);
			ap->SetOrbitAltitude(alt);
			ap->Launch ();
		} else {
			ap->Engage();
		}
		SetWindowText (GetDlgItem (TabHandle(), IDC_LAUNCH), "Disengage AP");
	} else {
		ap->Disengage();
		SetWindowText (GetDlgItem (TabHandle(), IDC_LAUNCH), "Engage AP");
	}
	return TRUE;
}

// --------------------------------------------------------------

int AscentAPDlgTabControl::OnCommand (WPARAM wParam, LPARAM lParam)
{
	switch (LOWORD(wParam)) {
	case IDC_LAUNCH:
		return OnLaunch();
	}
	return TabPage::OnCommand (wParam, lParam);
}


// ==============================================================
// class AscentAPDlgTabGimbal: AP gimbal tab
// ==============================================================

AscentAPDlgTabGimbal::AscentAPDlgTabGimbal (AscentAPDlg *frame)
: AscentAPDlgTab (frame, IDD_ASCENTAP_GIMBAL)
{
	pen1 = CreatePen (PS_SOLID, 0, 0xB0B0B0);
	pen2 = CreatePen (PS_SOLID, 0, 0x0000FF);
}

// --------------------------------------------------------------

AscentAPDlgTabGimbal::~AscentAPDlgTabGimbal ()
{
	DeleteObject (pen1);
	DeleteObject (pen2);
}

// --------------------------------------------------------------

INT_PTR AscentAPDlgTabGimbal::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG: {
		RECT rect;
		GetClientRect (GetDlgItem (hWnd, IDC_SSME_L), &rect);
		rad = min (rect.right-rect.left, rect.bottom-rect.top)*0.5;
		} return TRUE;
	case WM_PAINT:
		RepaintAll (hWnd);
		break;
	}
	return FALSE;
}

// --------------------------------------------------------------

void AscentAPDlgTabGimbal::Update (double simt)
{
	const double range = 10.5*RAD;
	double pitch, yaw;
	int i;
	int DlgId[5] = {IDC_SSME_L, IDC_SSME_R, IDC_SSME_U, IDC_SRB_L, IDC_SRB_R};
	for (i = 0; i < 5; i++) {
		HWND hCtrl = GetDlgItem (TabHandle(), DlgId[i]);
		if (i < 3)
			ap->GetVessel()->GetSSMEGimbalPos (i, pitch, yaw);
		else
			ap->GetVessel()->GetSRBGimbalPos (i-3, pitch, yaw);
		UpdateGimbalCross (hCtrl, i, pitch, yaw);
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabGimbal::UpdateGimbalCross (HWND hCtrl, int idx, double pitch, double yaw)
{
	const double range = 10.5*RAD;
	int x, y;
	x = (int)(yaw/range * rad + 0.5);
	y = (int)(pitch/range * rad + 0.5);
	if (x != gimbalx[idx] || y != gimbaly[idx]) {
		HDC hDC = GetDC (hCtrl);
		RECT rect;
		GetClientRect (hCtrl, &rect);
		int cntx = (rect.left+rect.right)/2;
		int cnty = (rect.top+rect.bottom)/2;
		HPEN ppen = (HPEN)SelectObject (hDC, GetStockObject (WHITE_PEN));
		SelectObject (hDC, GetStockObject (NULL_BRUSH));
		PaintGimbalCross (hDC, rect, gimbalx[idx], gimbaly[idx]);
		SelectObject (hDC, pen1);
		MoveToEx (hDC, rect.left, cnty, NULL); LineTo (hDC, rect.right, cnty);
		MoveToEx (hDC, cntx, rect.top, NULL); LineTo (hDC, cntx, rect.bottom);
		Rectangle (hDC, (rect.left+cntx)/2, (rect.top+cnty)/2, (rect.right+cntx)/2, (rect.bottom+cnty)/2);
		SelectObject (hDC, GetStockObject (BLACK_PEN));
		Rectangle (hDC, rect.left, rect.top, rect.right, rect.bottom);
		SelectObject (hDC, pen2);
		PaintGimbalCross (hDC, rect, gimbalx[idx]=x, gimbaly[idx]=y);
		SelectObject (hDC, ppen);
		ReleaseDC (hCtrl, hDC);
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabGimbal::PaintGimbalCross (HDC hDC, const RECT &rect, int x, int y)
{
	int xmin, xmax, ymin, ymax, cntx, cnty;
	xmin = rect.left, xmax = rect.right;
	ymin = rect.top, ymax = rect.bottom;
	cntx = (xmin+xmax)/2;
	cnty = (ymin+ymax)/2;
	x += cntx, y += cnty;
	if (x >= xmin && x < xmax) {
		MoveToEx (hDC, x, max(y-10, ymin), NULL);
		LineTo (hDC, x, min(y+11, ymax));
	}
	if (y >= ymin && y < ymax) {
		MoveToEx (hDC, max(x-10, xmin), y, NULL);
		LineTo (hDC, min(x+11,xmax), y);
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabGimbal::RepaintAll (HWND hWnd)
{
	int DlgId[5] = {IDC_SSME_L, IDC_SSME_R, IDC_SSME_U, IDC_SRB_L, IDC_SRB_R};
	for (int i = 0; i < 5; i++) {
		HWND hCtrl = GetDlgItem (hWnd, DlgId[i]);
		InvalidateRect (hCtrl, NULL, FALSE);
		UpdateWindow (hCtrl);
		PaintGimbalBox (hCtrl);
		gimbalx[i] = gimbaly[i] = 0;
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabGimbal::PaintGimbalBox (HWND hWnd)
{
	RECT rect;
	int cntx, cnty;
	HDC hDC = GetDC (hWnd);
	GetClientRect (hWnd, &rect);
	cntx = (rect.right+rect.left)/2;
	cnty = (rect.bottom+rect.top)/2;
	SelectObject (hDC, GetStockObject (WHITE_BRUSH));
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	Rectangle (hDC, rect.left, rect.top, rect.right, rect.bottom);
	SelectObject (hDC, pen1);
	MoveToEx (hDC, rect.left, cnty, NULL); LineTo (hDC, rect.right, cnty);
	MoveToEx (hDC, cntx, rect.top, NULL); LineTo (hDC, cntx, rect.bottom);
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	ReleaseDC (hWnd, hDC);
}


// ==============================================================
// class AscentAPDlgTabThrust: AP thrust tab
// ==============================================================

AscentAPDlgTabThrust::AscentAPDlgTabThrust (AscentAPDlg *frame)
: AscentAPDlgTab (frame, IDD_ASCENTAP_THRUST)
{
	Graph::InitGDI ();
	ssmegraph = new Graph(1);
	ssmegraph->SetTitle ("SSME thrust");
	ssmegraph->SetYLabel ("Thrust [%]");
	srbgraph = new Graph(1);
	srbgraph->SetTitle ("SRB thrust");
	srbgraph->SetYLabel ("Thrust [%]");
	updt = oapiGetSimTime();
	dupdt = 1.0;
}

// --------------------------------------------------------------

AscentAPDlgTabThrust::~AscentAPDlgTabThrust ()
{
	delete ssmegraph;
	delete srbgraph;
	Graph::FreeGDI();
}

// --------------------------------------------------------------

void AscentAPDlgTabThrust::Update (double simt)
{
	if (ap->Active() && simt >= updt) {
		double lvl;
		lvl = ap->GetVessel()->GetThrusterGroupLevel(THGROUP_MAIN);
		ssmegraph->AppendDataPoint ((float)lvl);
		lvl = ap->GetVessel()->GetSRBThrustLevel(0);
		srbgraph->AppendDataPoint ((float)lvl);
		RefreshGraph (ssmegraph, IDC_SSMETHRUST);
		RefreshGraph (srbgraph, IDC_SRBTHRUST);
		updt += dupdt;
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabThrust::RefreshGraph (Graph *graph, int GraphId)
{

	HWND hCtrl = GetDlgItem (TabHandle(), GraphId);
	InvalidateRect (hCtrl, NULL, TRUE);
	UpdateWindow (hCtrl);
	RECT rect;
	HDC hDC = GetDC (hCtrl);
	GetClientRect (hCtrl, &rect);
	graph->Refresh (hDC, rect.right-rect.left, rect.bottom-rect.top);
	ReleaseDC (hCtrl, hDC);
}

// --------------------------------------------------------------

int AscentAPDlgTabThrust::OnPaint ()
{
	RefreshGraph (ssmegraph, IDC_SSMETHRUST);
	RefreshGraph (srbgraph, IDC_SRBTHRUST);
	return FALSE;
}

// --------------------------------------------------------------

INT_PTR AscentAPDlgTabThrust::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT:
		return OnPaint ();
	}
	return FALSE;
}


// ==============================================================
// class AscentAPDlgTabAltitude: AP altitude tab
// ==============================================================

AscentAPDlgTabAltitude::AscentAPDlgTabAltitude (AscentAPDlg *frame)
: AscentAPDlgTab (frame, IDD_ASCENTAP_ALT)
{
	Graph::InitGDI ();
	altgraph = new Graph(1);
	altgraph->SetTitle ("Altitude");
	altgraph->SetYLabel ("alt [km]");
	updt = oapiGetSimTime();
	dupdt = 1.0;
}

// --------------------------------------------------------------

AscentAPDlgTabAltitude::~AscentAPDlgTabAltitude ()
{
	delete altgraph;
	Graph::FreeGDI();
}

// --------------------------------------------------------------

void AscentAPDlgTabAltitude::Update (double simt)
{
	if (ap->Active() && simt >= updt) {
		float alt;
		alt = (float)(ap->GetVessel()->GetAltitude()*1e-3);
		altgraph->AppendDataPoint (alt);
		RefreshGraph (altgraph, IDC_ALTITUDE);
		updt += dupdt;
	}
}

// --------------------------------------------------------------

void AscentAPDlgTabAltitude::RefreshGraph (Graph *graph, int GraphId)
{

	HWND hCtrl = GetDlgItem (TabHandle(), GraphId);
	InvalidateRect (hCtrl, NULL, TRUE);
	UpdateWindow (hCtrl);
	RECT rect;
	HDC hDC = GetDC (hCtrl);
	GetClientRect (hCtrl, &rect);
	graph->Refresh (hDC, rect.right-rect.left, rect.bottom-rect.top);
	ReleaseDC (hCtrl, hDC);
}

// --------------------------------------------------------------

int AscentAPDlgTabAltitude::OnPaint ()
{
	RefreshGraph (altgraph, IDC_ALTITUDE);
	return FALSE;
}

// --------------------------------------------------------------

INT_PTR AscentAPDlgTabAltitude::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT:
		return OnPaint ();
	}
	return FALSE;
}

// ==============================================================
// auxiliary functions
// ==============================================================

const char *MetStr (double met)
{
	static char str[32];
	int h, m;
	if (met < 0.0) {
		str[0] = '-';
		met = -met;
	} else
		str[0] = ' ';
	h = (int)(met/3600.0);
	met -= (double)h*3600;
	m = (int)(met/60.0);
	met -= (double)m*60;
	int nh = (h < 100 ? 2:3);
	h = min(h,999);
	sprintf (str+1, "%0*d:%02d:%04.1f", nh, h, m, met);
	return str;
}

