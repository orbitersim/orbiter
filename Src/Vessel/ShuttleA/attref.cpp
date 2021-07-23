// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// attref.cpp
// AttitudeReference class implementation
// ==============================================================

#include "attref.h"

AttitudeReference::AttitudeReference (const VESSEL *vessel)
{
	v = vessel;
	projmode = 0;
	mode = 0;
	tgtmode = 0;
	navid = 0;
	valid_axes = false;
	valid_euler = false;
	valid_tgteuler = false;
	euler_offs = _V(0,0,0);
	tgt_offs = _V(0,0,0);
	tgt_rvel = _V(0,0,0);
	tgt_ppos = _V(0,0,0);
	tgt_ptime = 0;
}

// ==============================================================

void AttitudeReference::SetProjMode (int newmode)
{
	projmode = newmode;
	valid_axes = false;
	valid_euler = false;
	valid_tgteuler = false;
}

// ==============================================================

void AttitudeReference::SetMode (int newmode)
{
	mode = newmode;
	valid_axes = false;
	valid_euler = false;
	valid_tgteuler = false;
}

// ==============================================================

void AttitudeReference::SetTgtmode (int newmode)
{
	tgtmode = newmode;
}

// ==============================================================

void AttitudeReference::SetNavid (int newnavid)
{
	navid = newnavid;
	if (mode == 4) {
		valid_axes = false;
		valid_euler = false;
		valid_tgteuler = false;
	}
}

// ==============================================================

void AttitudeReference::SetEulerOffset (const VECTOR3 &ofs)
{
	for (int i = 0; i < 3; i++)
		euler_offs.data[i] = posangle (ofs.data[i]);
	valid_euler = false;
}

// ==============================================================

void AttitudeReference::SetTgtOffset (const VECTOR3 &ofs)
{
	for (int i = 0; i < 3; i++)
		tgt_offs.data[i] = posangle (ofs.data[i]);
}

// ==============================================================

const MATRIX3 &AttitudeReference::GetFrameRotMatrix () const
{
	// Returns rotation matrix for rotation from reference frame to global frame

	if (!valid_axes) {
		VECTOR3 axis1, axis2, axis3;
		switch (mode) {
		case 0:    // inertial (ecliptic)
			axis3 = _V(1,0,0);
			axis2 = _V(0,1,0);
			break;
		case 1: {  // inertial (equator)
			MATRIX3 R;
			oapiGetPlanetObliquityMatrix (v->GetGravityRef(), &R);
			//axis3 = _V(R.m13, R.m23, R.m33);
			axis3 = _V(R.m11, R.m21, R.m31);
			axis2 = _V(R.m12, R.m22, R.m32);
			} break;
		case 2: {  // orbital velocity / orbital momentum vector
			OBJHANDLE hRef = v->GetGravityRef();
			v->GetRelativeVel (hRef, axis3);
			axis3 = unit (axis3);
			VECTOR3 vv, vm;
			v->GetRelativePos (hRef, vv);    // local vertical
			vm = crossp (axis3,vv);    // direction of orbital momentum
			axis2 = unit (crossp (vm,axis3));
			} break;
		case 3: {  // local horizon / local north (surface)
			OBJHANDLE hRef = v->GetSurfaceRef();
			v->GetRelativePos (hRef, axis2);
			axis2 = unit (axis2);
			MATRIX3 prot;
			oapiGetRotationMatrix (hRef, &prot);
			VECTOR3 paxis = {prot.m12, prot.m22, prot.m32};  // planet rotation axis in global frame
			VECTOR3 yaxis = unit (crossp (paxis,axis2));      // direction of yaw=+90 pole in global frame
			axis3 = crossp (axis2,yaxis);
			} break;
		case 4: {  // synced to NAV source (type-specific)
			NAVDATA ndata;
			NAVHANDLE hNav = v->GetNavSource (navid);
			axis3 = _V(0,0,1);
			axis2 = _V(0,1,0);
			if (hNav) {
				oapiGetNavData (hNav, &ndata);
				switch (ndata.type) {
				case TRANSMITTER_IDS: {
					VECTOR3 pos, dir, rot;
					MATRIX3 R;
					VESSEL *vtgt = oapiGetVesselInterface (ndata.ids.hVessel);
					vtgt->GetRotationMatrix (R);
					vtgt->GetDockParams (ndata.ids.hDock, pos, dir, rot);
					axis3 = -mul(R,dir);
					axis2 = mul(R,rot);
					} break;
				case TRANSMITTER_VTOL:
				case TRANSMITTER_VOR: {
					OBJHANDLE hRef = v->GetSurfaceRef();
					VECTOR3 spos, npos;
					v->GetRelativePos (hRef, axis2);
					v->GetGlobalPos (spos);
					axis2 = unit (axis2);
					oapiGetNavPos (hNav, &npos);
					npos -= spos;
					axis3 = unit(crossp(crossp(axis2,npos),axis2));
					} break;
				}
			}
			} break;
		}
		axis1 = crossp(axis2,axis3);
		R = _M(axis1.x, axis2.x, axis3.x,  axis1.y, axis2.y, axis3.y,  axis1.z, axis2.z, axis3.z);

		valid_axes = true;
		valid_euler = false;
	}
	return R;
}

// ==============================================================

const VECTOR3 &AttitudeReference::GetEulerAngles () const
{
	if (!valid_euler) {
		// Update the axes of the reference frame
		const MATRIX3 &Rref = GetFrameRotMatrix();

		// Rotation matrix ship->global
		MATRIX3 srot;
		v->GetRotationMatrix (srot);

		// map ship's local axes into reference frame
		VECTOR3 shipx = {srot.m11, srot.m21, srot.m31};
		VECTOR3 shipy = {srot.m12, srot.m22, srot.m32};
		VECTOR3 shipz = {srot.m13, srot.m23, srot.m33};
		shipx = tmul (Rref, shipx);
		shipy = tmul (Rref, shipy);
		shipz = tmul (Rref, shipz);

		if (projmode == 0) {
			euler.x = atan2(shipx.y, shipy.y);  // roll angle
			euler.y = asin(shipz.y);            // pitch angle
			euler.z = atan2(shipz.x, shipz.z);  // yaw angle
		} else {
			euler.x = -atan2(shipy.x, shipx.x); // roll angle
			euler.y = atan2(shipz.y,shipz.z);   // pitch angle
			euler.z = asin(shipz.x);            // yaw angle
		}
		euler += euler_offs;
		for (int i = 0; i < 3; i++)
			euler.data[i] = posangle (euler.data[i]);

		valid_euler = true;
	}

	return euler;
}

// ==============================================================

bool AttitudeReference::GetTgtEulerAngles (VECTOR3 &tgt_euler) const
{
	if (!valid_tgteuler) {
		switch (tgtmode) {
			case 1:  // fixed
				tgteuler.y = tgteuler.z = 0.0;
				tgteuler += tgt_offs;
				have_tgteuler = true;
				break;
			case 2:  // direction of NAV source
			case 3: { // relative velocity of NAV source
				NAVHANDLE hNav;
				if (mode >= 4 && (hNav = v->GetNavSource (navid))) {
					VECTOR3 dir, sdir;
					if (tgtmode == 2) {
						oapiGetNavPos (hNav, &dir);
						v->GetGlobalPos (sdir);
						dir = tmul (GetFrameRotMatrix(), unit (dir-sdir));
					} else {
						v->GetGlobalVel (sdir);
						dir = tmul (GetFrameRotMatrix(), unit (tgt_rvel));
					}
					if (projmode == 0) {
						tgteuler.y = asin (dir.y);
						tgteuler.z = atan2 (dir.x, dir.z);
					} else {
						tgteuler.y = atan2 (dir.y, dir.z);
						tgteuler.z = asin (dir.x);
					}
					tgteuler += tgt_offs;
					//tgteuler += GetEulerAngles (projection_mode);
					// frame-relative --> ship-relative; is this a good idea?
					tgteuler.y = posangle(tgteuler.y);
					tgteuler.z = posangle(tgteuler.z);
					have_tgteuler = true;
				} else 
					have_tgteuler = false;
				} break;
			default:
				have_tgteuler = false;
				break;
		}
	}
	valid_tgteuler = true;
	tgt_euler = tgteuler;
	return have_tgteuler;;
}

// ==============================================================

void AttitudeReference::PostStep (double simt, double simdt, double mjd)
{
	valid_axes = false;
	valid_euler = false;
	valid_tgteuler = false;

	if (mode >= 4 && tgtmode == 3) {
		NAVHANDLE hNav = v->GetNavSource (navid);
		if (hNav) {
			VECTOR3 tvel,svel;
			v->GetGlobalVel (svel);
			NAVDATA data;
			OBJHANDLE hObj = NULL;
			oapiGetNavData (hNav, &data);
			switch (data.type) {
				case TRANSMITTER_IDS:
					hObj = data.ids.hVessel;
					break;
				case TRANSMITTER_XPDR:
					hObj = data.xpdr.hVessel;
					break;
				case TRANSMITTER_VTOL:
					hObj = data.vtol.hBase;
					break;
				case TRANSMITTER_VOR: {
					hObj = data.vor.hPlanet;
					MATRIX3 Rp;
					oapiGetRotationMatrix (hObj, &Rp);
					oapiGetGlobalVel (hObj, &tvel);
					tvel += mul (Rp, _V(-sin(data.vor.lng),0,cos(data.vor.lng)) * PI2/oapiGetPlanetPeriod(hObj)*oapiGetSize(hObj)*cos(data.vor.lat));
					tgt_rvel = svel-tvel;
					sprintf (oapiDebugString(), "rvel: x=%f, y=%f, z=%f", tgt_rvel.x, tgt_rvel.y, tgt_rvel.z);
					} return; // done
			}
			if (hObj) {
				oapiGetGlobalVel (hObj, &tvel);
				tgt_rvel = svel-tvel;
			} else {
				// TODO
			}
		}
	}
}