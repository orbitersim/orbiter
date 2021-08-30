// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "Vessel.h"
#include "SuperVessel.h"
#include "Celbody.h"
#include "Psys.h"
#include "Camera.h"
#include "Vecmat.h"
#include "PlaybackEd.h"
#include "Log.h"
#include "Pane.h"
#include "State.h"
#include "MenuInfoBar.h"
#include <fstream>
#include <iomanip>
#include <io.h>
#include <direct.h>
#include <errno.h>

using namespace std;

extern Orbiter *g_pOrbiter;
extern Pane *g_pane;
extern TimeData td;
extern PlanetarySystem *g_psys;
extern Camera *g_camera;
extern char DBG_MSG[256];

const double max_step = 2.0;
const double att_step = 1.0;
const double eng_eps = 0.5;
const double EPS = 1e-8;

const int NTHGROUP = 15;
const char *THGROUPSTR[NTHGROUP] = {
	"MAIN","RETRO","HOVER",
	"RCS_PITCHUP","RCS_PITCHDOWN",
	"RCS_YAWLEFT","RCS_YAWRIGHT",
	"RCS_BANKLEFT","RCS_BANKRIGHT",
	"RCS_RIGHT","RCS_LEFT",
	"RCS_UP","RCS_DOWN",
	"RCS_FORWARD","RCS_BACK"	
};

static double Tofs = 0.0;
static double MJDofs = 0.0;
double RecordingSpeed = 1.0;
double WarpDelay = 0.0;
Vessel *vfocus = NULL;  // focus vessel as defined by playback stream

// ================================================================
// Local prototypes
// ================================================================

void Euler2Quaternion (double *a, Quaternion &q, int frm);


// ================================================================
// Flight recorder methods in class Vessel
// ================================================================

void Vessel::FRecorder_Reset ()
{
	frec_last.fstatus = FLIGHTSTATUS_UNDEFINED;
	frec_last.simt = frec_last_syst = -1e10;
	frec_last.frm = g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordPosFrame;
	//frec_last.frm = 0;  // ecliptic frame by default
	frec_last.crd = 0;  // cartesian coordinates by default
	frec_last.ref = 0;
	frec = 0;
	nfrec = 0;
	frec_att = 0;
	frec_att_last.simt = frec_att_last_syst = -1e10;
	frec_att_last.frm = g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordAttFrame;
	frec_att_last.ref = 0;
	nfrec_att = 0;
	nfrec_eng = 0;
	frec_eng_simt = -1e10;
	FRfname = 0;
	bFRplayback = bRequestPlayback = false;
	bFRrecord = false;
	RecordingSpeed = 1.0;
	WarpDelay = 0.0;
	vfocus = NULL;
	FRatc_stream = 0;
}

void Vessel::FRecorder_Activate (bool active, const char *fname, bool append)
{
	if (bFRrecord == active) return; // nothing to do
	if (active) {
		if (!append) FRecorder_Reset();
		bFRrecord = true;
		char cbuf[256];
		sprintf (cbuf, "Flights\\%s\\%s.pos", fname, name);
		if (FRfname) delete []FRfname;
		FRfname = new char[strlen(cbuf)+1]; TRACENEW
		strcpy (FRfname, cbuf);
		Tofs = td.SimT0;
		MJDofs = td.MJD0;
		//frec_last.frm = 1;  // for now, record in equatorial frame by default
		frec_last.crd = 1;  // for now, record in polar coordinates by default
	} else {
		bFRrecord = false;
		FRecorder_Save (true);
	}
}

void Vessel::FRecorder_Save (bool force)
{
	int i, iter = 0, niter = 1;
	DWORD j;
	double dt, alim;
	bool isfirst   = (frec_last.fstatus == FLIGHTSTATUS_UNDEFINED);
	bool newstatus = (frec_last.fstatus != fstatus);
	force = force || isfirst || newstatus;
	bool attforce  = force;

	const CelestialBody *ref = frec_last.ref;
	if (cbody != ref) force = true, niter++;
	if (frec_last.fstatus == FLIGHTSTATUS_LANDED && fstatus == FLIGHTSTATUS_FREEFLIGHT)
		FRecorder_SaveEvent ("TAKEOFF", cbody->Name());

	for (iter = 0; iter < niter; iter++) {
		if (ref) {
			Vector pos = s0->pos-ref->GPos();
			Vector vel = s0->vel-ref->GVel();
			if (frec_last.frm == 1) { // map to equatorial
				//vel = tmul (cbody->GRot(), vel);
				double lng, lat, rad, vref;
				ref->LocalToEquatorial (tmul (ref->GRot(), pos), lng, lat, rad);
				vref = Pi2/ref->RotT() * rad*cos(lat);
				vel = tmul (ref->GRot(), vel) - Vector(-vref*sin(lng),0,vref*cos(lng));
			}
			double cddir = dotp (vel.unit(), frec_last.rvel.unit());

			bool nextstep = (fstatus == FLIGHTSTATUS_FREEFLIGHT &&
				(g_pOrbiter->Cfg()->CfgRecPlayPrm.bSysInterval ? td.SysT1 - frec_last_syst : td.SimT1 - frec_last.simt) > max_step);

			if (nextstep || (cddir < 0.995) || force) {
				frec_last.simt    = td.SimT1;
				frec_last_syst    = td.SysT1;
				frec_last.fstatus = fstatus;
				//frec_last.ref     = cbody;
				//frec_last.rpos    = *gpos-cbody->GPos();
				switch (frec_last.frm) {
					case 0:  // ecliptic frame
						frec_last.rpos = s0->pos-ref->GPos();
						break;
					case 1:  // equatorial frame
						frec_last.rpos = ref->GlobalToLocal (s0->pos);
						break;
				}
				frec_last.rvel    = vel;

				ofstream ofs (FRfname, ios::app);
				ofs << setprecision(10)  << (frec_last.simt-Tofs) << ' ';
				if (frec_last.crd == 1) { // store in polar coords
					double r = frec_last.rpos.length();
					double phi = atan2 (frec_last.rpos.z, frec_last.rpos.x);
					double tht = asin (frec_last.rpos.y/r);
					ofs << setprecision(12) << r << ' ' << phi << ' ' << tht << ' ';
					double sphi = sin(phi), cphi = cos(phi), stht = sin(tht), ctht = cos(tht);
					double arg  = cphi*frec_last.rvel.x + sphi*frec_last.rvel.z;
					double vr   = stht*frec_last.rvel.y + ctht*arg;
					double vphi = (cphi*frec_last.rvel.z - sphi*frec_last.rvel.x) / (r*ctht);
					double vtht = (ctht*frec_last.rvel.y - stht*arg)/r;
					ofs << setprecision(10) << vr << ' ' << vphi << ' ' << vtht << endl;
				} else {
					ofs << setprecision(12) << frec_last.rpos.x << ' ' << frec_last.rpos.y << ' ' << frec_last.rpos.z << ' ';
					ofs << setprecision(10) << frec_last.rvel.x << ' ' << frec_last.rvel.y << ' ' << frec_last.rvel.z << endl;
				}
			}
		}
		if (cbody != ref) {
			ofstream ofs(FRfname, isfirst ? ios::trunc : ios::app);
			ofs << "STARTMJD " << setprecision(12) << MJDofs << endl;
			ofs << "REF " << cbody->Name() << endl;
			ofs << "FRM " << (frec_last.frm == 0 ? "ECLIPTIC" : "EQUATORIAL") << endl;
			ofs << "CRD " << (frec_last.crd == 0 ? "CARTESIAN" : "POLAR") << endl;
			frec_last.ref = ref = cbody;
		}
	} 

	// attitude data
	ref = frec_att_last.ref;
	niter = 1;
	if (ref != sp.ref) attforce = true, niter++;
	for (iter = 0; iter < niter; iter++) {
		if (ref) {
			double a[3];
			Quaternion q;
			switch (frec_att_last.frm) {
			case 0:
				a[0] = atan2 (s0->R.m23, s0->R.m33);
				a[1] = -asin (s0->R.m13);
				a[2] = atan2 (s0->R.m12, s0->R.m11);
				q.Set (s0->Q);
				break;
			case 1: {
				double lng, lat, rad, slng, slat, clng, clat;
				ref->GlobalToEquatorial (s0->pos, lng, lat, rad);
				slng = sin(lng), clng = cos(lng), slat = sin(lat), clat = cos(lat);
				Matrix rot (-slng, clat*clng, -slat*clng, // horizon->local
					         0   , slat,       clat,
							 clng, clat*slng, -slat*slng);
				//Matrix rot (sp.Hor2Local());
				rot.premul (ref->GRot());
				rot.tpremul (s0->R);

				if (fabs(rot.m32) > 1.0-EPS) { // apply tiny tilt when pitch=+/-90 to avoid instability
					static const double sineps=sin(1e-6), coseps=cos(1e-6);
					rot.premul (Matrix (1,0,0, 0,coseps,sineps, 0,-sineps,coseps));
				}
				a[0] = atan2 (rot.m12, rot.m22);     // bank
				a[1] = asin  (rot.m32);              // pitch
				a[2] = atan2 (rot.m31, rot.m33);     // yaw
				q.Set (rot);
				} break;
			}
			if (fstatus == FLIGHTSTATUS_FREEFLIGHT) {
				bool finestep = ((g_pOrbiter->Cfg()->CfgRecPlayPrm.bSysInterval ? td.SysT1 - frec_att_last_syst : td.SimT1 - frec_att_last.simt) > att_step);
				alim = (finestep ? 1e-3 : 1e-2);
				// the difference between two quaternions may have to be
				// defined a bit more cleverly ...
				double dvx = q.qvx - frec_att_last.q.qvx;
				double dvy = q.qvy - frec_att_last.q.qvy;
				double dvz = q.qvz - frec_att_last.q.qvz;
				double ds  = q.qs  - frec_att_last.q.qs;
				double diff = sqrt (dvx*dvx + dvy*dvy + dvz*dvz + ds*ds);
				if (diff > alim) attforce = true;
			}
			if (attforce) {
				char cbuf[256];
				strcpy (cbuf, FRfname); strcpy (cbuf+strlen(cbuf)-3, "att");
				ofstream ofs (cbuf, ios::app);
				ofs << setprecision(10) << (td.SimT1-Tofs) << setprecision(6);
				for (i = 0; i < 3; i++)
					ofs << ' ' << (/*frec_att_last.att[i] =*/ a[i]);
				ofs << endl;
				frec_att_last.q.Set (q);
				frec_att_last_syst = td.SysT1;
				frec_att_last.simt = td.SimT1;
			}
		
		}
		if (ref != sp.ref) {
			char cbuf[256];
			strcpy (cbuf, FRfname); strcpy (cbuf+strlen(cbuf)-3, "att");
			ofstream ofs (cbuf, isfirst ? ios::trunc : ios::app);
			if (isfirst)
				ofs << "STARTMJD " << setprecision(12) << MJDofs << endl;
			switch (frec_att_last.frm) {
			case 0:
				ofs << "FRM ECLIPTIC" << endl;
				break;
			case 1:
				ofs << "REF " << sp.ref->Name() << endl;
				ofs << "FRM HORIZON" << endl;
				break;
			}
			frec_att_last.ref = ref = sp.ref;
		}
	}

	// engine attributes
	if (nfrec_eng != nthruster) {
		if (nfrec_eng) delete []frec_eng;
		if (nthruster) {
			frec_eng = new double[nfrec_eng = nthruster]; TRACENEW
			for (j = 0; j < nthruster; j++) frec_eng[j] = -1;
			frec_eng_simt = -1e10; // force output
		}
		else frec_eng = 0;
	}
	bool bfopen = false;
	dt = td.SimT1-frec_eng_simt;
	alim = min (0.2, 0.1/dt);
	ofstream ofs;
	for (j = 0; j < nthruster; j++) {
		if (fabs(frec_eng[j]-thruster[j]->level) > alim || force) {
			if (!bfopen) {
				frec_eng_simt = td.SimT1;
				char cbuf[256];
				strcpy (cbuf, FRfname); strcpy (cbuf+strlen(cbuf)-3, "atc");
				ofs.open (cbuf, isfirst ? ios::trunc : ios::app);
				ofs << setprecision(10) << (frec_eng_simt-Tofs) << " ENG";
				bfopen = true;
			}
			ofs << ' ' << j << ':' << setprecision(2) << (frec_eng[j] = thruster[j]->level);
		}
	}
	if (bfopen) {
		ofs << endl;
		ofs.close();
	}
}

// Save a vessel-specific event
void Vessel::FRecorder_SaveEvent (const char *event_type, const char *event)
{
	if (!bFRrecord) return;
	char cbuf[256];
	strcpy (cbuf, FRfname); strcpy (cbuf+strlen(cbuf)-3, "atc");
	ofstream ofs(cbuf, ios::app);
	ofs << setprecision(10) << (td.SimT1-Tofs) << ' ' << event_type << ' ' << event << endl;
}

void Vessel::FRecorder_SaveEventInt (const char *event_type, int event)
{
	static char cbuf[24];
	FRecorder_SaveEvent (event_type, _itoa (event, cbuf, 10));
}

void Vessel::FRecorder_SaveEventFloat (const char *event_type, double event)
{
	static char cbuf[128];
	sprintf (cbuf, "%f", event);
	FRecorder_SaveEvent (event_type, cbuf);
}

void Vessel::FRecorder_Clear ()
{
	if (nfrec) {
		delete []frec;
		nfrec = 0;
	}
	if (nfrec_att) {
		delete []frec_att;
		nfrec_att = 0;
	}
	if (nfrec_eng) {
		delete []frec_eng;
		nfrec_eng = 0;
	}
	if (FRfname) delete []FRfname;
	if (FRatc_stream) {
		delete FRatc_stream;
		FRatc_stream = 0;
	}
	bFRplayback = false;
	bFRrecord = false;
}

bool Vessel::FRecorder_Read (const char *scname)
{
	int i;
	char fname[256], cbuf[256];

	for (i = strlen(scname)-1; i > 0; i--)
		if (scname[i-1] == '\\') break;
	sprintf (fname, "Flights\\%s\\%s.pos", scname+i, name);

	ifstream ifs (fname);
	if (!ifs) {
		bFRplayback = false;
		return false;
	}

	FRecorder_Clear();
	
	int nbuf = 0, nbuf_att = 0, frm = 0, crd = 0, attfrm = 0;
	double simt, x, y, z, vx, vy, vz;
	const CelestialBody *ref = g_psys->GetGravObj(0);

	// open position/velocity stream
	while (ifs.getline (cbuf, 256)) {
		if (!_strnicmp (cbuf, "REF", 3)) {
			ref = g_psys->GetGravObj (trim_string (cbuf+4), true);
			if (!ref) ref = g_psys->GetGravObj (0);
		} else if (!_strnicmp (cbuf, "FRM", 3)) {
			if (!_stricmp (trim_string (cbuf+4), "EQUATORIAL")) frm = 1;
			else frm = 0;
		} else if (!_strnicmp (cbuf, "CRD", 3)) {
			if (!_stricmp (trim_string (cbuf+4), "POLAR")) crd = 1;
			else crd = 0;
		} else if (!_strnicmp (cbuf, "STARTMJD", 8)) {
			sscanf (cbuf+9, "%lf", &MJDofs);
		} else {
			if (sscanf (cbuf, "%lf%lf%lf%lf%lf%lf%lf", &simt, &x, &y, &z, &vx, &vy, &vz) != 7)
				continue;
			if (crd == 1) { // map from polar coords
				double xz, r = x, phi = y, tht = z;
				double vr = vx, vphi = vy, vtht = vz;
				double sphi = sin(phi), cphi = cos(phi), stht = sin(tht), ctht = cos(tht);
				y = r*sin(tht); xz = r*cos(tht);
				x = xz*cos(phi); z = xz*sin(phi);
				vx = vr*cphi*ctht - r*vphi*sphi*ctht - r*vtht*cphi*stht;
				vy = vr*stht + r*vtht*ctht;
				vz = vr*sphi*ctht + r*vphi*cphi*ctht - r*vtht*sphi*stht;
				//vy = vr*sin(vtht); xz = vr*cos(vtht);
				//vx = xz*cos(vphi); vz = xz*sin(vphi);
			}
			if (nfrec == nbuf) { // re-allocate
				FRecord *tmp = new FRecord[nbuf += 1024]; TRACENEW
				if (nfrec) {
					memcpy (tmp, frec, nfrec*sizeof(FRecord));
					delete []frec;
				}
				frec = tmp;
			}
			frec[nfrec].simt = simt;
			frec[nfrec].frm  = frm;
			frec[nfrec].ref  = ref;
			frec[nfrec].rpos.Set (x, y, z);
			frec[nfrec].rvel.Set (vx, vy, vz);
			nfrec++;
		}
	}
	ifs.close();
	ifs.clear();
	cfrec = 0;
	cfrec_att = 0;

	// open attitude stream
	ref = g_psys->GetGravObj(0);
	strcpy (fname+strlen(fname)-3, "att");
	ifs.open (fname);
	while (ifs.getline (cbuf, 256)) {
		if (!_strnicmp (cbuf, "REF", 3)) {
			ref = g_psys->GetGravObj (trim_string (cbuf+4), true);
			if (!ref) ref = g_psys->GetGravObj (0);
		} else if (!_strnicmp (cbuf, "FRM", 3)) {
			if (!_stricmp (trim_string (cbuf+4), "HORIZON")) attfrm = 1;
			else attfrm = 0;
		} else if (!_strnicmp (cbuf, "STARTMJD", 8)) {
			sscanf (cbuf+9, "%lf", &MJDofs);
			// assumes that MJDofs from all streams are the same!
		} else {
			double a[3];
			sscanf (cbuf, "%lf%lf%lf%lf", &simt, a+0, a+1, a+2);
			if (nfrec_att == nbuf_att) { // re-allocate
				FRecord_att *tmp = new FRecord_att[nbuf_att += 1024]; TRACENEW
				if (nfrec_att) {
					memcpy (tmp, frec_att, nfrec_att*sizeof(FRecord_att));
					delete []frec_att;
				}
				frec_att = tmp;
			}
			frec_att[nfrec_att].simt = simt;
			frec_att[nfrec_att].frm = attfrm;
			frec_att[nfrec_att].ref = ref;

			// convert Euler angles to quaternions
			Euler2Quaternion (a, frec_att[nfrec_att].q, frec_att[nfrec_att].frm);
			//for (int i = 0; i < 3; i++)
			//	frec_att[nfrec_att].att[i] = a[i];

			nfrec_att++;
		}
	}

	// open articulation event stream
	if (FRatc_stream) delete FRatc_stream;
	strcpy (cbuf, fname); strcpy (cbuf+strlen(cbuf)-3, "atc");
	FRatc_stream = new ifstream (cbuf); TRACENEW
	*FRatc_stream >> frec_eng_simt;
	if (!FRatc_stream->good()) {
		delete FRatc_stream;
		FRatc_stream = 0;
	}

	bFRplayback = true;
	return true;
}

void Vessel::FRecorder_Play ()
{
	dASSERT (s1, "State vector not available");
	StateVectors *sv = s1;

	if (fstatus == FLIGHTSTATUS_FREEFLIGHT) {

		double dT, dt, w0, w1;
		double r0, r1, v0, v1, a0, b, lng, lat, rad, vref;
		int i;
		static Vector s;

		while (cfrec+2 < nfrec && frec[cfrec+1].simt < td.SimT1) cfrec++;
		dT = frec[cfrec+1].simt - frec[cfrec].simt;
		dt = td.SimT1 - frec[cfrec].simt;

		Vector P0 = frec[cfrec].rpos, P1 = frec[cfrec+1].rpos;
		Vector V0 = frec[cfrec].rvel, V1 = frec[cfrec+1].rvel;
		if (frec[cfrec].frm == 1) { // map from equatorial frame
			// propagate from current rotation state to rotation state at last sample
			double dlng = Pi2*dt/frec[cfrec].ref->RotT(), sind = sin(dlng), cosd = cos(dlng);
			s.x =  P0.x*cosd + P0.z*sind;
			s.z = -P0.x*sind + P0.z*cosd;
			s.y =  P0.y;
			P0.Set (mul (frec[cfrec].ref->s1->R, s));

			// Needs to be fixed!
			frec[cfrec].ref->LocalToEquatorial (s, lng, lat, rad);
			vref = Pi2/frec[cfrec].ref->RotT() * rad * cos(lat);
			s.x =  V0.x*cosd + V0.z*sind;
			s.z = -V0.x*sind + V0.z*cosd;
			s.y =  V0.y;
			V0.Set (mul (frec[cfrec].ref->s1->R, s + Vector (-vref*sin(lng),0,vref*cos(lng))));
		}
		if (frec[cfrec+1].frm == 1) { // map from equatorial frame
			double dlng = Pi2*(dt-dT)/frec[cfrec+1].ref->RotT(), sind = sin(dlng), cosd = cos(dlng);
			s.x =  P1.x*cosd + P1.z*sind;
			s.z = -P1.x*sind + P1.z*cosd;
			s.y =  P1.y;
			P1.Set (mul (frec[cfrec+1].ref->s1->R, s));
			frec[cfrec+1].ref->LocalToEquatorial (s, lng, lat, rad);
			vref = Pi2/frec[cfrec+1].ref->RotT() * rad * cos(lat);
			s.x =  V1.x*cosd + V1.z*sind;
			s.z = -V1.x*sind + V1.z*cosd;
			s.y =  V1.y;
			V1.Set (mul (frec[cfrec].ref->s1->R, s + Vector (-vref*sin(lng),0,vref*cos(lng))));
		}

		for (i = 0; i < 3; i++) {
			r0 = P0.data[i]; r1 = P1.data[i];
			v0 = V0.data[i]; v1 = V1.data[i];
			a0 = 2.0*(3.0*(r1-r0) - dT*(2.0*v0+v1)) / (dT*dT);
			b  = 6.0*(2.0*(r0-r1) + dT*(v0+v1)) / (dT*dT*dT);
			sv->vel.data[i] = v0 + a0*dt + 0.5*b*dt*dt;
			sv->pos.data[i] = r0 + v0*dt + 0.5*a0*dt*dt + b*dt*dt*dt/6.0;
		}

		sv->pos += frec[cfrec].ref->s1->pos;
		sv->vel += frec[cfrec].ref->s1->vel;
	
		// attitude
		if (td.SimT1 < frec_att[nfrec_att-1].simt) {

			// store old orientation for calculating angular velocities
			Vector r1 (sv->R.m11, sv->R.m21, sv->R.m31);
			Vector r2 (sv->R.m12, sv->R.m22, sv->R.m32);
			Vector r3 (sv->R.m13, sv->R.m23, sv->R.m33);

			while (cfrec_att+2 < nfrec_att && frec_att[cfrec_att+1].simt < td.SimT1) cfrec_att++;
			dt = frec_att[cfrec_att+1].simt - frec_att[cfrec_att].simt;
			w1 = (td.SimT1-frec_att[cfrec_att].simt)/dt;
			w0 = 1.0-w1;

			// Orientation at intermediate time point by interpolating endpoint quaternions
			if (frec_att[cfrec_att].frm == 0) {
				Quaternion Q;
				Q.interp (frec_att[cfrec_att].q, frec_att[cfrec_att+1].q, w1);
				sv->SetRot (Q);
			} else {
				Quaternion Q;
				Q.interp (frec_att[cfrec_att].q, frec_att[cfrec_att+1].q, w1);
				sv->R.Set (Q);
				double lng, lat, rad, slng, clng, slat, clat;
				Vector loc = tmul (frec_att[cfrec_att].ref->s1->R, sv->pos - frec_att[cfrec_att].ref->s1->pos);
				frec_att[cfrec_att].ref->LocalToEquatorial (loc, lng, lat, rad);
				slng = sin(lng), clng = cos(lng), slat = sin(lat), clat = cos(lat);
				sv->R.postmul (Matrix (-slng,      0,     clng,
					                    clat*clng, slat,  clat*slng,
									   -slat*clng, clat, -slat*slng));
				sv->R.tpostmul (frec_att[cfrec_att].ref->s1->R);
				//rrot.postmul (sp.Local2Hor());
				//rrot.tpostmul (cbody->GRot());
				sv->SetRot (transp (sv->R));
			}

			// recover angular velocities from change in rotation matrix
			// this may need more thought. The current algorithm may work
			// in a differential sense, but there should be something more
			// intelligent in the case of large changes in orientation
			Vector dx = tmul (sv->R,r1);
			Vector dy = tmul (sv->R,r2);
			Vector dz = tmul (sv->R,r3);
			sv->omega.x =  atan2 (dy.z, dz.z) * td.iSimDT;
			sv->omega.y = -atan2 (dx.z, dx.x) * td.iSimDT;
			sv->omega.z =  atan2 (dx.y, dx.x) * td.iSimDT;
		}
		el_valid = false;

		if (supervessel && supervessel->GetVessel(0) == this)
			supervessel->SetStateFromComponent (sv, 0);

	} // end freeflight
}

void Vessel::FRecorder_PlayEvent ()
{
	// articulation (also scanned when landed)
	while (FRatc_stream && td.SimT1 > frec_eng_simt) {
		char cbuf[1024], *s, *e, c;
		double lvl;
		int i;
		DWORD id;
		FRatc_stream->getline (cbuf, 1024);
		s = strtok (cbuf, " \t");
		if (s) {
			if (!_stricmp (s, "ENG")) {
				while (s = strtok (NULL, " \t\n")) {
					if (sscanf (s, "%d%c%lf", &id, &c, &lvl) == 3 && c == ':') {
						if (id < nthruster) SetThrusterLevel_playback (thruster[id], lvl);
					} else {
						for (i = 0; i < NTHGROUP; i++)
							if (!_strnicmp (s, THGROUPSTR[i], strlen (THGROUPSTR[i]))) break;
						if (i < NTHGROUP && sscanf (s+strlen(THGROUPSTR[i])+1, "%lf", &lvl)) {
							for (DWORD j = 0; j < thruster_grp_default[i].nts; j++)
								SetThrusterLevel_playback (thruster_grp_default[i].ts[j], lvl);
						}
					}
				}
			} else if (!_strnicmp (s, "LANDED", 6)) {
#ifdef UNDEF
				if (fstatus != FLIGHTSTATUS_LANDED) {
					Planet *p = g_psys->GetPlanet (s+7, true);
					if (supervessel) {
						double alt = supervessel->Altitude(); //rad - proxybody->Size();
						Matrix lrot (supervessel->s0->R);
						lrot.tpremul (p->s0->R);
						supervessel->InitLanded (p, supervessel->sp.lng, supervessel->sp.lat, supervessel->sp.dir, &lrot, alt);
					} else
						InitLanded (g_psys->GetPlanet (s+7, true), sp.lng, sp.lat, sp.dir);
				}
#endif
			} else if (!_strnicmp (s, "TAKEOFF", 7)) {
				if (fstatus == FLIGHTSTATUS_LANDED)
					bForceActive = true;
			} else if (!_strnicmp (s, "NAVMODE", 7)) {
				if (!strcmp (s+7, "CLR")) {
					sscanf (s+11, "%d", &i);
					ClrNavMode (i, false, true);
				} else {
					sscanf (s+8, "%d", &i);
					SetNavMode (i, true);
				}
			} else if (!_stricmp (s, "RCSMODE")) {
				sscanf (s+8, "%d", &i);
				SetAttMode (i, true);
			} else if (!_stricmp (s, "ADCMODE")) {
				sscanf (s+8, "%d", &i);
				SetADCtrlMode (i, true);
			} else if (!_stricmp (s, "UNDOCK")) {
				while (s = strtok (NULL, " \t\n")) {
					int dock;
					sscanf (s, "%d", &dock);
					Undock (dock);
				}
			} else if (!_stricmp (s, "DETACH")) {
				double v;
				int res = sscanf (s+7, "%d%lf", &id, &v);
				if (res < 2) v = 0.0;
				AttachmentSpec *as = GetAttachmentFromIndex (false, id);
				if (as) DetachChild (as, v);
			} else if (!_stricmp (s, "ATTACH")) {
				DWORD pidx, cidx;
				char cname[128], modestr[32];
				int res = sscanf (s+7, "%s%d%d%s", cname, &pidx, &cidx, modestr);
				Vessel *child = g_psys->GetVessel (cname, true);
				bool loose = (res > 3 && !_stricmp (modestr,"LOOSE") ? true : false);
				if (child) {
					AttachmentSpec *asp = GetAttachmentFromIndex (false, pidx);
					AttachmentSpec *asc = child->GetAttachmentFromIndex (true, cidx);
					if (asp && asc)
						AttachChild (child, asp, asc, loose);
				}
			} else if (!_strnicmp (s, "LIGHTSOURCE", 11)) { // light emitter event
				s = strtok (NULL, " \t\n");
				DWORD idx;
				if (sscanf (s, "%d", &idx) == 1 && idx < nemitter) {
					s = strtok (NULL, " \t\n");
					if (!_stricmp (s, "ACTIVATE")) {
						DWORD flag;
						if (sscanf (s+9, "%d", &flag) == 1)
							emitter[idx]->Activate (flag != 0);
					}
				}
			} else if (!_strnicmp (s, "TACC", 4)) { // DEPRECATED - now stored in system stream
				if (sscanf (s+5, "%lf%lf", &RecordingSpeed, &WarpDelay) < 2)
					WarpDelay = 0.0;
				if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayWarp)
						g_pOrbiter->SetWarpFactor (RecordingSpeed, true, WarpDelay);
			} else if (!_strnicmp (s, "CAMERA", 6)) { // DEPRECATED - now stored in system stream
				s = strtok (NULL, " \t\n");
				if (!_strnicmp (s, "PRESET", 6)) {
					sscanf (s+7, "%d", &i);
					g_camera->RecallPreset (i);
				}
			} else if (!_strnicmp (s, "NOTE", 4)) { // DEPRECATED - now stored in system stream
				oapi::ScreenAnnotation *sa = g_pOrbiter->SNotePB();
				if (sa) {
					if (!strcmp (s+4, "COL")) {
						double r, g, b;
						sscanf (s+8, "%lf%lf%lf", &r, &g, &b);
						VECTOR3 col = {r,g,b};
						sa->SetColour (col);
					} else if (!strcmp (s+4, "SIZE")) {
						double scale;
						sscanf (s+9, "%lf", &scale);
						sa->SetSize (scale);
					} else if (!strcmp (s+4, "POS")) {
						double x1, y1, x2, y2;
						sscanf (s+8, "%lf%lf%lf%lf", &x1, &y1, &x2, &y2);
						sa->SetPosition (x1, y1, x2, y2);
					} else if (!strcmp (s+4, "OFF")) {
						sa->ClearText();
					} else {
						sa->SetText (s+5);
					}
				}
			} else if (modIntf.v->Version() >= 1) { // pass event to vessel
				e = s+(strlen(s)+1);
				//e = strtok (NULL, " \t");
				((VESSEL2*)modIntf.v)->clbkPlaybackEvent (td.SimT1, frec_eng_simt, s, e);
			}
		}
		*FRatc_stream >> frec_eng_simt;
		if (!FRatc_stream->good()) {
			delete FRatc_stream;
			FRatc_stream = 0;
		}
	}

	FRecorder_CheckEnd ();
}


void Vessel::FRecorder_CheckEnd ()
{
	if (td.SimT1 > frec[nfrec-1].simt) { // reached end of playback list
		g_pOrbiter->EndPlayback();
		//FRecorder_EndPlayback();
		//g_pOrbiter->SNote()->ClearNote();

		// TEMPORARY
		//g_pOrbiter->ToggleRecorder (false, true);
	}
}

void Vessel::FRecorder_EndPlayback ()
{
	if (bFRplayback) {
		bFRplayback = false;
		Amom_add.Set(0,0,0);
		rvel_base.Set (s0->vel);
		rvel_add.Set(0,0,0);
		rpos_base.Set (s0->pos);
		rpos_add.Set(0,0,0);
		s0->Q.Set (s0->R);
		if (supervessel && supervessel->GetVessel(0) == this)
			supervessel->FRecorder_EndPlayback();
	}
}

// ================================================================
// System event recording/playback
// (implemented in class Orbiter)
// ================================================================

void Orbiter::FRecorder_Reset ()
{
	FRsysname = 0;
	FRsys_stream = 0;
	FReditor = 0;
	frec_sys_simt = -1e10;
	bRecord = bPlayback = false;
}

bool Orbiter::FRecorder_PrepareDir (const char *fname, bool force)
{
	char cbuf[256];
	strcpy (cbuf, "Flights\\"); strcat (cbuf, fname);
	if (_mkdir (cbuf) == -1) {
		if (errno == EEXIST && !force) return false;
		// don't overwrite existing recording
		struct _finddata_t fd;
		char cb2[256], cb3[256];
		strcpy (cb2, cbuf); strcat (cb2, "\\*");
		intptr_t handle = _findfirst (cb2, &fd), res = handle;
		while (res != -1) {
			if (!(fd.attrib & _A_SUBDIR)) {
				sprintf (cb3, "%s\\%s", cbuf, fd.name);
				_unlink (cb3);
			}
			res = _findnext (handle, &fd);
		}
		_findclose (handle);
	}
	return true;
}

void Orbiter::FRecorder_Activate (bool active, const char *fname, bool append)
{
	if (bRecord == active) return; // nothing to do
	if (active) {
		if (!append) FRecorder_Reset();
		bRecord = true;
		char cbuf[256];
		sprintf (cbuf, "Flights\\%s\\system.dat", fname);
		if (FRsysname) delete []FRsysname;
		FRsysname = new char[strlen(cbuf)+1]; TRACENEW
		strcpy (FRsysname, cbuf);
	} else {
		bRecord = false;
	}
	if (g_pane && g_pane->MIBar()) g_pane->MIBar()->SetRecording(bRecord);
}

// Save a system event
void Orbiter::FRecorder_SaveEvent (const char *event_type, const char *event)
{
	if (!bRecord) return;
	ofstream ofs(FRsysname, ios::app);
	ofs << setprecision(10) << (td.SimT1-Tofs) << ' ' << event_type << ' ' << event << endl;
}

void Orbiter::FRecorder_OpenPlayback (const char *scname)
{
	int i;
	char cbuf[256];

	if (FRsys_stream) delete FRsys_stream;

	for (i = strlen(scname)-1; i > 0; i--)
		if (scname[i-1] == '\\') break;
	sprintf (cbuf, "Flights\\%s\\system.dat", scname+i);
	if (FRsysname) delete []FRsysname;
	FRsysname = new char[strlen(cbuf)+1]; TRACENEW
	strcpy (FRsysname, cbuf);

	FRsys_stream = new ifstream (cbuf); TRACENEW
	*FRsys_stream >> frec_sys_simt;
	if (!FRsys_stream->good()) {
		delete FRsys_stream;
		FRsys_stream = 0;
	}
}

void Orbiter::FRecorder_SuspendPlayback ()
{
	if (FRsys_stream) {
		delete FRsys_stream;
		FRsys_stream = 0;
	}
}

void Orbiter::FRecorder_RescanPlayback ()
{
	oapi::ScreenAnnotation *sa = SNotePB();
	if (sa) sa->Reset();
	FRsys_stream = new ifstream (FRsysname); TRACENEW
	*FRsys_stream >> frec_sys_simt;
	FRecorder_Play(); // read up to current playback time
}

void Orbiter::FRecorder_ClosePlayback ()
{
	if (FRsys_stream) {
		delete FRsys_stream;
		FRsys_stream = 0;
	}
	if (FReditor) {
		delete FReditor;
		FReditor = 0;
	}
}

void Orbiter::FRecorder_Play ()
{
	// scan system event stream
	while (FRsys_stream && td.SimT1 > frec_sys_simt) {
		char cbuf[1024], *s;
		int i;
		FRsys_stream->getline (cbuf, 1024);
		s = strtok (cbuf, " \t");
		if (s) {
			if (!_strnicmp (s, "TACC", 4)) {
				if (sscanf (s+5, "%lf%lf", &RecordingSpeed, &WarpDelay) < 2)
					WarpDelay = 0.0;
				if (Cfg()->CfgRecPlayPrm.bReplayWarp)
						SetWarpFactor (RecordingSpeed, true, WarpDelay);
			} else if (!_strnicmp (s, "CAMERA", 6)) {
				s = strtok (NULL, " \t\n");
				if (!_strnicmp (s, "PRESET", 6)) {
					sscanf (s+7, "%d", &i);
					g_camera->RecallPreset (i);
				} else if (!_strnicmp (s, "SET", 3)) {
					CameraMode *cm = CameraMode::Create (s+4);
					if (cm) g_camera->SetCMode (cm);
					delete cm;
				}
			} else if (!_strnicmp (s, "FOCUS", 5)) {
				s = strtok (NULL, " \t\n");
				vfocus = g_psys->GetVessel (s, true);
				if (vfocus && Cfg()->CfgRecPlayPrm.bReplayFocus)
					g_pOrbiter->SetFocusObject (vfocus);
			} else if (!_strnicmp (s, "NOTE", 4)) {
				oapi::ScreenAnnotation *sa = SNotePB();
				if (sa) {
					if (!strcmp (s+4, "COL")) {
						double r, g, b;
						sscanf (s+8, "%lf%lf%lf", &r, &g, &b);
						VECTOR3 col = {r,g,b};
						sa->SetColour (col);
					} else if (!strcmp (s+4, "SIZE")) {
						double scale;
						sscanf (s+9, "%lf", &scale);
						sa->SetSize (scale);
					} else if (!strcmp (s+4, "POS")) {
						double x1, y1, x2, y2;
						sscanf (s+8, "%lf%lf%lf%lf", &x1, &y1, &x2, &y2);
						sa->SetPosition (x1, y1, x2, y2);
					} else if (!strcmp (s+4, "OFF")) {
						sa->ClearText();
					} else {
						sa->SetText (s+5);
					}
				}
			} else if (!_strnicmp (s, "JUMPTOTIME", 10)) {
				double jumptime;
				if (sscanf (s+11, "%lf", &jumptime) && jumptime > td.SimT0) {
					double tgtmjd = td.MJD0 + (jumptime-td.SimT0)/86400.0;
					g_pOrbiter->Timejump(tgtmjd, PROP_ORBITAL_FIXEDSURF);
				}
			} else if (!_strnicmp (s, "ENDSESSION", 10)) {
				if (hRenderWnd) PostMessage (hRenderWnd, WM_CLOSE, 0, 0);
			}
		}
		*FRsys_stream >> frec_sys_simt; // read time for next event
		if (!FRsys_stream->good()) {    // end of stream
			delete FRsys_stream;
			FRsys_stream = 0;
		}
	}
}

void Orbiter::FRecorder_ToggleEditor ()
{
	if (FReditor) {
		delete FReditor;
		FReditor = 0;
	} else {
		const char *playbackdir = pState->PlaybackDir();
		FReditor = new PlaybackEditor (this, playbackdir); TRACENEW
	}
}

// ================================================================
// helper functions

// convert Euler angles from given reference frame to quaternion
void Euler2Quaternion (double *a, Quaternion &q, int frm)
{
	double sinx = sin(a[0]), cosx = cos(a[0]);
	double siny = sin(a[1]), cosy = cos(a[1]);
	double sinz = sin(a[2]), cosz = cos(a[2]);
	if (frm == 0) { // global frame
		Matrix R (1,0,0,  0,cosx,sinx,  0,-sinx,cosx);
		R.postmul (Matrix (cosy,0,-siny,  0,1,0,  siny,0,cosy));
		R.postmul (Matrix (cosz,sinz,0,  -sinz,cosz,0,  0,0,1));
		q.Set (R);
	} else {        // local horizon frame
		Matrix R (cosx,sinx,0,  -sinx,cosx,0,  0,0,1);
		R.postmul (Matrix (1,0,0,  0,cosy,-siny,  0,siny,cosy));
		R.postmul (Matrix (cosz,0,-sinz,  0,1,0,  sinz,0,cosz));
		q.Set (R);
	}
}

