// ==============================================================
//                 ORBITER MODULE: SolarSail
//                  Part of the ORBITER SDK
//             Copyright (C) 2007 Martin Schweiger
//                   All rights reserved
//
// SolarSail.cpp
// Control module for SolarSail vessel class
// ==============================================================

#define ORBITER_MODULE

#include "SolarSail.h"
#include "meshres.h"

// ==============================================================
// Some vessel parameters
// ==============================================================
const double SAIL_RADIUS = 500.0;

// Calculate lift coefficient [Cl] as a function of aoa (angle of attack) over -Pi ... Pi
// Implemented here as a piecewise linear function
double LiftCoeff (double aoa)
{
	int i;
	const int nlift = 9;
	static const double AOA[nlift] = {-180*RAD,-60*RAD,-30*RAD,-1*RAD,15*RAD,20*RAD,25*RAD,60*RAD,180*RAD};
	static const double CL[nlift]  = {       0,      0,   -0.1,     0,   0.2,  0.25,   0.2,     0,      0};
	static const double SCL[nlift] = {(CL[1]-CL[0])/(AOA[1]-AOA[0]), (CL[2]-CL[1])/(AOA[2]-AOA[1]),
		                              (CL[3]-CL[2])/(AOA[3]-AOA[2]), (CL[4]-CL[3])/(AOA[4]-AOA[3]),
									  (CL[5]-CL[4])/(AOA[5]-AOA[4]), (CL[6]-CL[5])/(AOA[6]-AOA[5]),
									  (CL[7]-CL[6])/(AOA[7]-AOA[6]), (CL[8]-CL[7])/(AOA[8]-AOA[7])};
	for (i = 0; i < nlift-1 && AOA[i+1] < aoa; i++);
	return CL[i] + (aoa-AOA[i])*SCL[i];
}

// distance between two vertices
inline double Dst (const NTVERTEX *v1, const NTVERTEX *v2)
{
	double dx = v1->x - v2->x;
	double dy = v1->y - v2->y;
	double dz = v1->z - v2->z;
	return sqrt (dx*dx + dy*dy + dz*dz);
}

inline VECTOR3 Nml (const NTVERTEX *v1, const NTVERTEX *v2, const NTVERTEX *v3)
{
	float dx1 = v2->x - v1->x,   dx2 = v3->x - v1->x;
	float dy1 = v2->y - v1->y,   dy2 = v3->y - v1->y;
	float dz1 = v2->z - v1->z,   dz2 = v3->z - v1->z;

	return _V(dy1*dz2 - dy2*dz1, dz1*dx2 - dz2*dx1, dx1*dy2 - dx2*dy1);
}

inline VECTOR3 crossp (const NTVERTEX *v1, const NTVERTEX *v2)
{
	return _V(v1->y*v2->z - v2->y*v1->z, v1->z*v2->x - v2->z*v1->x, v1->x*v2->y - v2->x*v1->y);
}

// --------------------------------------------------------------
// One-time global setup across all instances
// --------------------------------------------------------------
void SolarSail::GlobalSetup()
{
	SolarSail::hMeshTpl = oapiLoadMeshGlobal ("SolarSail");
	oapiSetMeshProperty (SolarSail::hMeshTpl, MESHPROPERTY_MODULATEMATALPHA, 1);
	SetupElasticity (hMeshTpl);
}

// --------------------------------------------------------------
// Set up the dynamic elastic sail deformation code
// --------------------------------------------------------------
void SolarSail::SetupElasticity (MESHHANDLE hMesh)
{
	MESHGROUP *sail = oapiMeshGroup (hMesh, GRP_sail1);
	// all sail segments have the same mesh structure, so segment 1 represents all 4
	DWORD nvtx = sail->nVtx/2; // scan front side only
	DWORD nidx = sail->nIdx/2; // scan front side only
	DWORD ntri = nidx/3;
	WORD *idx = sail->Idx;
	NTVERTEX *vtx = sail->Vtx;
	DWORD i, j, k, m, nj, nk;

	// generate node neighbour graph
	sail_vbuf = new VECTOR3[nvtx];
	nbhr = new NBHR[nvtx];
	for (i = 0; i < nvtx; i++) {
		nbhr[i].nnd = 0;
		nbhr[i].fix = (vtx[i].x == 0 || vtx[i].y == 0);
	}

	for (i = 0; i < ntri; i++) {
		WORD *tri = idx+(i*3);
		for (j = 0; j < 3; j++) {
			nj = tri[j];
			for (k = 0; k < 3; k++) {
				if (j == k) continue;
				nk = tri[k];
				for (m = 0; m < nbhr[nj].nnd; m++)
					if (nbhr[nj].nd[m] == nk) break; // already in neighbour list
				if (m == nbhr[nj].nnd) {
					if (nbhr[nj].nnd == MAXNBHR) 
						strcpy (oapiDebugString(), "Problems!");
					else {
						nbhr[nj].nd[m] = nk;
						nbhr[nj].dst0[m] = Dst (vtx+nj, vtx+nk);
						nbhr[nj].nnd++;
					}
				}
			}
		}
	}
	sail_nvtx = nvtx;
	sail_ntri = ntri;
}

// --------------------------------------------------------------
// Constructor
// --------------------------------------------------------------
SolarSail::SolarSail (OBJHANDLE hVessel, int flightmodel)
: VESSEL3 (hVessel, flightmodel)
{
	int i;

	hMesh = NULL;
	mf = _V(0,0,0);
	DefineAnimations();
	for (i = 0; i < 4; i++)
		paddle_rot[i] = paddle_vis[i] = 0.5;
}

// --------------------------------------------------------------
// Define animation sequences for moving parts
// --------------------------------------------------------------
void SolarSail::DefineAnimations ()
{
	int i;

	// Steering paddle animations
	static UINT PaddleGrp[4] = {GRP_paddle1, GRP_paddle2, GRP_paddle3, GRP_paddle4};

	static MGROUP_ROTATE Paddle0 (0, PaddleGrp+0, 1, _V(0,0,0), _V(0,1,0), (float)(180.0*RAD));
	static MGROUP_ROTATE Paddle1 (0, PaddleGrp+1, 1, _V(0,0,0), _V(1,0,0), (float)(180.0*RAD));
	static MGROUP_ROTATE Paddle2 (0, PaddleGrp+2, 1, _V(0,0,0), _V(0,1,0), (float)(180.0*RAD));
	static MGROUP_ROTATE Paddle3 (0, PaddleGrp+3, 1, _V(0,0,0), _V(1,0,0), (float)(180.0*RAD));
	static MGROUP_ROTATE *Paddle[4] = {&Paddle0, &Paddle1, &Paddle2, &Paddle3};

	for (i = 0; i < 4; i++) {
		anim_paddle[i] = CreateAnimation(0.5);
		AddAnimationComponent (anim_paddle[i], 0, 1, Paddle[i]);
	}
}

// --------------------------------------------------------------
// Update sail nodal displacements
// --------------------------------------------------------------
void SolarSail::UpdateSail (const VECTOR3 *rpressure)
{
	// A kind of poor man's finite element code. Should eventually be done properly!
	
	static int sailidx = 0;

	const double elast = 1e-1;
	const double pscale = 1e3;
	DWORD i, j;
	NBHR *nb;
	NTVERTEX *vi, *vj;
	VECTOR3 dv, F, nm;
	static VECTOR3 *nml = 0;
	static DWORD *nsd = 0;
	double dst;

	sailidx = ++sailidx % 4;
	MESHGROUP *sail = oapiMeshGroup (hMesh, sailidx);

	for (i = 0; i < sail_nvtx; i++) {
		F = _V(0,0,rpressure->z*pscale); // note - should be calculated for LOCAL normal
		vi = sail->Vtx+i;
		nb = nbhr+i;
		if (nb->fix) {
			sail_vbuf[i] = _V(0,0,0);
			continue;
		}
		for (j = 0; j < nb->nnd; j++) {
			vj = sail->Vtx+nb->nd[j];
			dv.x = vj->x - vi->x;
			dv.y = vj->y - vi->y;
			dv.z = vj->z - vi->z;
			dst = length(dv);
			if (dst > nb->dst0[j]) { // is stretched
				F += dv*(elast/nb->dst0[j]);
			}
		}
		sail_vbuf[i] = F;
	}
	for (i = 0; i < sail_nvtx; i++) {
		sail->Vtx[i].x += (float)sail_vbuf[i].x;
		sail->Vtx[i].y += (float)sail_vbuf[i].y;
		sail->Vtx[i].z += (float)sail_vbuf[i].z;
	}
	for (i = 0; i < sail_nvtx; i++) {
		sail->Vtx[i+sail_nvtx].x += (float)sail_vbuf[i].x;
		sail->Vtx[i+sail_nvtx].y += (float)sail_vbuf[i].y;
		sail->Vtx[i+sail_nvtx].z += (float)sail_vbuf[i].z;
	}

	// calculate smooth normals - surely this could be done more efficiently!
	if (!nml) {
		nml = new VECTOR3[sail_nvtx];
		nsd = new DWORD[sail_nvtx];
	}
	memset (nml, 0, sail_nvtx*sizeof(VECTOR3));
	memset (nsd, 0, sail_nvtx*sizeof(DWORD));
	for (i = 0; i < sail_ntri; i++) {
		WORD *idx = sail->Idx + i*3;
		nm = Nml(sail->Vtx + *idx, sail->Vtx + *(idx+1), sail->Vtx + *(idx+2));
		for (j = 0; j < 3; j++) {
			nml[*(idx+j)] += unit(nm);
			nsd[*(idx+j)]++;
		}
	}
	for (i = 0; i < sail_nvtx; i++) {
		sail->Vtx[i].nx = (float)nml[i].x/nsd[i];
		sail->Vtx[i].ny = (float)nml[i].y/nsd[i];
		sail->Vtx[i].nz = (float)nml[i].z/nsd[i];
	}
	for (i = 0; i < sail_nvtx; i++) {
		sail->Vtx[i+sail_nvtx].nx = -(float)nml[i].x/nsd[i];
		sail->Vtx[i+sail_nvtx].ny = -(float)nml[i].y/nsd[i];
		sail->Vtx[i+sail_nvtx].nz = -(float)nml[i].z/nsd[i];
	}
}

// --------------------------------------------------------------
// Low-level steering: adjust the position of a paddle
// --------------------------------------------------------------
void SolarSail::SetPaddle (int p, double pos)
{
	paddle_rot[p] = pos;
}

// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set the capabilities of the vessel class
// --------------------------------------------------------------
void SolarSail::clbkSetClassCaps (FILEHANDLE cfg)
{
	// physical specs
	SetSize (SAIL_RADIUS*2.0);
	SetEmptyMass (100.0);
	SetCW (0.3, 0.3, 0.6, 0.9);
	SetWingAspect (0.7);
	SetWingEffectiveness (2.5);
	SetCrossSections (_V(10.5,15.0,5.8));
	SetRotDrag (_V(0.6,0.6,0.35));
	if (GetFlightModel() >= 1) {
		SetPitchMomentScale (1e-4);
		SetYawMomentScale (1e-4);
	}
	SetPMI (_V(3e3,3e3,6e3));
	SetTrimScale (0.05);
	SetCameraOffset (_V(0,0.8,0));
	SetLiftCoeffFunc (LiftCoeff);
	SetDockParams (_V(0,1.3,-1), _V(0,1,0), _V(0,0,-1));
	SetTouchdownPoints (_V(0,-1.5,2), _V(-1,-1.5,-1.5), _V(1,-1.5,-1.5));

	// visual specs
	AddMesh (hMeshTpl);
}

// --------------------------------------------------------------
// Frame pre-update
// --------------------------------------------------------------
void SolarSail::clbkPreStep (double simt, double simdt, double mjd)
{
	// calculate solar pressure on steering paddles
	int i;
	const double paddle_area = 7812.5;
	const double albedo = 2.0;
	static VECTOR3 ppos[4] = {
		_V(0,-550,0), _V(-550,0,0), _V(0,550,0), _V(550,0,0)
	};
	VECTOR3 nml;
	for (i = 0; i < 4; i++) {
		double phi = (paddle_rot[i]-0.5)*PI;
		double sphi = sin(phi), cphi = cos(phi);
		if (i%2 == 0) nml = _V(-sphi,0,cphi);
		else          nml = _V(0,sphi,cphi);
		double f = dotp (mf, nml);
		if (f < 0) {
			nml = -nml;
			f = -f;
		}
		f *= paddle_area*albedo;
		AddForce (nml*f, ppos[i]);
	}
}

// --------------------------------------------------------------
// Frame update
// --------------------------------------------------------------
void SolarSail::clbkPostStep (double simt, double simdt, double mjd)
{
	int i;

	if (hMesh) UpdateSail (&mf);

	for (i = 0; i < 4; i++) {
		if (paddle_vis[i] != paddle_rot[i])
			SetAnimation (anim_paddle[i], paddle_vis[i] = paddle_rot[i]);
	}
}

// --------------------------------------------------------------
// Implement effects of radiation pressure
// --------------------------------------------------------------
void SolarSail::clbkGetRadiationForce (const VECTOR3 &mflux, VECTOR3 &F, VECTOR3 &pos)
{
	mf = mflux;                   // store flux value
	const double albedo = 2.0;    // fully reflective
	const double area = SAIL_RADIUS*SAIL_RADIUS*PI;

	// The sail is oriented normal to the vessel z-axis.
	// Therefore only the z-component of the radiation momentum flux contributes
	// to change the sail's momentum (Fresnel reflection)
	double mom = mflux.z * albedo *area;
	F = _V(0,0,mom);
	pos = _V(0,0,0);        // don't induce torque
}

// --------------------------------------------------------------
// Create SolarSail visual
// --------------------------------------------------------------
void SolarSail::clbkVisualCreated (VISHANDLE vis, int refcount)
{
	hMesh = GetMesh (vis, 0);
}

// --------------------------------------------------------------
// Destroy SolarSail visual
// --------------------------------------------------------------
void SolarSail::clbkVisualDestroyed (VISHANDLE vis, int refcount)
{
	hMesh = NULL;
}

// --------------------------------------------------------------
// Respond to generic messages
// --------------------------------------------------------------
int SolarSail::clbkGeneric (int msgid, int prm, void *context)
{
	switch (msgid) {
	case VMSG_LUAINTERPRETER:
		return Lua_InitInterpreter (context);
	case VMSG_LUAINSTANCE:
		return Lua_InitInstance (context);
	}
	return 0;
}

// --------------------------------------------------------------
// Static member initialisations
// --------------------------------------------------------------
MESHHANDLE SolarSail::hMeshTpl = NULL;
NBHR *SolarSail::nbhr = NULL;
VECTOR3 *SolarSail::sail_vbuf = NULL;
DWORD SolarSail::sail_nvtx = 0;
DWORD SolarSail::sail_ntri = 0;

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Global initialisation
// --------------------------------------------------------------

DLLCLBK void InitModule (HINSTANCE hModule)
{
	SolarSail::GlobalSetup();
}

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new SolarSail (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (SolarSail*)vessel;
}
