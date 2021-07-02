#include "Orbiter.h"
#include "Body.h"
#include "Planet.h"
#include "Surface.h"
#include "Camera.h"
#include "VPlanet.h"
#include "Texture.h"
#include "D3dmath.h"
#include <stdio.h>

extern Orbiter *g_pOrbiter;
extern double SimT;
extern Camera *g_camera;
extern double g_farplane;
extern char DBG_MSG[256];

#define BASE_NLNG 256
#define BASE_NLAT 64

static const int NLNG[MAXLEVEL] = {256,512,1024,2048,4096}; // #patches in longitude direction
static const int NLAT[MAXLEVEL] = { 64,128, 256, 512,1024}; // #patches in latitude direction
static double DLNG[MAXLEVEL];                               // angular longitude range per patch
static double DLAT[MAXLEVEL];                               // angular latitude range per patch
static int IDXOFS[MAXLEVEL+1];                              // offsets into the index list

static const double pole_limit = 0.5*Pi05; 
static double base_dlng = Pi2/(double)BASE_NLNG;
static double base_dlat = 2.0*pole_limit/(double)BASE_NLAT;
static double base_lng[BASE_NLNG+1];
static double base_slng[BASE_NLNG+1];
static double base_clng[BASE_NLNG+1];
static double base_lat[BASE_NLAT+1];
static double base_slat[BASE_NLAT+1];
static double base_clat[BASE_NLAT+1];
static bool setup = false;

// the odd numbers are to avoid borders around the textures
// (assuming 32x32 pixel textures embedded in 256x256 files)
// 8 rotation+mirror states for 4 tile corners
static float tu[8*4] = {
	 0.001953125f, 0.001953125f, 0.123046875f, 0.123046875f,
	 0.001953125f, 0.123046875f, 0.001953125f, 0.123046875f,
	 0.123046875f, 0.123046875f, 0.001953125f, 0.001953125f,
	 0.123046875f, 0.001953125f, 0.123046875f, 0.001953125f,
	 0.123046875f, 0.123046875f, 0.001953125f, 0.001953125f,
	 0.123046875f, 0.001953125f, 0.123046875f, 0.001953125f,
	 0.001953125f, 0.001953125f, 0.123046875f, 0.123046875f,
	 0.001953125f, 0.123046875f, 0.001953125f, 0.123046875f};
static float tv[8*4] = {
	 0.123046875f, 0.001953125f, 0.123046875f, 0.001953125f,
	 0.001953125f, 0.001953125f, 0.123046875f, 0.123046875f,
	 0.001953125f, 0.123046875f, 0.001953125f, 0.123046875f,
	 0.123046875f, 0.123046875f, 0.001953125f, 0.001953125f,
	 0.123046875f, 0.001953125f, 0.123046875f, 0.001953125f,
	 0.001953125f, 0.001953125f, 0.123046875f, 0.123046875f,
	 0.001953125f, 0.123046875f, 0.001953125f, 0.123046875f,
	 0.123046875f, 0.123046875f, 0.001953125f, 0.001953125f};

DynSurface::DynSurface (const Body *_body, const VPlanet *_vplanet)
: body(_body), vplanet(_vplanet)
{
	int i;
	//char cbuf[256];

	updT = -1.0;
	updDT = 1.0;

	if (body->Type() == OBJTP_PLANET) {
		planet = (Planet*)body;
		maxres = planet->max_dyn_level;
		if (maxres > MAXLEVEL) maxres = MAXLEVEL;
		res_dist = new double[maxres]; TRACENEW
		tex = planet->generictex;
		ntex = planet->ngenerictex;
	} else {
		planet = 0;
		maxres = 0;
		res_dist = 0;
	}

	if (!setup) {
		IDXOFS[0] = 0;
		for (i = 0; i < MAXLEVEL; i++) {
			DLNG[i] = Pi2/(double)NLNG[i];
			DLAT[i] = 2.0*pole_limit/(double)NLAT[i];
			IDXOFS[i+1] = IDXOFS[i] + NLNG[i]*NLAT[i];
		}

		for (i = 0; i <= BASE_NLNG; i++) {
			base_lng[i] = i*base_dlng - Pi;
			base_slng[i] = sin(base_lng[i]);
			base_clng[i] = cos(base_lng[i]);
		}
		for (i = 0; i <= BASE_NLAT; i++) {
			base_lat[i] = i*base_dlat - pole_limit;
			base_slat[i] = sin(base_lat[i]);
			base_clat[i] = cos(base_lat[i]);
		}
		setup = true;
	}

	dbase = 0;
	if (maxres) {
		FILE *file;
		if (file = fopen (g_pOrbiter->TexPath (body->Name(), ".dat"), "rb")) {
			int size = IDXOFS[maxres];
			dbase = new unsigned short[size]; TRACENEW
			fread (dbase, sizeof (unsigned short), size, file);
			fclose (file);
		}
	}

	Init();
}

DynSurface::~DynSurface ()
{
	if (dbase) delete []dbase;
	if (res_dist) delete []res_dist;

	for (int res = 0; res < maxres; res++)
		if (smesh.nVtx[res]) {
			delete []smesh.Vtx[res];
			delete []smesh.Texid[res];
		}
}

void DynSurface::Init ()
{
	static const double res_factor = 1e-3; // resolution scaling factor
	int res;

	for (res = 1; res < maxres; res++) {
		res_dist[res] = res_factor/(vplanet->ScaleFactor() * (pow(10.0,res)-1.0));
		// the distance at which resolution changes between res and res-1
		// by definition res_dist[0] = Infinity
	}

	for (res = 0; res < maxres; res++) {
		smesh.nVtx[res] = 0;
		smesh.iLng0[res] = -1;
	}
}

inline void SetVertex (D3DVERTEX *vtx, double slng, double clng, double slat, double clat, D3DVALUE rad)
{
	vtx->x = (vtx->nx = (D3DVALUE)(clat*clng)) * rad;
	vtx->y = (vtx->ny = (D3DVALUE)slat) * rad;
	vtx->z = (vtx->nz = (D3DVALUE)(clat*slng)) * rad;
}

void DynSurface::Render (LPDIRECT3DDEVICE7 dev)
{
	static const double maxdist = 0.9*g_farplane;
	int i, j, res0, res, ilng, ilat, ilng0, ilng1, ilat0, ilat1, nx, ny, i0, j0;
	int nstep, hx0, hy0, hx1, hy1, pilng0, pilng1, pilat0, pilat1;
	int tex_idx, file_idx_old, file_idx, file_row, file_col, *tid;
	unsigned short *dbr, db, rot = 0;
	double lng, lat, lg, lt, rad, beta;
	float scale, *rtu, *rtv;
	D3DVALUE dtu, dtv;
	bool do_scale, final_res;
	Vector pcam;
	D3DVERTEX *vtx;
	double cdist = vplanet->CDist();               // distance camera <-> planet centre
	double prad = body->Size();                    // planet radius
	D3DVALUE fr = (D3DVALUE)prad;
	if (cdist-prad < 10.0) cdist = prad+10.0;      // sanity
	double calt = cdist-prad;                      // camera altitude
	double alpha = acos (prad/cdist);              // angular radius of visibility
	double hdist = sqrt (cdist*cdist - prad*prad); // distance to horizon
	if (hdist < 100.0) hdist = 100.0;              // sanity
	body->GlobalToLocal (g_camera->GPos(), pcam);  // camera in planet coords
	body->LocalToEquatorial (pcam, lng, lat, rad); // camera in polar coords

	// World transformation matrix
	if (do_scale = (hdist > maxdist)) scale = (float)(maxdist / hdist);
	D3DMATRIX br;
	SetInvD3DRotation (br, body->GRot());
	br._41 = br._42 = br._43 = br._14  = br._24 = br._34 = 0.0f;
	br._44 = 1.0f;
	if (do_scale) {
		br._11 *= scale, br._12 *= scale, br._13 *= scale;
		br._21 *= scale, br._22 *= scale, br._23 *= scale;
		br._31 *= scale, br._32 *= scale, br._33 *= scale;
	}
	// premult with camera translation
	float px = -(float)pcam.x, py = -(float)pcam.y, pz = -(float)pcam.z;
	br._41 = px*br._11 + py*br._21 + pz*br._31;
	br._42 = px*br._12 + py*br._22 + pz*br._32;
	br._43 = px*br._13 + py*br._23 + pz*br._33;

	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &br);

#ifdef UNDEF
	// Add fog
	// NOTE: The fog colour needs further thought (needs to be black on the
	// night side)
	if (planet->HasAtmosphere()) {
		if (SimT > updT) {
			static const double rho_min = 6.5e3;
			static const double rho_max = 6.5e4;
			const AtmosphereParams *atm = planet->AtmParams();
			double rho = planet->AtmDensity (cdist);
			double th0 = (atm->rho0 - rho) / atm->C;
			// atmospheric thickness when looking down
			double th1 = th0 * hdist/calt;
			// atmospheric thickness when looking towards horizon
			// approximation! underestimates thickness at higher altitudes
			fogStart = (float)(hdist * (rho_min-th0)/(th1-th0));
			fogEnd   = (float)(hdist * (rho_max-th0)/(th1-th0));
			if (do_scale) fogStart *= scale, fogEnd *= scale;
			updT = SimT + updDT;
		}
		dev->SetRenderState(D3DRENDERSTATE_FOGENABLE, TRUE);
		dev->SetRenderState(D3DRENDERSTATE_FOGCOLOR, 0x00C0C0FF);
		dev->SetRenderState(D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_LINEAR);
		dev->SetRenderState(D3DRENDERSTATE_FOGTABLESTART, *(DWORD*)(&fogStart));
		dev->SetRenderState(D3DRENDERSTATE_FOGTABLEEND,   *(DWORD*)(&fogEnd));
	}
#endif

	for (i = 1, res0 = maxres-1; i < maxres; i++)
		if (calt > res_dist[i]) { res0 = i-1; break; }

	for (res = res0, final_res = false;; res--) {

		if (res) {
			// range for this resolution
			beta = acos ((prad*prad + cdist*cdist - res_dist[res]*res_dist[res])/(2.0*prad*cdist));
			if (beta > alpha) {
				beta = alpha; // limit is visibility range
				final_res = true; // termination flag
			}
		} else {
			beta = alpha; // go to visibility limit for lowest resolution
			final_res = true;
		}
		if (res < res0) {
			pilng0 = ilng0/2;
			pilng1 = ilng1/2;
			pilat0 = ilat0/2;
			pilat1 = ilat1/2;
		}
		nstep = (int)(beta/DLNG[res])+1;       // semi-dimension of patch grid
		if (!nstep) nstep = 2;
		else if (nstep & 1) nstep++;           // need even number here

		ilng = (int)((lng+Pi)/DLNG[res]);
		if (ilng & 1) ilng++;
		ilng0 = ilng-nstep; ilng1 = ilng+nstep; // left and right tile limits
		ilat = (int)((lat+pole_limit)/DLAT[res]);
		if (ilat & 1) ilat++;
		if ((ilat0 = ilat-nstep) < 0) ilat0 = 0;
		if ((ilat1 = ilat+nstep) > NLAT[res]) ilat1 = NLAT[res];
		nx = ilng1-ilng0, ny = ilat1-ilat0;
		if (res < res0) {
			hx0 = pilng0-ilng0; hx1 = pilng1-ilng0;
			hy0 = pilat0-ilat0; hy1 = pilat1-ilat0;
		}

		// generate vertices
		if (ilng0 != smesh.iLng0[res] || ilng1 != smesh.iLng1[res] ||
			ilat0 != smesh.iLat0[res] || ilat1 != smesh.iLat1[res]) {

			int nnd = nx*ny*4;
			if (smesh.nVtx[res] < nnd) {
				if (smesh.nVtx[res]) {
					delete []smesh.Vtx[res];
					delete []smesh.Texid[res];
				}
				smesh.Vtx[res] = new D3DVERTEX[smesh.nVtx[res]=nnd]; TRACENEW
				smesh.Texid[res] = new int[nnd]; TRACENEW
			}
			double *slng = new double[nx+1]; TRACENEW
			double *clng = new double[nx+1]; TRACENEW
			double *slat = new double[ny+1]; TRACENEW
			double *clat = new double[ny+1]; TRACENEW
			for (i = 0; i <= nx; i++) {
				lg = (i+ilng0)*DLNG[res]-Pi;
				slng[i] = sin(lg), clng[i] = cos(lg);
			}
			for (i = 0; i <= ny; i++) {
				lt = (i+ilat0)*DLAT[res]-pole_limit;
				slat[i] = sin(lt), clat[i] = cos(lt);
			}
			vtx = smesh.Vtx[res];
			tid = smesh.Texid[res];
			dbr = (dbase ? dbase+IDXOFS[res] : 0);
			for (j = 0; j < ny; j++) {
				j0 = NLAT[res]-1-(ilat0+j);
				for (i = 0; i < nx; i++) {
					// texture coordinates
					i0 = (i+ilng0+NLNG[res]) % NLNG[res];
					if (dbr) {
						db = dbr[j0*NLNG[res] + i0];
						tex_idx  = db & 0xFF;
						file_idx = tex_idx >> 6;
						*tid++ = file_idx + 4*res;
						// assuming there are 4 texture maps at each resolution
						file_row = (tex_idx >> 3) & 7;
						file_col = tex_idx & 7;
						rot = db >> 13;
						dtu = file_col*0.125f;
						dtv = file_row*0.125f;
						rtu = tu + rot*4;
						rtv = tv + rot*4;
					} else {
						*tid++ = -1;
					}
					// vertex 0
					/*if (i) *vtx = *(vtx-2);
					else if (j) *vtx = *(vtx-nx*4+1);
					else*/ SetVertex (vtx, slng[i], clng[i], slat[j], clat[j], fr);
					if (dbr) vtx->tu = rtu[0]+dtu, vtx->tv = rtv[0]+dtv;
					vtx++;
					// vertex 1
					/*if (i) *vtx = *(vtx-2);
					else*/ SetVertex (vtx, slng[i], clng[i], slat[j+1], clat[j+1], fr);
					if (dbr) vtx->tu = rtu[1]+dtu, vtx->tv = rtv[1]+dtv;
					vtx++;
					// vertex 2
					/*if (j) *vtx = *(vtx-nx*4+1);
					else*/ SetVertex (vtx, slng[i+1], clng[i+1], slat[j], clat[j], fr);
					if (dbr) vtx->tu = rtu[2]+dtu, vtx->tv = rtv[2]+dtv;
					vtx++;
					// vertex 3
					SetVertex (vtx, slng[i+1], clng[i+1], slat[j+1], clat[j+1], fr);
					if (dbr) vtx->tu = rtu[3]+dtu, vtx->tv = rtv[3]+dtv;
					vtx++;
				}
			}

			smesh.iLng0[res] = ilng0;
			smesh.iLng1[res] = ilng1;
			smesh.iLat0[res] = ilat0;
			smesh.iLat1[res] = ilat1;
		}

		// render
		vtx = smesh.Vtx[res];
		tid = smesh.Texid[res];
		file_idx_old = -1;
		for (j = 0; j < ny; j++) {
			for (i = 0; i < nx; i++) {

				if (res == res0 || i < hx0 || i >= hx1 || j < hy0 || j >= hy1) {
					// set textures
					if (*tid != file_idx_old) {
						file_idx_old = *tid;
						if (*tid >= 0) dev->SetTexture (0, tex[*tid]);
						else dev->SetTexture (0, NULL);
					}
					dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, vtx, 4, 0);
				}
				vtx += 4;
				tid++;
			}
		}
		if (final_res) break;
	}

	// turn off fog
	dev->SetRenderState(D3DRENDERSTATE_FOGENABLE, FALSE);

	return;
}