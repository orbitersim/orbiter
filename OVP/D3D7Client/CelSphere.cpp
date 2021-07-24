// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// CelSphere.cpp
// Class CelestialSphere (implementation)
//
// This class is responsible for rendering the celestial sphere
// background (stars, constellations, grids, labels, etc.)
// ==============================================================

#include "CelSphere.h"

#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

CelestialSphere::CelestialSphere (D3D7Client *_gc)
{
	gc = _gc;
	sphere_r = 1e3f; // the actual render distance for the celestial sphere
	                 // is irrelevant, since it is rendered without z-buffer,
	                 // but it must be within the fustrum limits - check this
	                 // in case the near and far planes are dynamically changed!
	LoadStars ();
	LoadConstellationLines ();
	AllocGrids ();
}

// ==============================================================

CelestialSphere::~CelestialSphere ()
{
	DWORD i;

	if (nsbuf) {
		for (i = 0; i < nsbuf; i++)	svtx[i]->Release();
		delete []svtx;
	}
	if (ncline) delete []cnstvtx;
	grdlng->Release();
	grdlat->Release();
}

// ==============================================================

void CelestialSphere::LoadStars ()
{
	StarRenderPrm *prm = (StarRenderPrm*)gc->GetConfigParam (CFGPRM_STARRENDERPRM);

	double a, b, xz;

	if (prm->mag_lo > prm->mag_hi) {
		if (prm->map_log) {
			// scaling factors for logarithmic brightness mapping
			a = -log(prm->brt_min)/(prm->mag_lo-prm->mag_hi);
		} else {
			// scaling factors for linear brightness mapping
			a = (1.0-prm->brt_min)/(prm->mag_hi-prm->mag_lo);
			b = prm->brt_min - prm->mag_lo*a;
		}
	} else {
		oapiWriteLog("WARNING: Inconsistent magnitude limits for background star brightness. Disabling background stars.");
	}

	float c;
	int lvl, plvl = 256;
	DWORD i, j, k, nv, idx = 0;
	DWORD buflen = D3DMAXNUMVERTICES;
	DWORD bufsize = 16;
	nsbuf = nsvtx = 0;
	svtx = new LPDIRECT3DVERTEXBUFFER7[bufsize];

	D3DVERTEXBUFFERDESC vbdesc;
	gc->SetDefault (vbdesc);
	vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;

	struct StarRec {
		float lng, lat, mag;
	} *data = new StarRec[buflen];

	if (prm->mag_lo <= prm->mag_hi) return;

	// Read binary data from file
	FILE *f = fopen ("Star.bin", "rb");
	if (!f) return;
	while (nv = fread (data, sizeof(StarRec), buflen, f)) {
		// limit number of stars to predefined magnitude - SHOULD BE BINARY SEARCH
		for (i = 0; i < nv; i++)
			if (data[i].mag > prm->mag_lo) { nv = i; break; }
		if (nv) {
			if (nsbuf >= bufsize) { // grow vertex buffer list
				LPDIRECT3DVERTEXBUFFER7 *tmp = new LPDIRECT3DVERTEXBUFFER7[bufsize+16];
				memcpy (tmp, svtx, bufsize*sizeof(LPDIRECT3DVERTEXBUFFER7*));
				delete []svtx;
				svtx = tmp;
				bufsize += 16;
			}
			vbdesc.dwNumVertices = nv;
			gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, svtx+nsbuf, 0);
			VERTEX_XYZC *vbuf;
			svtx[nsbuf]->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
			for (j = 0; j < nv; j++) {
				StarRec &rec = data[j];
				VERTEX_XYZC &v = vbuf[j];
				xz = sphere_r * cos (rec.lat);
				v.x = (float)(xz * cos (rec.lng));
				v.z = (float)(xz * sin (rec.lng));
				v.y = (float)(sphere_r * sin (rec.lat));

				if (prm->map_log)
					c = (float)min (1.0, max (prm->brt_min, exp(-(rec.mag-prm->mag_hi)*a)));
				else
					c = (float)min (1.0, max (prm->brt_min, a*rec.mag+b));

				v.col = D3DRGBA (c,c,c,1);
				lvl = (int)(c*256.0*0.5);
				if (lvl > 255) lvl = 255;
				for (k = lvl; k < (DWORD)plvl; k++) lvlid[k] = idx;
				plvl = lvl;
				idx++;
			}
			svtx[nsbuf]->Unlock();
			svtx[nsbuf]->Optimize (gc->GetDevice(), 0);
			nsvtx += nv;
			nsbuf++;
		}
		if (nv < buflen) break;
	}
	fclose (f);

	if (bufsize > nsbuf) { // shrink buffer list to size
		LPDIRECT3DVERTEXBUFFER7 *tmp = new LPDIRECT3DVERTEXBUFFER7[nsbuf];
		memcpy (tmp, svtx, nsbuf*sizeof(LPDIRECT3DVERTEXBUFFER7*));
		delete []svtx;
		svtx = tmp;
	}

	for (i = 0; i < (DWORD)plvl; i++) lvlid[i] = idx;

	delete []data;
}

// ==============================================================

void CelestialSphere::LoadConstellationLines ()
{
	const DWORD maxline = 1000; // plenty for default data base, but check with custom data bases!

	GraphicsClient::ConstRec *cline = new GraphicsClient::ConstRec[maxline];
	ncline = gc->LoadConstellationLines (maxline, cline);
	if (ncline) {
		cnstvtx = new VERTEX_XYZ[ncline*2]; // two end points per line
		DWORD n;
		double xz;
		for (n = 0; n < ncline; n++) {
			GraphicsClient::ConstRec *rec = cline+n;
			xz = sphere_r * cos (rec->lat1);
			cnstvtx[n*2].x = (float)(xz * cos(rec->lng1));
			cnstvtx[n*2].z = (float)(xz * sin(rec->lng1));
			cnstvtx[n*2].y = (float)(sphere_r * sin(rec->lat1));
			xz = sphere_r * cos (rec->lat2);
			cnstvtx[n*2+1].x = (float)(xz * cos(rec->lng2));
			cnstvtx[n*2+1].z = (float)(xz * sin(rec->lng2));
			cnstvtx[n*2+1].y = (float)(sphere_r * sin(rec->lat2));
		}
	}
	delete []cline;
}

// ==============================================================

void CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;

	D3DVERTEXBUFFERDESC vbdesc;
	gc->SetDefault (vbdesc);
	vbdesc.dwFVF  = D3DFVF_XYZ;
	vbdesc.dwNumVertices = (NSEG+1) * 11;
	gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &grdlng, 0);
	VERTEX_XYZ *vbuf;
	grdlng->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j <= 10; j++) {
		lat = (j-5)*15*RAD;
		xz = sphere_r * cos(lat);
		y  = sphere_r * sin(lat);
		for (i = 0; i <= NSEG; i++) {
			lng = 2.0*PI * (double)i/(double)NSEG;
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlng->Unlock();
	grdlng->Optimize (gc->GetDevice(), 0);

	vbdesc.dwNumVertices = (NSEG+1) * 12;
	gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &grdlat, 0);
	grdlat->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j < 12; j++) {
		lng = j*15*RAD;
		for (i = 0; i <= NSEG; i++) {
			lat = 2.0*PI * (double)i/(double)NSEG;
			xz = sphere_r * cos(lat);
			y  = sphere_r * sin(lat);
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlat->Unlock();
	grdlat->Optimize (gc->GetDevice(), 0);
}

// ==============================================================

void CelestialSphere::RenderStars (LPDIRECT3DDEVICE7 dev, DWORD nmax, const VECTOR3 *bgcol)
{
	// render in chunks, because some graphics cards have a limit in the
	// vertex list size

	if (!nsvtx) return; // nothing to do

	DWORD i, j, ns = nsvtx;

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min (255, (int)((min(bgcol->x,1.0) + min(bgcol->y,1.0) + min(bgcol->z,1.0))*128.0));
		ns = min ((int)ns, lvlid[bglvl]);
	}

	for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
		dev->DrawPrimitiveVB (D3DPT_POINTLIST, svtx[j], 0, min (ns-i, D3DMAXNUMVERTICES), 0);
}

// ==============================================================

void CelestialSphere::RenderConstellations (LPDIRECT3DDEVICE7 dev, VECTOR3 &col)
{
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	dev->DrawPrimitive (D3DPT_LINELIST, D3DFVF_XYZ, cnstvtx, ncline*2, 0);
}

// ==============================================================

void CelestialSphere::RenderGreatCircle (LPDIRECT3DDEVICE7 dev, VECTOR3 &col)
{
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	dev->DrawPrimitiveVB (D3DPT_LINESTRIP, grdlng, 5*(NSEG+1), NSEG+1, 0);
}

// ==============================================================

void CelestialSphere::RenderGrid (LPDIRECT3DDEVICE7 dev, VECTOR3 &col, bool eqline)
{
	int i;
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, grdlng, i*(NSEG+1), NSEG+1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, grdlat, i*(NSEG+1), NSEG+1, 0);
}
