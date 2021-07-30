// ==============================================================
// CelSphere.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// Class CelestialSphere (implementation)
//
// This class is responsible for rendering the celestial sphere
// background (stars, constellations, grids, labels, etc.)
// ==============================================================

#include "CelSphere.h"
#include "D3D9Config.h"
#include "AABBUtil.h"


#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

CelestialSphere::CelestialSphere(D3D9Client *_gc)
	: gc(_gc)
	, pDevice(_gc->GetDevice())
	, maxNumVertices(_gc->GetHardwareCaps()->MaxPrimitiveCount)
{
	sphere_r = 1e6f; // the actual render distance for the celestial sphere
	                 // is irrelevant, since it is rendered without z-buffer,
	                 // but it must be within the frustum limits - check this
	                 // in case the near and far planes are dynamically changed!
	LoadStars ();
	LoadConstellationLines ();
	AllocGrids ();
}

// ==============================================================

CelestialSphere::~CelestialSphere()
{
	if (nsbuf) {
		for (DWORD i = 0; i < nsbuf; ++i) {
			svtx[i]->Release();
		}
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
		oapiWriteLog("D3D9: WARNING: Inconsistent magnitude limits for background star brightness. Disabling background stars.");
	}

	DWORD nv;
	float c;
	int lvl, plvl = 256;
	DWORD i, j, k, idx = 0;
	DWORD bufsize = 16;
	nsbuf = 0;
	nsvtx = 0;
	svtx = new LPDIRECT3DVERTEXBUFFER9[bufsize];

	struct StarRec {
		float lng, lat, mag;
	} *data = new StarRec[maxNumVertices];

	if (prm->mag_lo <= prm->mag_hi) { delete []data; return; }

	// Read binary data from file
	FILE *f;
	fopen_s(&f, "Star.bin", "rb");
	if (!f) { delete []data; return; }
	while (nv = DWORD(fread (data, sizeof(StarRec), maxNumVertices, f))) {
		// limit number of stars to predefined magnitude - SHOULD BE BINARY SEARCH
		for (i = 0; i < nv; i++)
			if (data[i].mag > prm->mag_lo) { nv = i; break; }
		if (nv) {
			if (nsbuf >= bufsize) { // grow vertex buffer list
				LPDIRECT3DVERTEXBUFFER9 *tmp = new LPDIRECT3DVERTEXBUFFER9[bufsize+16];
				memcpy2 (tmp, svtx, bufsize*sizeof(LPDIRECT3DVERTEXBUFFER9));
				delete []svtx;
				svtx = tmp;
				bufsize += 16;
			}
			
			pDevice->CreateVertexBuffer(UINT(nv*sizeof(VERTEX_XYZC)), D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &svtx[nsbuf], NULL);
			VERTEX_XYZC *vbuf;
			svtx[nsbuf]->Lock(0, 0, (LPVOID*)&vbuf, 0);
			for (j = 0; j < nv; j++) {
				StarRec &rec = data[j];
				VERTEX_XYZC &v = vbuf[j];
				xz = sphere_r * cos (rec.lat);
				v.x = (float)(xz * cos (rec.lng));
				v.z = (float)(xz * sin (rec.lng));
				v.y = (float)(sphere_r * sin (rec.lat));

				if (prm->map_log) c = (float)min (1.0, max (prm->brt_min, exp(-(rec.mag-prm->mag_hi)*a)));
				else 			  c = (float)min (1.0, max (prm->brt_min, a*rec.mag+b));

				v.col = D3DXCOLOR(c,c,c,1);
				lvl = (int)(c*256.0*0.5);
				if (lvl > 255) lvl = 255;
				for (k = lvl; k < (DWORD)plvl; k++) lvlid[k] = idx;
				plvl = lvl;
				idx++;
			}
			svtx[nsbuf]->Unlock();
			nsvtx += nv;
			nsbuf++;
		}
		if (nv < maxNumVertices) break;
	}
	fclose (f);

	if (bufsize > nsbuf) { // shrink buffer list to size
		LPDIRECT3DVERTEXBUFFER9 *tmp = new LPDIRECT3DVERTEXBUFFER9[nsbuf];
		memcpy2 (tmp, svtx, nsbuf*sizeof(LPDIRECT3DVERTEXBUFFER9));
		delete []svtx;
		svtx = tmp;
	}

	for (i = 0; i < (DWORD)plvl; i++) lvlid[i] = idx;

	delete []data;
}

// ==============================================================

void CelestialSphere::LoadConstellationLines()
{
	gc->WriteLog("[Loading Constellations]");
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
	VERTEX_XYZ *vbuf;

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*11, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &grdlng, NULL);
	grdlng->Lock (0, 0, (LPVOID*)&vbuf, 0);
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

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*12, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &grdlat, NULL);
	grdlat->Lock (0, 0, (LPVOID*)&vbuf, 0);
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
}

// ==============================================================

void CelestialSphere::RenderStars(ID3DXEffect *FX, DWORD nmax, const VECTOR3 *bgcol)
{
	_TRACE;

	// render in chunks, because some graphics cards have a limit in the
	// vertex list size
	UINT i, j, numPasses = 0;
	if (nmax > nsvtx) nmax = nsvtx; // sanity check

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min (255, (int)((min(bgcol->x,1.0) + min(bgcol->y,1.0) + min(bgcol->z,1.0))*128.0));
		nmax = min (nmax, (DWORD)lvlid[bglvl]);
	}

	HR(pDevice->SetVertexDeclaration(pPosColorDecl));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < nmax; i += maxNumVertices, j++)
	{
		HR(pDevice->SetStreamSource(0, svtx[j], 0, sizeof(VERTEX_XYZC)));
		HR(pDevice->DrawPrimitive(D3DPT_POINTLIST, 0, min (nmax-i, maxNumVertices)));
	}
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void CelestialSphere::RenderConstellations(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->DrawPrimitiveUP(D3DPT_LINELIST, ncline, cnstvtx, sizeof(VERTEX_XYZ)));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void CelestialSphere::RenderGreatCircle(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDevice->SetStreamSource(0, grdlng, 0, sizeof(VERTEX_XYZ)));
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, 5*(NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
	
}

// ==============================================================

void CelestialSphere::RenderGrid(ID3DXEffect *FX, bool eqline)
{
	_TRACE;
	int i;
	UINT numPasses = 0;
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->SetStreamSource(0, grdlng, 0, sizeof(VERTEX_XYZ)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)	HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i*(NSEG+1), NSEG));
	HR(pDevice->SetStreamSource(0, grdlat, 0, sizeof(VERTEX_XYZ)));
	for (i = 0; i < 12; i++) HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i * (NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
}
