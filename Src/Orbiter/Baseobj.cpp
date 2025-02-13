// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <d3d.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include "Orbiter.h"
#include "Config.h"
#include "Planet.h"
#include "Baseobj.h"
#include "Base.h"
#include "Camera.h"
#include "Log.h"
#include "Shadow.h"
#include "Util.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern Camera *g_camera;
extern char DBG_MSG[256];

// ==============================================================================
// class BaseObject

BaseObject::BaseObject (const Base *_base): base(_base)
{
	relpos.x = relpos.y = relpos.z = 0.0;
	scale.x = scale.y = scale.z = 1.0;
	rot = 0.0;
	elev = yofs = 0.0;
}

BaseObject *BaseObject::Create (const Base *_base, istream &is)
{
	char cbuf[256], *tok;
	int err;
	BaseObject *bo = 0;

	for (;;) {
		if (!is.getline (cbuf, 256)) return 0;
		trim_string (cbuf);
		if ((tok = strtok (cbuf, " \t")) == NULL) continue;
		if      (!_stricmp (tok, "END_OBJECTLIST"))
			return 0;
		else if (!_stricmp (tok, "MESH")) {
			bo = new MeshObject (_base); TRACENEW
		} else if (!_stricmp (tok, "BLOCK")) {
			bo = new Block (_base); TRACENEW
		} else if (!_stricmp (tok, "HANGAR")) {
			bo = new Hangar (_base); TRACENEW
		} else if (!_stricmp (tok, "HANGAR2")) {
			bo = new Hangar2 (_base); TRACENEW
		} else if (!_stricmp (tok, "HANGAR3")) {
			bo = new Hangar3 (_base); TRACENEW
		} else if (!_stricmp (tok, "TANK")) {
			bo = new Tank (_base); TRACENEW
		} else if (!_stricmp (tok, "LPAD1")) {
			bo = new Lpad01 (_base); TRACENEW
		} else if (!_stricmp (tok, "LPAD2")) {
			bo = new Lpad02 (_base); TRACENEW
		} else if (!_stricmp (tok, "LPAD2A")) {
			bo = new Lpad02a (_base); TRACENEW
		} else if (!_stricmp (tok, "RUNWAY")) {
			bo = new Runway (_base); TRACENEW
		} else if (!_stricmp (tok, "RUNWAYLIGHTS")) {
			bo = new RunwayLights (_base); TRACENEW
		} else if (!_stricmp (tok, "BEACONARRAY")) {
			bo = new BeaconArray (_base); TRACENEW
		} else if (!_stricmp (tok, "TRAIN1")) {
			bo = new Train1 (_base); TRACENEW
		} else if (!_stricmp (tok, "TRAIN2")) {
			bo = new Train2 (_base); TRACENEW
		} else if (!_stricmp (tok, "SOLARPLANT")) {
			bo = new SolarPlant (_base); TRACENEW
		} else {
			LOGOUT ("BaseObject: Parse error");
			return 0;
		}
		if (bo) {
			if ((err = bo->Read (is)) == 0) {
				return bo;
			} else { // parse error
				delete bo;
				LOGOUT ("BaseObject: Parse error %d", err);
				return 0;
			}
		}
	}
}

int BaseObject::Read (istream &is)
{
	char cbuf[256], label[32], *value, *cp;
	int r, res = 0;
	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		if (!*cp)
			continue; // empty line
		sscanf (cp, "%s", label);
		value = trim_string(cp+strlen(label));
		if (!_stricmp (label, "POS")) {
			if (sscanf (value, "%lf%lf%lf", &relpos.x, &relpos.y, &relpos.z) != 3) {
				ParseError("POS: expected 3 scalar values");
				res = 2;
			}
		} else if (!_stricmp (label, "SCALE")) {
			int nv = sscanf (value, "%lf%lf%lf", &scale.x, &scale.y, &scale.z);
			if (nv < 3) {
				if (nv == 1) {
					scale.y = scale.z = scale.x; // isotropic scaling
				} else {
					ParseError("SCALE: expected 1 or 3 scalar values");
					res = 2;
				}
			}
		} else if (!_stricmp (label, "ROT")) {
			if (sscanf (value, "%lf", &rot) != 1) {
				ParseError("ROT: expected a scalar value");
				res = 2;
			} else {
				rot *= RAD;
			}
		} else {
			r = ParseLine (label, value);
			if (!res) res = r;
		}
	} while (_stricmp (label, "END"));
	return res;
}

void BaseObject::Setup ()
{
	base->Rel_EquPos (relpos, equpos.x, equpos.y);
	elev = base->RefPlanet()->Elevation (equpos.x, equpos.y);
	yofs = elev - base->Elevation();
	if (base->MapObjectsToSphere())
		yofs += ElevCorrection (relpos.x, relpos.z);
	equpos.z = base->RefPlanet()->Size() + relpos.y + elev;
	relpos.y += yofs;
}

D3DVALUE BaseObject::ElevCorrection (D3DVALUE px, D3DVALUE pz)
{
	double r = base->RefPlanet()->Size();
	double r2 = r*r;
	double d2 = px*px + pz*pz;
	return D3DVAL(r - sqrt (r2+d2));
}

void BaseObject::MapToCurvature (NTVERTEX *vtx, int nvtx)
{
	int i;
	double dst2, h;
	double rad = base->RefPlanet()->Size();
	double r2 = rad*rad;
	for (i = 0; i < nvtx; i++) {
		dst2 = vtx[i].x*vtx[i].x + vtx[i].z*vtx[i].z;
		h = sqrt (r2 + dst2) - rad;
		vtx[i].y -= (float)h;
	}
}

void BaseObject::MapToAltitude (NTVERTEX *vtx, int nvtx)
{
	double lng, lat;
	double elev0 = base->Elevation();
	for (int i = 0; i < nvtx; i++) {
		base->Rel_EquPos (Vector (vtx[i].x, vtx[i].y, vtx[i].z), lng, lat);
		vtx[i].y += (float)base->RefPlanet()->Elevation (lng, lat) - elev;
	}
}

void BaseObject::ParseError (const char *msg) const
{
	char errmsg[256];
	_snprintf (errmsg, 255, "Parse error from base definition file for %s: %s", base->Name(), msg);
	LOGOUT_ERR(errmsg);
}

// ==============================================================================
// class MeshObject (generic mesh)

MeshObject::MeshObject (const Base *_base): BaseObject (_base)
{
	specs = OBJSPEC_EXPORTVERTEX | OBJSPEC_RENDERSHADOW;
	texid = 0;
	fname = 0;
	ownmat = false;
	undersh = false;
	preload = false;
	rendershadow = false;
	mesh = 0;
}

MeshObject::~MeshObject ()
{
	if (fname) free (fname);
	if (mesh) delete mesh;
}

int MeshObject::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "FILE")) {
		fname = _strdup (value);
	} else if (!_stricmp (label, "WRAPTOSURFACE")) {
		specs |= OBJSPEC_WRAPTOSURFACE;
	} else if (!_stricmp (label, "SHADOW")) {
		specs |= OBJSPEC_RENDERSHADOW /*| OBJSPEC_EXPORTSHADOWMESH*/;
		// removed OBJSPEC_EXPORTSHADOWMESH to avoid accumulated shadow meshes with excessive
		// vertex numbers (vertex buffers are limited to 64000 vertices)
	} else if (!_stricmp (label, "OWNSHADOW")) {
		specs |= OBJSPEC_OWNSHADOW;
	} else if (!_stricmp (label, "UNDERSHADOWS")) {
		undersh = true;
	} else if (!_stricmp (label, "PRELOAD")) {
		preload = true;
	} else if (!_stricmp (label, "LPAD")) {
		specs |= OBJSPEC_LPAD;
	} else if (!_stricmp (label, "OWNMATERIAL")) {
		ownmat = true;
	} else if (!_stricmp (label, "TEX")) {
		texid = NameToId (value);
	}
	return res;
}

int MeshObject::Read (istream &is)
{
	if (fname) { free (fname); fname = 0; }
	specs = 0; //OBJSPEC_EXPORTVERTEX;  // default specs
	ownmat = undersh = preload = false;

	int res = BaseObject::Read (is);

	if (!fname) return 2; // we need a file name

	if (ownmat)
		//specs |= (undersh ? OBJSPEC_RENDERBEFORESHADOW : OBJSPEC_RENDERAFTERSHADOW);
		specs |= OBJSPEC_EXPORTMESH;
	else
		specs |= OBJSPEC_EXPORTVERTEX;
	if (undersh)
		specs |= OBJSPEC_UNDERSHADOW;
	return 0;
}

void MeshObject::Setup ()
{
	BaseObject::Setup ();
	if (preload) {
		LoadMesh (fname);
		free (fname);
		fname = 0;
	}
}

void MeshObject::Activate ()
{
	if (!preload && !mesh) {
		LoadMesh (fname);
		//if (dh) mesh->Translate(0,dh,0);
	}
}

void MeshObject::Deactivate ()
{
#ifdef UNDEF // don't delete meshes on destroying the visual
	if (!preload && mesh) {
		delete mesh;
		mesh = 0;
	}
#endif
}

bool MeshObject::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	GroupSpec *gs = mesh->GetGroup (grp);
	if (!gs) return false;
	nvtx = gs->nVtx;
	nidx = gs->nIdx;
	_texid = texid;        // global for now
	undershadow = undersh; // should be refinable for individual groups
	groundshadow = ((gs->UsrFlag & 0x1) == 0);
	return true;
}

void MeshObject::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	GroupSpec *gs = mesh->GetGroup (grp);
	if (gs) {
		WORD iofs = (WORD)idx_ofs;
		memcpy (vtx, gs->Vtx, gs->nVtx*sizeof(NTVERTEX));
		for (DWORD i = 0; i < gs->nIdx; i++)
			idx[i] = gs->Idx[i]+iofs;
	}
}

Mesh* MeshObject::ExportMesh ()
{
	if (specs & OBJSPEC_EXPORTMESH) return mesh;
	else                            return NULL;
}

Mesh *MeshObject::ExportShadowMesh (double &shelev)
{
	if (!(specs & OBJSPEC_EXPORTSHADOWMESH)) return NULL;
	DWORD i, j, nvtx = 0, nidx = 0, ngrp = mesh->nGroup();
	for (i = 0; i < ngrp; i++) {
		GroupSpec *grp = mesh->GetGroup(i);
		if (grp->UsrFlag & 0x1) continue; // no shadows
		nvtx += grp->nVtx;
		nidx += grp->nIdx;
	}
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *idx = new WORD[nidx]; TRACENEW
	nvtx = nidx = 0;
	for (i = 0; i < ngrp; i++) {
		GroupSpec *grp = mesh->GetGroup(i);
		if (grp->UsrFlag & 0x1) continue; // no shadows
		memcpy (vtx+nvtx, grp->Vtx, grp->nVtx*sizeof(NTVERTEX));
		for (j = 0; j < grp->nIdx; j++)
			idx[nidx+j] = grp->Idx[j]+(WORD)nvtx;
		nvtx += grp->nVtx;
		nidx += grp->nIdx;
	}

	shelev = yofs;
	if (nvtx) { TRACENEW; return new Mesh (vtx, nvtx, idx, nidx); }
	else return NULL;
}

bool MeshObject::LoadMesh (char *fname)
{
	int i;

	if (!mesh) { mesh = new Mesh; TRACENEW }
	if (!::LoadMesh (fname, *mesh)) {
		delete mesh; mesh = 0;
		ngrp = 0;
		return false;
	}
	ngrp = mesh->nGroup();
	if (scale.x != 1.0f || scale.y != 1.0f || scale.z != 1.0f)
		mesh->Scale (scale.x, scale.y, scale.z);
	if (rot)
		mesh->Rotate (Mesh::ROTATE_Y, rot);
	mesh->Translate (relpos.x, relpos.y, relpos.z);
	if (specs & OBJSPEC_WRAPTOSURFACE) {
		for (i = 0; i < ngrp; i++)
			MapToAltitude(mesh->GetGroup(i)->Vtx, mesh->GetGroup(i)->nVtx);
	}
	if (specs & OBJSPEC_RENDERSHADOW) {        // globally enable shadows
		for (i = 0; i < ngrp; i++) mesh->GetGroup(i)->UsrFlag &= ~0x1;
	} else if (!(specs & OBJSPEC_OWNSHADOW)) { // globally disable shadows
		for (i = 0; i < ngrp; i++) mesh->GetGroup(i)->UsrFlag |=  0x1;
	}
	return true;
}

void MeshObject::UpdateShadow (Vector &fromsun, double azim)
{
	ax = (float)fromsun.x;
	az = (float)fromsun.z;
}

void MeshObject::Render (LPDIRECT3DDEVICE7 dev, bool day)
{
}

void MeshObject::RenderShadow (LPDIRECT3DDEVICE7 dev)
{
}

// ==============================================================================
// class Block (simple buildings etc.)

Block::Block (const Base *_base): BaseObject (_base)
{
	ngrp = 3; // 3 mesh groups: x-walls, z-walls and roof
	for (int i = 0; i < 3; i++) {
		texid[i] = 0;
		tuscale[i] = tvscale[i] = 1.0f;
	}
	dyndata = 0;
}

Block::~Block ()
{
	Deactivate();
}

int Block::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_strnicmp (label, "TEX", 3)) {
		D3DVALUE su, sv;
		int i;
		char name[32];
		if (sscanf (label+3, "%d", &i) != 1 || i < 1 || i > 3) {
			ParseError("Block: TEXn: Expected integer value 1-3 for n");
			res = 2;
		}
		if (sscanf (value, "%s%f%f", name, &su, &sv) != 3) {
			ParseError("Block: TEXn: expected 3 values (*char, scalar, scalar)");
			res = 2;
		} 
		texid[i-1] = NameToId (name);
		tuscale[i-1] = su;
		tvscale[i-1] = sv;
	}
	return res;
}

void Block::Activate ()
{
	if (dyndata) return;  // active already
	dyndata = new struct DYNDATA; TRACENEW
	dyndata->databuf = new D3DVALUE[13]; TRACENEW
	D3DVALUE dx = 0.5f*scale.x, dy = scale.y, dz = 0.5f*scale.z;
	D3DVALUE srot = (D3DVALUE)sin(rot), crot = (D3DVALUE)cos(rot);
	D3DVALUE dxcrot = dx*crot, dxsrot = dx*srot;
	D3DVALUE dzsrot = dz*srot, dzcrot = dz*crot;
	dyndata->databuf[0] =  dxcrot + dzsrot + relpos.x;
	dyndata->databuf[1] = -dxcrot + dzsrot + relpos.x;
	dyndata->databuf[2] = -dxcrot - dzsrot + relpos.x;
	dyndata->databuf[3] =  dxcrot - dzsrot + relpos.x;
	dyndata->databuf[4] =  relpos.y;
	dyndata->databuf[5] =  relpos.y + dy;
	dyndata->databuf[6] =  dxsrot - dzcrot + relpos.z;
	dyndata->databuf[7] = -dxsrot - dzcrot + relpos.z;
	dyndata->databuf[8] = -dxsrot + dzcrot + relpos.z;
	dyndata->databuf[9] =  dxsrot + dzcrot + relpos.z;
	dyndata->databuf[10] = srot;
	dyndata->databuf[11] = crot;
	dyndata->databuf[12] = yofs;
}

void Block::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->databuf;
		dyndata->databuf = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

bool Block::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp < 0 || grp >= 3) return false;

	static DWORD nv[3] = {8,8,4};
	static DWORD ni[3] = {12,12,6};
	nvtx = nv[grp];
	nidx = ni[grp];
	undershadow = false;
	groundshadow = true;
	_texid = texid[grp];
	return true;
}

void Block::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	static WORD sidx[12] = {0,1,2,2,3,0,4,5,6,6,7,4};
	DWORD i;
	WORD iofs = (WORD)idx_ofs;
	D3DVALUE *db = dyndata->databuf;

	switch (grp) {
	case 0:
		//dyndata->gv0 = vtx;
		vtx[0].x = vtx[3].x = db[0];
		vtx[1].x = vtx[2].x = db[1];
		vtx[4].x = vtx[7].x = db[2];
		vtx[5].x = vtx[6].x = db[3];
		vtx[0].y = vtx[1].y = vtx[4].y = vtx[5].y = db[4];
		vtx[2].y = vtx[3].y = vtx[6].y = vtx[7].y = db[5];
		vtx[0].z = vtx[3].z = db[6];
		vtx[1].z = vtx[2].z = db[7];
		vtx[4].z = vtx[7].z = db[8];
		vtx[5].z = vtx[6].z = db[9];
		vtx[0].nx = vtx[1].nx = vtx[2].nx = vtx[3].nx =  db[10];
		vtx[4].nx = vtx[5].nx = vtx[6].nx = vtx[7].nx = -db[10];
		vtx[0].ny = vtx[1].ny = vtx[2].ny = vtx[3].ny =  0.0;
		vtx[4].ny = vtx[5].ny = vtx[6].ny = vtx[7].ny =  0.0;
		vtx[0].nz = vtx[1].nz = vtx[2].nz = vtx[3].nz = -db[11];
		vtx[4].nz = vtx[5].nz = vtx[6].nz = vtx[7].nz =  db[11];
		vtx[0].tu = vtx[3].tu = vtx[4].tu = vtx[7].tu =  tuscale[0];
		vtx[1].tu = vtx[2].tu = vtx[5].tu = vtx[6].tu =  0.0;
		vtx[0].tv = vtx[1].tv = vtx[4].tv = vtx[5].tv =  tvscale[0];
		vtx[2].tv = vtx[3].tv = vtx[6].tv = vtx[7].tv =  0.0;
		for (i = 0; i < 12; i++) *idx++ = sidx[i] + iofs;
		return;
	case 1:
		vtx[1].x = vtx[2].x = db[0];
		vtx[4].x = vtx[7].x = db[1];
		vtx[5].x = vtx[6].x = db[2];
		vtx[0].x = vtx[3].x = db[3];
		vtx[0].y = vtx[1].y = vtx[4].y = vtx[5].y = db[4];
		vtx[2].y = vtx[3].y = vtx[6].y = vtx[7].y = db[5];
		vtx[1].z = vtx[2].z = db[6];
		vtx[4].z = vtx[7].z = db[7];
		vtx[5].z = vtx[6].z = db[8];
		vtx[0].z = vtx[3].z = db[9];
		vtx[0].nx = vtx[1].nx = vtx[2].nx = vtx[3].nx =  db[11];
		vtx[4].nx = vtx[5].nx = vtx[6].nx = vtx[7].nx = -db[11];
		vtx[0].ny = vtx[1].ny = vtx[2].ny = vtx[3].ny =  0.0;
		vtx[4].ny = vtx[5].ny = vtx[6].ny = vtx[7].ny =  0.0;
		vtx[0].nz = vtx[1].nz = vtx[2].nz = vtx[3].nz =  db[10];
		vtx[4].nz = vtx[5].nz = vtx[6].nz = vtx[7].nz = -db[10];
		vtx[0].tu = vtx[3].tu = vtx[4].tu = vtx[7].tu =  tuscale[1];
		vtx[1].tu = vtx[2].tu = vtx[5].tu = vtx[6].tu =  0.0;
		vtx[0].tv = vtx[1].tv = vtx[4].tv = vtx[5].tv =  tvscale[1];
		vtx[2].tv = vtx[3].tv = vtx[6].tv = vtx[7].tv =  0.0;
		for (i = 0; i < 12; i++) *idx++ = sidx[i] + iofs;
		return;
	case 2:
		vtx[0].x = db[0];
		vtx[1].x = db[1];
		vtx[2].x = db[2];
		vtx[3].x = db[3];
		vtx[0].y = vtx[1].y = vtx[2].y = vtx[3].y = db[5];
		vtx[0].z = db[6];
		vtx[1].z = db[7];
		vtx[2].z = db[8];
		vtx[3].z = db[9];
		vtx[0].nx = vtx[1].nx = vtx[2].nx = vtx[3].nx =  0.0;
		vtx[0].ny = vtx[1].ny = vtx[2].ny = vtx[3].ny =  1.0;
		vtx[0].nz = vtx[1].nz = vtx[2].nz = vtx[3].nz =  0.0;
		vtx[0].tu = vtx[3].tu = tuscale[2];
		vtx[1].tu = vtx[2].tu = 0.0;
		vtx[0].tv = vtx[1].tv = tvscale[2];
		vtx[2].tv = vtx[3].tv = 0.0;
		for (i = 0; i < 6; i++) *idx++ = sidx[i] + iofs;
		return;
	}

}

Mesh *Block::ExportShadowMesh (double &shelev)
{
	static WORD sidx[30] = {0,1,4, 4,1,5, 1,2,5, 5,2,6, 2,3,6, 6,3,7, 3,0,7, 7,0,4, 4,5,6, 6,7,4};
	D3DVALUE *db = dyndata->databuf;
	DWORD i, nvtx = 8, nidx = 30;
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *idx = new WORD[nidx]; TRACENEW
	memcpy (idx, sidx, nidx*sizeof(WORD));
	memset (vtx, 0, nvtx*sizeof(NTVERTEX));
	for (i = 0; i < 8; i++)	{
		vtx[i].x = db[i%4];
		vtx[i].y = db[4+i/4];
		vtx[i].z = db[6+i%4];
	}
	shelev = yofs;
	TRACENEW; return new Mesh (vtx, nvtx, idx, nidx);
}

#ifdef UNDEF
bool Block::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 6;
	nidx = 12;
	return true;
}

void Block::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	static WORD sidx[12] = {0,1,2,2,1,3,2,3,4,4,3,5};
	DWORD i;
	for (i = 0; i < 12; i++) idx[i] = sidx[i];
	for (i = 0; i < 6; i++) vtx[i].y = dyndata->databuf[12];
	dyndata->shvtx = vtx;
}

void Block::UpdateShadow (Vector &fromsun, double az)
{
	D3DVALUE dx0 = pos.y*(D3DVALUE)fromsun.x;
	D3DVALUE dx1 = (pos.y+scale.y)*(D3DVALUE)fromsun.x;
	D3DVALUE dz0 = pos.y*(D3DVALUE)fromsun.z;
	D3DVALUE dz1 = (pos.y+scale.y)*(D3DVALUE)fromsun.z;
	D3DVERTEX *gv0 = dyndata->gv0;
	VERTEX_XYZ *vptr = dyndata->shvtx;
	az -= rot;
	if (az < -Pi) az += Pi2;
	else if (az > Pi) az -= Pi2;
	if (az >= 0) {
		if (az < Pi05) {
			vptr->x = gv0[1].x + dx0;  vptr->z = gv0[1].z + dz0;  vptr++;
			vptr->x = gv0[4].x + dx0;  vptr->z = gv0[4].z + dz0;  vptr++;
			vptr->x = gv0[0].x + dx0;  vptr->z = gv0[0].z + dz0;  vptr++;
			vptr->x = gv0[7].x + dx1;  vptr->z = gv0[7].z + dz1;  vptr++;
			vptr->x = gv0[3].x + dx1;  vptr->z = gv0[3].z + dz1;  vptr++;
			vptr->x = gv0[6].x + dx1;  vptr->z = gv0[6].z + dz1;
		} else {
			vptr->x = gv0[0].x + dx0;  vptr->z = gv0[0].z + dz0;  vptr++;
			vptr->x = gv0[1].x + dx0;  vptr->z = gv0[1].z + dz0;  vptr++;
			vptr->x = gv0[5].x + dx0;  vptr->z = gv0[5].z + dz0;  vptr++;
			vptr->x = gv0[2].x + dx1;  vptr->z = gv0[2].z + dz1;  vptr++;
			vptr->x = gv0[6].x + dx1;  vptr->z = gv0[6].z + dz1;  vptr++;
			vptr->x = gv0[7].x + dx1;  vptr->z = gv0[7].z + dz1;
		}
	} else {
		if (az > -Pi05) {
			vptr->x = gv0[4].x + dx0;  vptr->z = gv0[4].z + dz0;  vptr++;
			vptr->x = gv0[5].x + dx0;  vptr->z = gv0[5].z + dz0;  vptr++;
			vptr->x = gv0[1].x + dx0;  vptr->z = gv0[1].z + dz0;  vptr++;
			vptr->x = gv0[6].x + dx1;  vptr->z = gv0[6].z + dz1;  vptr++;
			vptr->x = gv0[2].x + dx1;  vptr->z = gv0[2].z + dz1;  vptr++;
			vptr->x = gv0[3].x + dx1;  vptr->z = gv0[3].z + dz1;
		} else {
			vptr->x = gv0[5].x + dx0;  vptr->z = gv0[5].z + dz0;  vptr++;
			vptr->x = gv0[0].x + dx0;  vptr->z = gv0[0].z + dz0;  vptr++;
			vptr->x = gv0[4].x + dx0;  vptr->z = gv0[4].z + dz0;  vptr++;
			vptr->x = gv0[3].x + dx1;  vptr->z = gv0[3].z + dz1;  vptr++;
			vptr->x = gv0[7].x + dx1;  vptr->z = gv0[7].z + dz1;  vptr++;
			vptr->x = gv0[2].x + dx1;  vptr->z = gv0[2].z + dz1;
		}
	}
}
#endif

// ==============================================================================
// class Hangar: a block with a barrel roof

Hangar::Hangar (const Base *_base): BaseObject (_base)
{
	ngrp = 3;
	for (int i = 0; i < 3; i++) {
		texid[i] = 0;
		tuscale[i] = tvscale[i] = 1.0f;
	}
	dyndata = 0;
}

Hangar::~Hangar ()
{
	Deactivate ();
}

int Hangar::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_strnicmp (label, "TEX", 3)) {
		D3DVALUE su, sv;
		int i;
		char name[32];
		if (sscanf (label+3, "%d", &i) != 1 || i < 1 || i > 3) {
			ParseError("Hangar: TEXn: Expected integer value 1-3 for n");
			res = 2;
		}
		if (sscanf (value, "%s%f%f", name, &su, &sv) != 3) {
			ParseError("Hangar: TEXn: expected 3 values (*char, scalar, scalar)");
			res = 2;
		}
		texid[i-1] = NameToId (name);
		tuscale[i-1] = su;
		tvscale[i-1] = sv;
	}
	return res;
}

void Hangar::Activate ()
{
	if (dyndata) return; // active already
	dyndata = new struct DYNDATA; TRACENEW
	dyndata->Vtx = new NTVERTEX[44]; TRACENEW
	NTVERTEX *Vtx = dyndata->Vtx;
	D3DVALUE dx = 0.5f*scale.x, dy = scale.y, dz = 0.5f*scale.z;
	D3DVALUE dy1 = 0.5f*dy; // side wall height
	D3DVALUE dy2 = dy-dy1;  // roof height
	D3DVALUE srot = (D3DVALUE)sin(rot), crot = (D3DVALUE)cos(rot);
	D3DVALUE dxcrot = dx*crot, dxsrot = dx*srot;
	D3DVALUE dzsrot = dz*srot, dzcrot = dz*crot;
	D3DVALUE dxcrot1 = 0.72f*dxcrot, dxcrot2 = 0.28f*dxcrot;
	D3DVALUE dxsrot1 = 0.72f*dxsrot, dxsrot2 = 0.28f*dxsrot;
	D3DVALUE tufac = tuscale[0]*dz/dx;

	Vtx[0].x  = Vtx[7].x  = Vtx[17].x = Vtx[18].x = Vtx[29].x =  dxcrot  + dzsrot + relpos.x;
	Vtx[1].x  = Vtx[2].x  = Vtx[20].x = Vtx[23].x = Vtx[39].x = -dxcrot  + dzsrot + relpos.x;
	Vtx[3].x  = Vtx[37].x = Vtx[25].x = Vtx[26].x = Vtx[41].x = Vtx[42].x = -dxcrot1 + dzsrot + relpos.x;
	Vtx[4].x  = Vtx[35].x =                                     -dxcrot2 + dzsrot + relpos.x;
	Vtx[5].x  = Vtx[33].x =                                      dxcrot2 + dzsrot + relpos.x;
	Vtx[6].x  = Vtx[31].x = Vtx[24].x = Vtx[27].x = Vtx[40].x = Vtx[43].x =  dxcrot1 + dzsrot + relpos.x;
	Vtx[8].x  = Vtx[15].x = Vtx[21].x = Vtx[22].x = Vtx[38].x = -dxcrot  - dzsrot + relpos.x;
	Vtx[9].x  = Vtx[10].x = Vtx[16].x = Vtx[19].x = Vtx[28].x =  dxcrot  - dzsrot + relpos.x;
	Vtx[11].x = Vtx[30].x =                                      dxcrot1 - dzsrot + relpos.x;
	Vtx[12].x = Vtx[32].x =                                      dxcrot2 - dzsrot + relpos.x;
	Vtx[13].x = Vtx[34].x =                                     -dxcrot2 - dzsrot + relpos.x;
	Vtx[14].x = Vtx[36].x =                                     -dxcrot1 - dzsrot + relpos.x;
	Vtx[0].z  = Vtx[7].z  = Vtx[17].z = Vtx[18].z = Vtx[29].z =  dxsrot  - dzcrot + relpos.z;
	Vtx[1].z  = Vtx[2].z  = Vtx[20].z = Vtx[23].z = Vtx[39].z = -dxsrot  - dzcrot + relpos.z;
	Vtx[3].z  = Vtx[37].z = Vtx[25].z = Vtx[26].z = Vtx[41].z = Vtx[42].z = -dxsrot1 - dzcrot + relpos.z;
	Vtx[4].z  = Vtx[35].z =                                     -dxsrot2 - dzcrot + relpos.z;
	Vtx[5].z  = Vtx[33].z =                                      dxsrot2 - dzcrot + relpos.z;
	Vtx[6].z  = Vtx[31].z = Vtx[24].z = Vtx[27].z = Vtx[40].z = Vtx[43].z =  dxsrot1 - dzcrot + relpos.z;
	Vtx[8].z  = Vtx[15].z = Vtx[21].z = Vtx[22].z = Vtx[38].z = -dxsrot  + dzcrot + relpos.z;
	Vtx[9].z  = Vtx[10].z = Vtx[16].z = Vtx[19].z = Vtx[28].z =  dxsrot  + dzcrot + relpos.z;
	Vtx[11].z = Vtx[30].z =                                      dxsrot1 + dzcrot + relpos.z;
	Vtx[12].z = Vtx[32].z =                                      dxsrot2 + dzcrot + relpos.z;
	Vtx[13].z = Vtx[34].z =                                     -dxsrot2 + dzcrot + relpos.z;
	Vtx[14].z = Vtx[36].z =                                     -dxsrot1 + dzcrot + relpos.z;
	Vtx[0].y = Vtx[1].y = Vtx[8].y = Vtx[9].y = Vtx[16].y = Vtx[17].y = Vtx[20].y = Vtx[21].y =
		Vtx[24].y = Vtx[25].y = Vtx[40].y = Vtx[41].y = relpos.y;
	Vtx[2].y = Vtx[7].y = Vtx[10].y = Vtx[15].y = Vtx[18].y = Vtx[19].y = Vtx[22].y = Vtx[23].y =
		Vtx[28].y = Vtx[29].y = Vtx[38].y = Vtx[39].y = Vtx[26].y = Vtx[27].y = Vtx[42].y = Vtx[43].y = dy1 + relpos.y;
	Vtx[3].y = Vtx[6].y = Vtx[11].y = Vtx[14].y = Vtx[30].y = Vtx[31].y = Vtx[36].y = Vtx[37].y = dy1 + 0.55f*dy2 + relpos.y;
	Vtx[4].y = Vtx[5].y = Vtx[12].y = Vtx[13].y = Vtx[32].y = Vtx[33].y = Vtx[34].y = Vtx[35].y = dy1 + 0.95f*dy2 + relpos.y;

	int i;
	for (i = 0; i < 8; i++) Vtx[i].nx =  srot, Vtx[i].ny = 0.0, Vtx[i].nz = -crot;
	for (; i < 16; i++)     Vtx[i].nx = -srot, Vtx[i].ny = 0.0, Vtx[i].nz =  crot;
	for (; i < 20; i++)     Vtx[i].nx =  crot, Vtx[i].ny = 0.0, Vtx[i].nz =  srot;
	for (; i < 24; i++)     Vtx[i].nx = -crot, Vtx[i].ny = 0.0, Vtx[i].nz = -srot;
	for (; i < 28; i++)     Vtx[i].nx =  srot, Vtx[i].ny = 0.0, Vtx[i].nz = -crot;
	for (i = 40; i < 44; i++) Vtx[i].nx =  srot, Vtx[i].ny = 0.0, Vtx[i].nz = -crot;
	Vtx[38].nx = Vtx[39].nx = -(Vtx[28].nx = Vtx[29].nx = 0.707f*crot);
	Vtx[38].ny = Vtx[39].ny = Vtx[28].ny = Vtx[29].ny = 0.707f;
	Vtx[38].nz = Vtx[39].nz = -(Vtx[28].nz = Vtx[29].nz = 0.707f*srot);
	Vtx[36].nx = Vtx[37].nx = -(Vtx[30].nx = Vtx[31].nx = 0.5f*crot);
	Vtx[36].ny = Vtx[37].ny = Vtx[30].ny = Vtx[31].ny = 0.82f;
	Vtx[36].nz = Vtx[37].nz = -(Vtx[30].nz = Vtx[31].nz = 0.5f*srot);
	Vtx[34].nx = Vtx[35].nx = -(Vtx[32].nx = Vtx[33].nx = 0.18f*crot);
	Vtx[34].ny = Vtx[35].ny = Vtx[32].ny = Vtx[33].ny = 0.96f;
	Vtx[34].nz = Vtx[35].nz = -(Vtx[32].nz = Vtx[33].nz = 0.18f*srot);

	Vtx[0].tu = Vtx[7].tu = Vtx[8].tu = Vtx[15].tu = tuscale[0];
	Vtx[1].tu = Vtx[2].tu = Vtx[9].tu = Vtx[10].tu = Vtx[17].tu = Vtx[18].tu = Vtx[21].tu = Vtx[22].tu = 0.0;
	Vtx[3].tu = Vtx[11].tu = Vtx[25].tu = Vtx[26].tu = 0.14f*tuscale[0];
	Vtx[4].tu = Vtx[12].tu = 0.36f*tuscale[0];
	Vtx[5].tu = Vtx[13].tu = 0.64f*tuscale[0];
	Vtx[6].tu = Vtx[14].tu = Vtx[24].tu = Vtx[27].tu = 0.86f*tuscale[0];
	Vtx[16].tu = Vtx[19].tu = Vtx[20].tu = Vtx[23].tu = tufac;
	Vtx[40].tu = Vtx[43].tu = tuscale[1];
	Vtx[41].tu = Vtx[42].tu = 0.0;
	Vtx[0].tv = Vtx[1].tv = Vtx[8].tv = Vtx[9].tv = Vtx[16].tv = Vtx[17].tv =
		Vtx[20].tv = Vtx[21].tv = Vtx[24].tv = Vtx[25].tv = 0.0;
	Vtx[2].tv = Vtx[7].tv = Vtx[10].tv = Vtx[15].tv = Vtx[18].tv = Vtx[19].tv =
		Vtx[22].tv = Vtx[23].tv = Vtx[26].tv = Vtx[27].tv = 0.5f*tvscale[0];
	Vtx[3].tv = Vtx[6].tv = Vtx[11].tv = Vtx[14].tv = 0.75f*tvscale[0];
	Vtx[4].tv = Vtx[5].tv = Vtx[12].tv = Vtx[13].tv = tvscale[0];
	Vtx[40].tv = Vtx[41].tv = 0.0;
	Vtx[42].tv = Vtx[43].tv = tvscale[1];
	for (i = 0; i < 6; i++) {
		Vtx[i*2+28].tu = Vtx[i*2+29].tu = i*tuscale[2]*0.2f;
		Vtx[i*2+28].tv = 0.0f;
		Vtx[i*2+29].tv = tvscale[2];
	}	
}

void Hangar::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

bool Hangar::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp < 0 || grp >= 3) return false;

	static DWORD nv[3] = {28,4,12};
	static DWORD ni[3] = {54,6,30};
	nvtx = nv[grp];
	nidx = ni[grp];
	_texid = texid[grp];
	undershadow = false;
	groundshadow = true;
	return true;
}

void Hangar::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	static WORD sidx0[54] = {0,24,7,27,7,24,25,1,26,2,26,1,6,7,2,2,3,6,5,6,3,3,4,5,
		                     8,9,10,10,15,8,15,10,11,11,14,15,14,11,12,12,13,14,
							 16,17,19,18,19,17,20,21,23,22,23,21};
	static WORD sidx1[6]  = {0,1,3,2,3,1};
	static WORD sidx2[30] = {0,1,2,3,2,1,2,3,4,5,4,3,4,5,6,7,6,5,6,7,8,9,8,7,8,9,10,11,10,9};
	DWORD i;
	WORD iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		memcpy (vtx, dyndata->Vtx, 28*sizeof(NTVERTEX));
		for (i = 0; i < 54; i++) *idx++ = sidx0[i] + iofs;
		return;
	case 1:
		memcpy (vtx, dyndata->Vtx+40, 4*sizeof(NTVERTEX));
		for (i = 0; i < 6; i++) *idx++ = sidx1[i] + iofs;
		return;
	case 2:
		memcpy (vtx, dyndata->Vtx+28, 12*sizeof(NTVERTEX));
		for (i = 0; i < 30; i++) *idx++ = sidx2[i] + iofs;
		return;
	}
}

Mesh *Hangar::ExportShadowMesh (double &shelev)
{
	static WORD sidx[78] = {
		0,1,2,3,2,1,2,3,4,5,4,3,4,5,6,7,6,5,6,7,8,9,8,7,8,9,10,11,10,9, // roof
		0,12,1, 12,13,1, 11,15,10, 15,14,10,            // side walls
		10,14,12, 0,10,12, 8,10,0, 2,8,0, 6,8,2, 4,6,2, // front wall
		1,13,15, 11,1,15, 3,1,11, 9,3,11, 5,3,9, 7,5,9  // back wall
	};
	DWORD nvtx = 16;
	DWORD nidx = 78;
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *idx = new WORD[nidx]; TRACENEW
	memcpy (vtx, dyndata->Vtx+28, 12*sizeof(NTVERTEX));
	vtx[12] = dyndata->Vtx[9];
	vtx[13] = dyndata->Vtx[0];
	vtx[14] = dyndata->Vtx[8];
	vtx[15] = dyndata->Vtx[1];
	memcpy (idx, sidx, nidx*sizeof(WORD));
	shelev = yofs;
	TRACENEW; return new Mesh (vtx, nvtx, idx, nidx);
}

#ifdef UNDEF
bool Hangar::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 10;
	nidx = 24;
	return true;
}

void Hangar::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	for (WORD i = 0; i < 8; i++) {
		*idx++ = 0;
		*idx++ = i+2;
		*idx++ = i+1;
	}
	for (i = 0; i < 10; i++) vtx[i].y = 0.0;
	dyndata->shvtx = vtx;
}

void Hangar::UpdateShadow (Vector &fromsun, double az)
{
	static VECTOR2D proj[16];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	DWORD i, nCH;
	WORD *CHidx;
	D3DVERTEX *Vtx = dyndata->Vtx;
	VERTEX_XYZ *vptr = dyndata->shvtx;

	// project bounding vertices onto y=0
	for (i = 0; i < 16; i++) {
		proj[i].x = Vtx[i].x + dx*Vtx[i].y;
		proj[i].y = Vtx[i].z + dz*Vtx[i].y;
	}
	Find2DConvexHull(16, proj, &nCH, &CHidx);
	// we assume that nCH is 10
	for (i = 0; i < 10; i++) {
		vptr[i].x = proj[CHidx[i]].x;
		vptr[i].z = proj[CHidx[i]].y;
	}
}
#endif

// ==============================================================================
// class Hangar2: a block with a tent-shaped roof

Hangar2::Hangar2 (const Base *_base): BaseObject (_base)
{
	ngrp = 3;
	roofh = -1.0f; // use default
	for (int i = 0; i < 3; i++) {
		texid[i] = 0;
		tuscale[i] = tvscale[i] = 1.0f;
	}
	dyndata = 0;
}

Hangar2::~Hangar2 ()
{
	Deactivate ();
}

int Hangar2::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "ROOFH")) {
		if (sscanf (value, "%f", &roofh) != 1) {
			ParseError("Hangar2: ROOFH: Expected scalar value");
			res = 2;
		}
	} else if (!_strnicmp (label, "TEX", 3)) {
		D3DVALUE su, sv;
		int i;
		char name[32];
		if (sscanf (label+3, "%d", &i) != 1 || i < 1 || i > 3) {
			ParseError("Hangar2: TEXn: Expected integer value 1-3 for n");
			res = 2;
		}
		if (sscanf (value, "%s%f%f", name, &su, &sv) != 3) {
			ParseError("Hangar2: TEXn: expected 3 values (*char, scalar, scalar)");
			res = 2;
		}
		texid[i-1] = NameToId (name);
		tuscale[i-1] = su;
		tvscale[i-1] = sv;
	}
	return res;
}

bool Hangar2::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp < 0 || grp >= 3) return false;

	static DWORD nv[3] = {10,8,8};
	static DWORD ni[3] = {18,12,12};
	nvtx = nv[grp];
	nidx = ni[grp];
	_texid = texid[grp];
	undershadow = false;
	groundshadow = true;
	return true;
}

void Hangar2::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	static WORD sidx0[18] = {0,1,2,2,4,0,4,2,3,6,5,9,9,7,6,7,9,8};
	static WORD sidx1[12] = {0,1,3,2,3,1,5,4,6,7,6,4};
	DWORD i;
	WORD iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		memcpy (vtx, dyndata->Vtx, 10*sizeof(NTVERTEX));
		for (i = 0; i < 18; i++) *idx++ = sidx0[i] + iofs;
		return;
	case 1:
		memcpy (vtx, dyndata->Vtx+10, 8*sizeof(NTVERTEX));
		for (i = 0; i < 12; i++) *idx++ = sidx1[i] + iofs;
		return;
	case 2:
		memcpy (vtx, dyndata->Vtx+18, 8*sizeof(NTVERTEX));
		for (i = 0; i < 12; i++) *idx++ = sidx1[i] + iofs;
		return;
	}
}

Mesh *Hangar2::ExportShadowMesh (double &shelev)
{
	static WORD sidx[42] = {
		0,1,2,2,4,0,4,2,3,6,5,9,9,7,6,7,9,8, // front and back walls
		0,4,5, 9,5,4, 1,6,7, 7,2,1,          // side walls
		3,9,4, 9,3,8, 3,2,8, 2,7,8           // roof
	};
	DWORD nvtx = 10;
	DWORD nidx = 42;
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *idx = new WORD[nidx]; TRACENEW
	memcpy (vtx, dyndata->Vtx, 10*sizeof(NTVERTEX));
	memcpy (idx, sidx, nidx*sizeof(WORD));
	shelev = yofs;
	TRACENEW; return new Mesh (vtx, nvtx, idx, nidx);
}

#ifdef UNDEF
bool Hangar2::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 7;
	nidx = 15;
	return true;
}

void Hangar2::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	for (WORD i = 0; i < 5; i++) {
		*idx++ = 0;
		*idx++ = i+2;
		*idx++ = i+1;
	}
	for (i = 0; i < 7; i++) vtx[i].y = 0.0;
	dyndata->shvtx = vtx;
}

void Hangar2::UpdateShadow (Vector &fromsun, double az)
{
	static VECTOR2D proj[10];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	DWORD i, nCH;
	WORD *CHidx;
	D3DVERTEX *Vtx = dyndata->Vtx;
	VERTEX_XYZ *vptr = dyndata->shvtx;

	// project bounding vertices onto y=0
	for (i = 0; i < 10; i++) {
		proj[i].x = Vtx[i].x + dx*Vtx[i].y;
		proj[i].y = Vtx[i].z + dz*Vtx[i].y;
	}
	Find2DConvexHull(10, proj, &nCH, &CHidx);
	// we assume that nCH is 7
	for (i = 0; i < 7; i++) {
		vptr[i].x = proj[CHidx[i]].x;
		vptr[i].z = proj[CHidx[i]].y;
	}
}
#endif

void Hangar2::Activate ()
{
    if (dyndata) return; // active already
    dyndata = new struct DYNDATA; TRACENEW
    dyndata->Vtx = new NTVERTEX[26]; TRACENEW
    NTVERTEX *Vtx = dyndata->Vtx;
    D3DVALUE dx = 0.5f*scale.x, dy = scale.y, dz = 0.5f*scale.z;
    D3DVALUE dy2 = (roofh < 0.0 || roofh > dy ? 0.5f*dy : roofh); // roof height
    D3DVALUE dy1 = dy - dy2; // side wall height
    D3DVALUE srot = (D3DVALUE)sin(rot), crot = (D3DVALUE)cos(rot);
    D3DVALUE dxcrot = dx*crot, dxsrot = dx*srot;
    D3DVALUE dzsrot = dz*srot, dzcrot = dz*crot;
    D3DVALUE tufac = tuscale[0]*dz/dx;

    Vtx[0].x = Vtx[4].x = Vtx[11].x = Vtx[12].x = Vtx[19].x =  dxcrot + dzsrot + relpos.x;
    Vtx[1].x = Vtx[2].x = Vtx[15].x = Vtx[16].x = Vtx[23].x = -dxcrot + dzsrot + relpos.x;
    Vtx[5].x = Vtx[9].x = Vtx[10].x = Vtx[13].x = Vtx[18].x =  dxcrot - dzsrot + relpos.x;
    Vtx[6].x = Vtx[7].x = Vtx[14].x = Vtx[17].x = Vtx[22].x = -dxcrot - dzsrot + relpos.x;
    Vtx[3].x = Vtx[20].x = Vtx[24].x =                         dzsrot + relpos.x;
    Vtx[8].x = Vtx[21].x = Vtx[25].x =                        -dzsrot + relpos.x;
    Vtx[0].z = Vtx[4].z = Vtx[11].z = Vtx[12].z = Vtx[19].z =  dxsrot - dzcrot + relpos.z;
    Vtx[1].z = Vtx[2].z = Vtx[15].z = Vtx[16].z = Vtx[23].z = -dxsrot - dzcrot + relpos.z;
    Vtx[5].z = Vtx[9].z = Vtx[10].z = Vtx[13].z = Vtx[18].z =  dxsrot + dzcrot + relpos.z;
    Vtx[6].z = Vtx[7].z = Vtx[14].z = Vtx[17].z = Vtx[22].z = -dxsrot + dzcrot + relpos.z;
    Vtx[3].z = Vtx[20].z = Vtx[24].z =                        -dzcrot + relpos.z;
    Vtx[8].z = Vtx[21].z = Vtx[25].z =                         dzcrot + relpos.z;
    Vtx[0].y = Vtx[1].y = Vtx[5].y = Vtx[6].y = Vtx[10].y = Vtx[11].y = Vtx[14].y = Vtx[15].y = relpos.y;
    Vtx[2].y = Vtx[4].y = Vtx[7].y = Vtx[9].y = Vtx[12].y = Vtx[13].y = Vtx[16].y = Vtx[17].y =
    	       Vtx[18].y = Vtx[19].y = Vtx[22].y = Vtx[23].y = dy1 + relpos.y;
    Vtx[3].y = Vtx[8].y = Vtx[20].y = Vtx[21].y = Vtx[24].y = Vtx[25].y = dy + relpos.y;

    int i;
    double alpha = atan2 (dy2, dx); // roof angle
    float ny = (float)cos(alpha), nxz = (float)sin(alpha);

    for (i = 0; i < 5; i++) Vtx[i].nx =  srot, Vtx[i].ny = 0.0, Vtx[i].nz = -crot;
    for (; i < 10; i++)     Vtx[i].nx = -srot, Vtx[i].ny = 0.0, Vtx[i].nz =  crot;
    for (; i < 14; i++)     Vtx[i].nx =  crot, Vtx[i].ny = 0.0, Vtx[i].nz =  srot;
    for (; i < 18; i++)     Vtx[i].nx = -crot, Vtx[i].ny = 0.0, Vtx[i].nz = -srot;
    for (; i < 22; i++)     Vtx[i].nx =  nxz*crot, Vtx[i].ny = ny, Vtx[i].nz =  nxz*srot;
    for (; i < 26; i++)     Vtx[i].nx = -nxz*crot, Vtx[i].ny = ny, Vtx[i].nz = -nxz*srot;

    Vtx[0].tu = Vtx[4].tu = Vtx[6].tu = Vtx[7].tu = 0.0;
    Vtx[1].tu = Vtx[2].tu = Vtx[5].tu = Vtx[9].tu = tuscale[0];
    Vtx[3].tu = Vtx[8].tu = 0.5f*tuscale[0];
    Vtx[10].tu = Vtx[13].tu = Vtx[15].tu = Vtx[16].tu = 0.0;
    Vtx[11].tu = Vtx[12].tu = Vtx[14].tu = Vtx[17].tu = tuscale[1];
    Vtx[18].tu = Vtx[21].tu = Vtx[22].tu = Vtx[25].tu = tuscale[2];
    Vtx[19].tu = Vtx[20].tu = Vtx[23].tu = Vtx[24].tu = 0.0;
    Vtx[0].tv = Vtx[1].tv = Vtx[5].tv = Vtx[6].tv = tvscale[0];
    Vtx[10].tv = Vtx[11].tv = Vtx[14].tv = Vtx[15].tv = tvscale[1];
    Vtx[2].tv = Vtx[4].tv = Vtx[7].tv = Vtx[9].tv =
		Vtx[12].tv = Vtx[13].tv = Vtx[16].tv = Vtx[17].tv = 0.0;
    Vtx[3].tv = Vtx[8].tv = -tvscale[0]*dy2/dy1;
    Vtx[20].tv = Vtx[21].tv = Vtx[24].tv = Vtx[25].tv = 0.0;
    Vtx[18].tv = Vtx[19].tv = Vtx[22].tv = Vtx[23].tv = tvscale[2];
}

void Hangar2::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

// ==============================================================================
// class Hangar3: hangar-type building with barrel roof reaching to the ground
// and recessed entry

Hangar3::Hangar3 (const Base *_base): BaseObject (_base)
{
	ngrp = 3;
	for (int i = 0; i < 3; i++) {
		texid[i] = 0;
		tuscale[i] = tvscale[i] = 1.0f;
	}
	dyndata = 0;
}

Hangar3::~Hangar3 ()
{
	Deactivate ();
}

int Hangar3::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_strnicmp (label, "TEX", 3)) {
		D3DVALUE su, sv;
		int i;
		char name[32];
		if (sscanf (label+3, "%d", &i) != 1 || i < 1 || i > 3) {
			ParseError("Hangar3: TEXn: Expected integer value 1-3 for n");
			res = 2;
		}
		if (sscanf (value, "%s%f%f", name, &su, &sv) != 3) {
			ParseError("Hangar3: TEXn: expected 3 values (*char, scalar, scalar)");
			res = 2;
		}
		texid[i-1] = NameToId (name);
		tuscale[i-1] = su;
		tvscale[i-1] = sv;
	}
	return res;
}

bool Hangar3::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp < 0 || grp >= 3) return false;

	static DWORD nv[3] = {18,8,14};
	static DWORD ni[3] = {36,24,36};
	nvtx = nv[grp];
	nidx = ni[grp];
	_texid = texid[grp];
	undershadow = false;
	groundshadow = true;
	return true;
}

void Hangar3::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	static WORD sidx0[60] = {0,6,1,1,6,5,1,5,2,2,5,4,2,4,3, // back wall
	          				 7,8,14,8,9,14,9,10,16,16,10,17,10,11,17,11,12,15,12,13,15}; // front wall
	static WORD sidx1[24] = {0,1,5,5,4,0,6,7,4,4,5,6,2,3,7,7,6,2,1,2,6,6,5,1}; // entry
	static WORD sidx2[36] = {0,1,7,8,7,1,1,2,8,9,8,2,2,3,9,10,9,3,3,4,10,4,11,10,4,5,11,12,11,5,5,6,12,13,12,6}; // roof
	DWORD i;
	WORD iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		memcpy (vtx, dyndata->Vtx, 18*sizeof(NTVERTEX));
		for (i = 0; i < 60; i++) *idx++ = sidx0[i] + iofs;
		return;
	case 1:
		memcpy (vtx, dyndata->Vtx+18, 8*sizeof(NTVERTEX));
		for (i = 0; i < 24; i++) *idx++ = sidx1[i] + iofs;
		return;
	case 2:
		memcpy (vtx, dyndata->Vtx+26, 14*sizeof(NTVERTEX));
		for (i = 0; i < 36; i++) *idx++ = sidx2[i] + iofs;
		return;
	}
}

Mesh *Hangar3::ExportShadowMesh (double &shelev)
{
	static WORD sidx[66] = {
		0,1,7,8,7,1,1,2,8,9,8,2,2,3,9,10,9,3,3,4,10,4,11,10,4,5,11,12,11,5,5,6,12,13,12,6,
		0,6,1, 1,6,5, 1,5,2, 2,5,4, 3,2,4, // front wall
		7,8,13, 8,12,13, 9,12,8, 9,11,12, 9,10,11 // back wall
	}; // roof
	DWORD nvtx = 14;
	DWORD nidx = 66;
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *idx = new WORD[nidx]; TRACENEW
	memcpy (vtx, dyndata->Vtx+26, nvtx*sizeof(NTVERTEX));
	memcpy (idx, sidx, nidx*sizeof(WORD));
	shelev = yofs;
	TRACENEW; return new Mesh (vtx, nvtx, idx, nidx);
}

#ifdef UNDEF
bool Hangar3::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 9;
	nidx = 21;
	return true;
}

void Hangar3::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	for (WORD i = 0; i < 7; i++) {
		*idx++ = 0;
		*idx++ = i+2;
		*idx++ = i+1;
	}
	for (i = 0; i < 9; i++) vtx[i].y = 0.0;
	dyndata->shvtx = vtx;
}

void Hangar3::UpdateShadow (Vector &fromsun, double az)
{
	static VECTOR2D proj[14];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	DWORD i, nCH;
	WORD *CHidx;
	D3DVERTEX *Vtx = dyndata->Vtx;
	VERTEX_XYZ *vptr = dyndata->shvtx;

	// project bounding vertices onto y=0
	for (i = 0; i < 14; i++) {
		proj[i].x = Vtx[i].x + dx*Vtx[i].y;
		proj[i].y = Vtx[i].z + dz*Vtx[i].y;
	}
	Find2DConvexHull(14, proj, &nCH, &CHidx);
	// we assume that nCH is 9
	for (i = 0; i < 9; i++) {
		vptr[i].x = proj[CHidx[i]].x;
		vptr[i].z = proj[CHidx[i]].y;
	}
}
#endif

void Hangar3::Activate ()
{
    static D3DVALUE recess = 2.0f; // should be configurable
    if (dyndata) return; // active already
    dyndata = new struct DYNDATA; TRACENEW
    dyndata->Vtx = new NTVERTEX[40]; TRACENEW
    NTVERTEX *Vtx = dyndata->Vtx;
    D3DVALUE dx = 0.5f*scale.x, dy = scale.y, dz = 0.5f*scale.z;
    D3DVALUE h1 = 0.543f*dy, h2 = 0.884f*dy, h3 = dy;
    D3DVALUE srot = (D3DVALUE)sin(rot), crot = (D3DVALUE)cos(rot);
    D3DVALUE dxcrot = dx*crot, dxsrot = dx*srot;
    D3DVALUE dzsrot = dz*srot, dzcrot = dz*crot;
    D3DVALUE dxcrot1 = 0.707f*dxcrot, dxcrot2 = 0.366f*dxcrot;
    D3DVALUE dxsrot1 = 0.707f*dxsrot, dxsrot2 = 0.366f*dxsrot;
    D3DVALUE dzcrot1 = (dz-recess)*crot, dzsrot1 = (dz-recess)*srot;
    Vtx[ 0].x = Vtx[26].x =  dxcrot  + dzsrot + relpos.x;
    Vtx[ 1].x = Vtx[27].x =  dxcrot1 + dzsrot + relpos.x;
    Vtx[ 2].x = Vtx[28].x =  dxcrot2 + dzsrot + relpos.x;
    Vtx[ 3].x = Vtx[29].x =            dzsrot + relpos.x;
    Vtx[ 4].x = Vtx[30].x = -dxcrot2 + dzsrot + relpos.x;
    Vtx[ 5].x = Vtx[31].x = -dxcrot1 + dzsrot + relpos.x;
    Vtx[ 6].x = Vtx[32].x = -dxcrot  + dzsrot + relpos.x;
    Vtx[ 7].x = Vtx[33].x =  dxcrot  - dzsrot + relpos.x;
    Vtx[ 8].x = Vtx[34].x =  dxcrot1 - dzsrot + relpos.x;
    Vtx[ 9].x = Vtx[35].x = Vtx[14].x = Vtx[16].x = Vtx[18].x = Vtx[19].x =  dxcrot2 - dzsrot + relpos.x;
    Vtx[10].x = Vtx[36].x =          - dzsrot + relpos.x;
    Vtx[11].x = Vtx[37].x = Vtx[15].x = Vtx[17].x = Vtx[20].x = Vtx[21].x = -dxcrot2 - dzsrot + relpos.x;
    Vtx[12].x = Vtx[38].x = -dxcrot1 - dzsrot + relpos.x;
    Vtx[13].x = Vtx[39].x = -dxcrot  - dzsrot + relpos.x;
    Vtx[22].x = Vtx[23].x =  dxcrot2 - dzsrot1 + relpos.x;
    Vtx[24].x = Vtx[25].x = -dxcrot2 - dzsrot1 + relpos.x;
    Vtx[ 0].z = Vtx[26].z =  dxsrot  - dzcrot  + relpos.z;
    Vtx[ 1].z = Vtx[27].z =  dxsrot1 - dzcrot  + relpos.z;
    Vtx[ 2].z = Vtx[28].z =  dxsrot2 - dzcrot  + relpos.z;
    Vtx[ 3].z = Vtx[29].z =          - dzcrot  + relpos.z;
    Vtx[ 4].z = Vtx[30].z = -dxsrot2 - dzcrot  + relpos.z;
    Vtx[ 5].z = Vtx[31].z = -dxsrot1 - dzcrot  + relpos.z;
    Vtx[ 6].z = Vtx[32].z = -dxsrot  - dzcrot  + relpos.z;
    Vtx[ 7].z = Vtx[33].z =  dxsrot  + dzcrot  + relpos.z;
    Vtx[ 8].z = Vtx[34].z =  dxsrot1 + dzcrot  + relpos.z;
    Vtx[ 9].z = Vtx[35].z = Vtx[14].z = Vtx[16].z = Vtx[18].z = Vtx[19].z =  dxsrot2 + dzcrot  + relpos.z;
    Vtx[10].z = Vtx[36].z =            dzcrot  + relpos.z;
    Vtx[11].z = Vtx[37].z = Vtx[15].z = Vtx[17].z = Vtx[20].z = Vtx[21].z = -dxsrot2 + dzcrot  + relpos.z;
    Vtx[12].z = Vtx[38].z = -dxsrot1 + dzcrot  + relpos.z;
    Vtx[13].z = Vtx[39].z = -dxsrot  + dzcrot  + relpos.z;
    Vtx[22].z = Vtx[23].z =  dxsrot2 + dzcrot1 + relpos.z;
    Vtx[24].z = Vtx[25].z = -dxsrot2 + dzcrot1 + relpos.z;
    Vtx[ 0].y = Vtx[ 6].y = Vtx[ 7].y = Vtx[13].y = Vtx[26].y = Vtx[32].y = Vtx[33].y = Vtx[39].y
	      = Vtx[14].y = Vtx[15].y = Vtx[18].y = Vtx[21].y = Vtx[22].y = Vtx[25].y = relpos.y;
    Vtx[ 1].y = Vtx[ 5].y = Vtx[ 8].y = Vtx[12].y = Vtx[27].y = Vtx[31].y = Vtx[34].y = Vtx[38].y
	      = Vtx[16].y = Vtx[17].y = Vtx[19].y = Vtx[20].y = Vtx[23].y = Vtx[24].y = h1 + relpos.y;
    Vtx[ 2].y = Vtx[ 4].y = Vtx[ 9].y = Vtx[11].y = Vtx[28].y = Vtx[30].y = Vtx[35].y = Vtx[37].y = h2 + relpos.y;
    Vtx[ 3].y = Vtx[10].y = Vtx[29].y = Vtx[36].y = h3 + relpos.y;

    int i;
    for (i = 0; i < 7; i++) Vtx[i].nx =  srot, Vtx[i].ny = 0.0, Vtx[i].nz = -crot;
    for (; i < 18; i++)     Vtx[i].nx = -srot, Vtx[i].ny = 0.0, Vtx[i].nz =  crot;
    Vtx[32].nx = Vtx[39].nx = -(Vtx[26].nx = Vtx[33].nx = 0.707f*crot);
    Vtx[32].ny = Vtx[39].ny =   Vtx[26].ny = Vtx[33].ny = 0.707f;
    Vtx[32].nz = Vtx[39].nz = -(Vtx[26].nz = Vtx[33].nz = 0.707f*srot);
    Vtx[31].nx = Vtx[38].nx = -(Vtx[27].nx = Vtx[34].nx = 0.5f*crot);
    Vtx[31].ny = Vtx[38].ny =   Vtx[27].ny = Vtx[34].ny = 0.866f;
    Vtx[31].nz = Vtx[38].nz = -(Vtx[27].nz = Vtx[34].nz = 0.5f*srot);
    Vtx[30].nx = Vtx[37].nx = -(Vtx[28].nx = Vtx[35].nx = 0.259f*crot);
    Vtx[30].ny = Vtx[37].ny =   Vtx[28].ny = Vtx[35].ny = 0.966f;
    Vtx[30].nz = Vtx[37].nz = -(Vtx[28].nz = Vtx[35].nz = 0.259f*srot);
    Vtx[29].nx = Vtx[36].nx = Vtx[29].nz = Vtx[36].nz = 0.0;
    Vtx[29].ny = Vtx[36].ny = 1.0;
    Vtx[20].nx = Vtx[21].nx = -(Vtx[18].nx = Vtx[19].nx = crot);
    Vtx[20].ny = Vtx[21].ny =   Vtx[18].ny = Vtx[19].ny = 0.0;
    Vtx[20].nz = Vtx[21].nz = -(Vtx[18].nz = Vtx[19].nz = srot);
    Vtx[22].nx = Vtx[23].nx = Vtx[24].nx = Vtx[25].nx = -srot;
    Vtx[22].ny = Vtx[23].ny = Vtx[24].ny = Vtx[25].ny =  0.0;
    Vtx[22].nz = Vtx[23].nz = Vtx[24].nz = Vtx[25].nz =  crot;
	
    // texture coordinates for barrel roof (vtx 26-39)
    for (i = 0; i < 7; i++) {
	Vtx[26+i].tu = 0.0;
	Vtx[33+i].tu = tuscale[2];
    }
    Vtx[29].tv = Vtx[36].tv = 0.0;
    for (i = 1; i < 4; i++) {
	Vtx[29+i].tv = Vtx[29-i].tv = Vtx[36+i].tv = Vtx[36-i].tv = tvscale[2]*i*0.3333f;
    }
    // texture coordinates for front/back and door - still need to be done
    for (i = 0; i < 26; i++) {
	Vtx[i].tu = Vtx[i].tv = 0.0;
    }
}

void Hangar3::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

// ==============================================================================
// class Tank: a vertical cylinder without a base face

Tank::Tank (const Base *_base): BaseObject (_base)
{
	ngrp = 2; // 2 mesh groups: mantle and top
	for (int i = 0; i < 2; i++) {
		texid[i] = 0;
		tuscale[i] = tvscale[i] = 1.0f;
	}
	nstep = 12;
	dyndata = 0;
}

int Tank::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "NSTEP")) {
		if (sscanf (value, "%d", &nstep) != 1) {
			ParseError("Tank: NSTEP: Expected integer value");
			res = 2;
		}
		if (nstep < 3) nstep = 3;
	} else if (!_strnicmp (label, "TEX", 3)) {
		D3DVALUE su, sv;
		int i;
		char name[32];
		if (sscanf (label+3, "%d", &i) != 1 || i < 1 || i > 2) {
			ParseError("Tank: TEXn: Expected integer value 1-2 for n");
			res = 2;
		}
		if (sscanf (value, "%s%f%f", name, &su, &sv) != 3) {
			ParseError("Tank: TEXn: expected 3 values (*char, scalar, scalar)");
			res = 2;
		}
		texid[i-1] = NameToId (name);
		tuscale[i-1] = su;
		tvscale[i-1] = sv;
	}
	return res;
}

void Tank::Activate ()
{
	if (dyndata) return; // active already
	dyndata = new struct DYNDATA; TRACENEW
	dyndata->Vtx = new NTVERTEX[nstep*3+3]; TRACENEW
	NTVERTEX *Vtx = dyndata->Vtx;
	D3DVALUE dx, dz, dnx, dnz, fac, ifac = 1.0f/(D3DVALUE)nstep;
	D3DVALUE srot = (D3DVALUE)sin(rot), crot = (D3DVALUE)cos(rot);
	DWORD i, ofs1 = nstep+1, ofs2 = 2*nstep+2;
	double alpha;

	for (i = 0; i < nstep; i++) {
		fac = (D3DVALUE)i*ifac;
		alpha = Pi2*fac;
		dx = (dnx = (D3DVALUE)cos(alpha)) * scale.x;
		dz = (dnz = (D3DVALUE)sin(alpha)) * scale.z;
		Vtx[i].x = Vtx[ofs1+i].x = Vtx[ofs2+i].x = crot*dx - srot*dz + relpos.x;
		Vtx[i].z = Vtx[ofs1+i].z = Vtx[ofs2+i].z = srot*dx + crot*dz + relpos.z;
		Vtx[i].y = relpos.y;
		Vtx[ofs1+i].y = Vtx[ofs2+i].y = scale.y + relpos.y;
		Vtx[i].nx = Vtx[ofs1+i].nx = crot*dnx - srot*dnz;
		Vtx[i].ny = Vtx[ofs1+i].ny = 0.0f;
		Vtx[i].nz = Vtx[ofs1+i].nz = srot*dnx + crot*dnz;
		Vtx[ofs2+i].nx = Vtx[ofs2+i].nz = 0.0f;
		Vtx[ofs2+i].ny = 1.0f;
		Vtx[i].tu = Vtx[ofs1+i].tu = fac*tuscale[0];
		Vtx[i].tv = 0.0f;
		Vtx[ofs1+i].tv = tvscale[0];
		Vtx[ofs2+i].tu = (0.5f+dnx)*tuscale[1];
		Vtx[ofs2+i].tv = (0.5f+dnz)*tvscale[1];
	}
	Vtx[nstep].x = Vtx[ofs2-1].x = Vtx[0].x;
	Vtx[nstep].z = Vtx[ofs2-1].z = Vtx[0].z;
	Vtx[nstep].y = relpos.y; Vtx[ofs2-1].y = scale.y + relpos.y;
	Vtx[nstep].nx = Vtx[ofs2-1].nx = Vtx[0].nx;
	Vtx[nstep].ny = Vtx[ofs2-1].ny = 0.0f;
	Vtx[nstep].nz = Vtx[ofs2-1].nz = Vtx[0].nz;
	Vtx[nstep].tu = Vtx[ofs2-1].tu = tuscale[0];
	Vtx[nstep].tv = 0.0f; Vtx[ofs2-1].tv = tvscale[0];
}

void Tank::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

bool Tank::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	switch (grp) {
	case 0:
		nvtx = (nstep+1)*2;
		nidx = nstep*6;
		_texid = texid[0];
		undershadow = false;
		groundshadow = true;
		return true;
	case 1:
		nvtx = nstep;
		nidx = (nstep-2)*3;
		_texid = texid[1];
		undershadow = false;
		groundshadow = true;
		return true;
	default:
		return false;
	}
}

void Tank::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	WORD i, ofs = (WORD)(nstep+1), iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		memcpy (vtx, dyndata->Vtx, (nstep+1)*2*sizeof(NTVERTEX));
		for (i = 0; i < nstep; i++) {
			*idx++ = iofs + i;
			*idx++ = iofs + ofs+i;
			*idx++ = iofs + i+1;
			*idx++ = iofs + ofs+1+i;
			*idx++ = iofs + i+1;
			*idx++ = iofs + ofs+i;
		}
		return;
	case 1:
		memcpy (vtx, dyndata->Vtx+((nstep+1)*2), nstep*sizeof(NTVERTEX));
		for (i = 1; i < nstep-1; i++) {
			*idx++ = iofs;
			*idx++ = iofs + i+1;
			*idx++ = iofs + i;
		}
		return;
	}
}

Mesh *Tank::ExportShadowMesh (double &shelev)
{
	DWORD nvtx = nstep*2;
	DWORD nidx = nstep*6 + (nstep-2)*3;
	NTVERTEX *vtx = new NTVERTEX[nvtx]; TRACENEW
	WORD *id, *idx = new WORD[nidx]; TRACENEW
	memcpy (vtx, dyndata->Vtx, nstep*sizeof(NTVERTEX));
	memcpy (vtx+nstep, dyndata->Vtx+nstep+1, nstep*sizeof(NTVERTEX));

	WORD i, ofs = (WORD)nstep, ns = (WORD)nstep;
	for (i = 0, id = idx; i < ns; i++) {
		*id++ = i;
		*id++ = ofs+i;
		*id++ = (i+1)%ns;
		*id++ = ofs+(1+i)%ns;
		*id++ = (i+1)%ns;
		*id++ = ofs+i;
	}
	for (i = 1; i < ns-1; i++) {
		*id++ = ofs;
		*id++ = ofs+(i+1);
		*id++ = ofs+i;
	}
	shelev = yofs;
	TRACENEW; return new Mesh (vtx, nvtx, idx, nidx);
}

#ifdef UNDEF
bool Tank::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = nstep+2;
	nidx = nstep*3;
	return true;
}

void Tank::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	for (WORD i = 0; i < nstep; i++) {
		*idx++ = 0;
		*idx++ = i+2;
		*idx++ = i+1;
	}
	for (i = 0; i < nstep+2; i++) vtx[i].y = dyndata->dh;
	dyndata->shvtx = vtx;
}

void Tank::UpdateShadow (Vector &fromsun, double az)
{
	static DWORD nproj = 24;
	static VECTOR2D *proj = new VECTOR2D[24];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	DWORD i, nCH;
	WORD *CHidx;
	D3DVERTEX *Vtx = dyndata->Vtx;
	VERTEX_XYZ *vptr = dyndata->shvtx;

	if (nstep*2 > nproj) {
		delete []proj;
		proj = new VECTOR2D[nproj = nstep*2]; TRACENEW
	}
	// project bounding vertices onto y=0
	for (i = 0; i < nstep; i++) {
		proj[i+nstep].x = (proj[i].x = Vtx[i+1].x) + dx*scale.y;
		proj[i+nstep].y = (proj[i].y = Vtx[i+1].z) + dz*scale.y;
	}
	Find2DConvexHull(nstep*2, proj, &nCH, &CHidx);
	// we assume that nCH is nstep+2
	for (i = 0; i < nstep+2; i++) {
		vptr[i].x = proj[CHidx[i]].x;
		vptr[i].z = proj[CHidx[i]].y;
	}
}
#endif

// ======================================================================================
// class Lpad: base class for landing pads

Lpad::Lpad (const Base *_base): BaseObject (_base)
{
	padno = 0; // default
	ILSfreq = 0.0f; // undefined
}

// ======================================================================================
// class Lpad01: octagonal landing pad

NTVERTEX Lpad01::Vtx[44] = {
	{ 7.5f,0,  0,    0,1,0, 0.59375f,0.5},
	{ 7.5f,0, 10,    0,1,0, 0.59375f,0.375f},
	{-7.5f,0, 10,    0,1,0, 0.40625f,0.375f},
	{-7.5f,0,  0,    0,1,0, 0.40625f,0.5},
	{-7.5f,0,-10,    0,1,0, 0.40625f,0.375f},
	{ 7.5f,0,-10,    0,1,0, 0.59375f,0.375f},
	{  40,0,  0,     0,1,0, 1,0.5},
	{  40,0, 16.57f, 0,1,0, 1,0.29289f},
	{  16.57f, 0,40, 0,1,0, 0.70711f,0},
	{ -16.57f, 0,40, 0,1,0, 0.29289f,0},
	{ -40,0, 16.57f, 0,1,0, 0,0.29289f},
	{ -40,0,  0,     0,1,0, 0,0.5},
	{ -40,0,-16.57f, 0,1,0, 0,0.29289f},
	{ -16.57f,0,-40, 0,1,0, 0.29289f,0},
	{  16.57f,0,-40, 0,1,0, 0.70711f,0},
	{  40,0,-16.57f, 0,1,0, 1,0.29289f},

	{ 7.5f,0,-10, 0,1,0, 0,0},
	{ 7.5f,0, 10, 0,1,0, 0,0},
	{-7.5f,0, 10, 0,1,0, 0,0},
	{-7.5f,0,-10, 0,1,0, 0,0},

	{16.57f,0,-40,      -0.270598f,0.707107f,0.653282f,  0.9375f,1},
	{40,0,-16.57f,      -0.653282f,0.707107f,0.270598f,  0.9375,0.5},
	{40,0,16.57f,       -0.653282f,0.707107f,-0.270598f, 0.9375,1},
	{16.57f,0,40,       -0.270598f,0.707107f,-0.653282f, 0.9375,0.5},
	{-16.57f,0,40,       0.270598f,0.707107f,-0.653282f, 0.9375,1},
	{-40,0,16.57f,       0.653282f,0.707107f,-0.270598f, 0.9375,0.5},
	{-40,0,-16.57f,      0.653282f,0.707107f,0.270598f,  0.9375,1},
	{-16.57f,0,-40,      0.270598f,0.707107f,0.653282f,  0.9375,0.5},
	{16.951f,1,-40.924f,  0,1,0,                       1,1},
	{40.924f,1,-16.951f,  0,1,0,                       1,0.5},
	{40.924f,1,16.951f,   0,1,0,                       1,1},
	{16.951f,1,40.924f,   0,1,0,                       1,0.5},
	{-16.951f,1,40.924f,  0,1,0,                       1,1},
	{-40.924f,1,16.951f,  0,1,0,                       1,0.5},
	{-40.924f,1,-16.951f, 0,1,0,                       1,1},
	{-16.951f,1,-40.924f, 0,1,0,                       1,0.5},
	{17.334f,0,-41.848f,  0.270598f,0.707107f,-0.653282f, 0.9375,1},
	{41.848f,0,-17.334f,  0.653282f,0.707107f,-0.270598f, 0.9375,0.5},
	{41.848f,0,17.334f,   0.653282f,0.707107f,0.270598f,  0.9375,1},
	{17.334f,0,41.848f,   0.270598f,0.707107f,0.653282f,  0.9375,0.5},
	{-17.334f,0,41.848f, -0.270598f,0.707107f,0.653282f,  0.9375,1},
	{-41.848f,0,17.334f, -0.653282f,0.707107f,0.270598f,  0.9375,0.5},
	{-41.848f,0,-17.334f,-0.653282f,0.707107f,-0.270598f, 0.9375,1},
	{-17.334f,0,-41.848f,-0.270598f,0.707107f,-0.653282f, 0.9375,0.5}
};

WORD Lpad01::Idx[144] = {
	0,1,6,1,7,6,1,8,7,1,9,8,1,2,9,2,10,9,2,11,10,2,3,11,
	3,4,11,4,12,11,4,13,12,4,14,13,4,5,14,5,15,14,5,6,15,5,0,6,
	16,19,17,18,17,19,
	20,21,28,29,28,21,28,29,36,37,36,29,
	21,22,29,30,29,22,29,30,37,38,37,30,
	22,23,30,31,30,23,30,31,38,39,38,31,
	23,24,31,32,31,24,31,32,39,40,39,32,
	24,25,32,33,32,25,32,33,40,41,40,33,
	25,26,33,34,33,26,33,34,41,42,41,34,
	26,27,34,35,34,27,34,35,42,43,42,35,
	20,28,36,27,43,35
};

Lpad01::Lpad01 (const Base *_base): Lpad (_base)
{
	ngrp = 1;
	texid = 0;
}

int Lpad01::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "TEX")) {
		texid = NameToId (value);
	} else if (!_stricmp (label, "NAV")) {
		if (sscanf (value, "%f", &ILSfreq) != 1) {
			ParseError ("Lpad1: NAV: expected scalar value");
			res = 2;
		}
	}
	return res;
}

bool Lpad01::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp == 0) {
		nvtx = 44;
		nidx = 144;
		_texid = texid;
		undershadow = true;
		groundshadow = false;
		return true;
	} else return false;
}

void Lpad01::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	if (grp == 0) {
		D3DVALUE cosr = scale.x*(D3DVALUE)cos(rot), sinr = scale.x*(D3DVALUE)sin(rot);
		DWORD i;
		WORD iofs = (WORD)idx_ofs;
		NTVERTEX *src = Vtx;
		memcpy (vtx, src, 44*sizeof(NTVERTEX));
		vtx[16].tu = vtx[17].tu = (vtx[18].tu = vtx[19].tu = ((padno+1)%5)*0.1875f) + 0.1875f;
		vtx[16].tv = vtx[19].tv = (vtx[17].tv = vtx[18].tv = ((padno+1)/5)*0.25f + 0.5f) + 0.25f;
		NTVERTEX *vtx0 = vtx;
		for (i = 0; i < 44; i++, vtx++, src++) {
			vtx->x = cosr*src->x - sinr*src->z + relpos.x;
			vtx->y = scale.x*src->y + relpos.y;
			vtx->z = sinr*src->x + cosr*src->z + relpos.z;
		}
		MapToAltitude (vtx0, 44);
		for (i = 0; i < 144; i++) *idx++ = Idx[i] + iofs;
	}
}

// ======================================================================================
// class Lpad02: square landing pad

NTVERTEX Lpad02::Vtx[28] = {
	{-15,0,  0,    0,1,0,   0.3125f,0.4965f },
	{-40,0,  0,    0,1,0,   0,      0.4965f },
	{-15,0, 17.5,  0,1,0,   0.3125f,0.28125f},
	{-40,0, 40,    0,1,0,   0,      0       },
	{ 15,0, 17.5,  0,1,0,   0.6875f,0.28125f},
	{ 40,0, 40,    0,1,0,   1,      0       },
	{ 15,0,  0,    0,1,0,   0.6875f,0.4965f },
	{ 40,0,  0,    0,1,0,   1,      0.4965f },
	{ 15,0,-17.5,  0,1,0,   0.6875f,0.28125f},
	{ 40,0,-40,    0,1,0,   1,      0       },
	{-15,0,-17.5,  0,1,0,   0.3125f,0.28125f},
	{-40,0,-40,    0,1,0,   0,      0       },

	{  0,0,  0,    0,1,0,   0.1875f,0.71875f},
	{-15,0,  0,    0,1,0,   0,      0.71875f},
	{-15,0, 17.5,  0,1,0,   0,      0.5     },
	{  0,0, 17.5,  0,1,0,   0.1875f,0.5     },

	{ 15,0,  0,    0,1,0,   0.1875f,0.71875f},
	{  0,0,  0,    0,1,0,   0,      0.71875f},
	{  0,0, 17.5,  0,1,0,   0,      0.5     },
	{ 15,0, 17.5,  0,1,0,   0.1875f,0.5     },

	{  0,0,  0,    0,1,0,   0.1875f,0.71875f},
	{ 15,0,  0,    0,1,0,   0,      0.71875f},
	{ 15,0,-17.5,  0,1,0,   0,      0.5     },
	{  0,0,-17.5,  0,1,0,   0.1875f,0.5     },

	{-15,0,  0,    0,1,0,   0.1875f,0.71875f},
	{  0,0,  0,    0,1,0,   0,      0.71875f},
	{  0,0,-17.5,  0,1,0,   0,      0.5     },
	{-15,0,-17.5,  0,1,0,   0.1875f,0.5     }
};

WORD Lpad02::Idx[60] = {
	0,1,2, 2,1,3, 2,3,5, 5,4,2, 4,5,7, 7,6,4, 6,7,8, 8,7,9,
	9,11,8, 8,11,10, 10,11,1, 1,0,10,
	12,13,15, 14,15,13, 16,17,19, 18,19,17,
	20,21,23, 22,23,21, 24,25,27, 26,27,25
};

Lpad02::Lpad02 (const Base *_base): Lpad (_base)
{
	ngrp = 1;
	texid = 0;
}

int Lpad02::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "TEX")) {
		texid = NameToId (value);
	} else if (!_stricmp (label, "NAV")) {
		if (sscanf (value, "%f", &ILSfreq) != 1) {
			ParseError ("Lpad2: NAV: expected scalar value");
			res = 2;
		}
	}
	return res;
}

bool Lpad02::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp == 0) {
		nvtx = 28;
		nidx = 60;
		_texid = texid;
		undershadow = true;
		groundshadow = false;
		return true;
	} else return false;
}

void Lpad02::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	if (grp == 0) {
		D3DVALUE cosr = scale.x*(D3DVALUE)cos(rot), sinr = scale.x*(D3DVALUE)sin(rot);
		DWORD i;
		WORD iofs = (WORD)idx_ofs;
		NTVERTEX *src = Vtx;
		memcpy (vtx, src, 28*sizeof(NTVERTEX));
		// set pad number
		vtx[17].tu = vtx[18].tu = vtx[25].tu = vtx[26].tu = 0.1875f +
			(vtx[16].tu = vtx[19].tu = vtx[24].tu = vtx[27].tu = (((padno+1)/10)%5)*0.1875f);
		vtx[13].tu = vtx[14].tu = vtx[21].tu = vtx[22].tu = 0.1875f +
			(vtx[12].tu = vtx[15].tu = vtx[20].tu = vtx[23].tu = (((padno+1)%10)%5)*0.1875f);
		vtx[18].tv = vtx[19].tv = vtx[26].tv = vtx[27].tv = 0.21875f - 0.007f +
			(vtx[16].tv = vtx[17].tv = vtx[24].tv = vtx[25].tv = (((padno+1)/10)/5)*0.21875f + 0.5f + 0.0035f);
		vtx[14].tv = vtx[15].tv = vtx[22].tv = vtx[23].tv = 0.21875f - 0.007f +
			(vtx[12].tv = vtx[13].tv = vtx[20].tv = vtx[21].tv = (((padno+1)%10)/5)*0.21875f + 0.5f + 0.0035f);

		NTVERTEX *vtx0 = vtx;
		for (i = 0; i < 28; i++, vtx++, src++) {
			vtx->x = cosr*src->x - sinr*src->z + relpos.x;
			vtx->y = scale.x*src->y + relpos.y;
			vtx->z = sinr*src->x + cosr*src->z + relpos.z;
		}
		MapToAltitude (vtx0, 28);
		for (i = 0; i < 60; i++) *idx++ = Idx[i] + iofs;
	}
}

// ======================================================================================
// class Lpad02a: square landing pad

NTVERTEX Lpad02a::Vtx[37] = {
	{-40,0,-40,      0,1,0,     0.75f,  0.75f  },
	{  0,0,-40,      0,1,0,     0.0035f,0.75f  },
	{ 40,0,-40,      0,1,0,     0.75f,  0.75f  },
	{-40,0,  0,      0,1,0,     0.75f,  0.0035f},
	{  0,0,  0,      0,1,0,     0.0035f,0.0035f},
	{ 40,0,  0,      0,1,0,     0.75f,  0.0035f},
	{-40,0, 40,      0,1,0,     0.75f,  0.75f  },
	{  0,0, 40,      0,1,0,     0.0035f,0.75f  },
	{ 40,0, 40,      0,1,0,     0.75f,  0.75f  },
    {-10,0,-15.417f,  0,1,0,    0.1875f,0.289f },
	{  0,0,-15.417f,  0,1,0,    0.0035f,0.289f },
	{ 10,0,-15.417f,  0,1,0,    0.1875f,0.289f },
	{-10,0, -2.917f,  0,1,0,    0.1875f,0.0547f},
	{  0,0, -2.917f,  0,1,0,    0.0035f,0.0547f},
	{ 10,0, -2.917f,  0,1,0,    0.1875f,0.0547f},
	{-10,0,  2.917f,  0,1,0,    0.1875f,0.0547f},
	{  0,0,  2.917f,  0,1,0,    0.0035f,0.0547f},
	{ 10,0,  2.917f,  0,1,0,    0.1875f,0.0547f},
	{-10,0, 15.417f,  0,1,0,    0.1875f,0.289f },
	{  0,0, 15.417f,  0,1,0,    0.0035f,0.289f },
	{ 10,0, 15.417f,  0,1,0,    0.1875f,0.289f },

	{-10,0,-15.417f,  0,1,0,    0.1875f,0.289f },
    {  0,0,-15.417f,  0,1,0,    0.375f, 0.289f },
	{-10,0, -2.917f,  0,1,0,    0.1875f,0.0547f},
	{  0,0, -2.917f,  0,1,0,    0.375f, 0.0547f},

	{  0,0,-15.417f,  0,1,0,    0.1875f,0.289f },
	{ 10,0,-15.417f,  0,1,0,    0.375f, 0.289f },
	{  0,0, -2.917f,  0,1,0,    0.1875f,0.0547f},
	{ 10,0, -2.917f,  0,1,0,    0.375f, 0.0547f},

	{ 10,0, 15.417f,  0,1,0,    0.1875f,0.289f },
	{  0,0, 15.417f,  0,1,0,    0.375f, 0.289f },
	{ 10,0,  2.917f,  0,1,0,    0.1875f,0.0547f},
	{  0,0,  2.917f,  0,1,0,    0.375f, 0.0547f},

	{  0,0, 15.417f,  0,1,0,    0.1875f,0.289f },
	{-10,0, 15.417f,  0,1,0,    0.375f, 0.289f },
	{  0,0,  2.917f,  0,1,0,    0.1875f,0.0547f},
	{-10,0,  2.917f,  0,1,0,    0.375f, 0.0547f}
};

WORD Lpad02a::Idx[96] = {
	4,13,12,  3,4,12,  3,12,9,  0,3,9,  0,9,1,  1,9,10,
	4,14,13,  5,14,4,  5,11,14, 2,11,5, 1,11,2, 1,10,11,
	4,15,16,  3,15,4,  3,18,15, 6,18,3, 6,7,18, 18,7,19,
	4,16,17,  4,17,5,  5,17,20, 5,20,8, 8,20,7, 7,20,19,
	21,23,22, 24,22,23, 25,27,26, 28,26,27,
	29,31,30, 32,30,31, 33,35,34, 36,34,35
};

Lpad02a::Lpad02a (const Base *_base): Lpad (_base)
{
	ngrp = 1;
	texid = 0;
}

int Lpad02a::ParseLine (const char *label, const char *value)
{
	int res = 0;
	if (!_stricmp (label, "TEX")) {
		texid = NameToId (value);
	} else if (!_stricmp (label, "NAV")) {
		if (sscanf (value, "%f", &ILSfreq) != 1) {
			ParseError ("Lpad2a: NAV: expected scalar value");
			res = 2;
		}
	}
	return res;
}

bool Lpad02a::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	if (grp == 0) {
		nvtx = 37;
		nidx = 96;
		_texid = texid;
		undershadow = true;
		groundshadow = false;
		return true;
	} else return false;
}

void Lpad02a::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	if (grp == 0) {
		D3DVALUE cosr = scale.x*(D3DVALUE)cos(rot), sinr = scale.x*(D3DVALUE)sin(rot);
		DWORD i;
		WORD iofs = (WORD)idx_ofs;
		NTVERTEX *src = Vtx;
		memcpy (vtx, src, 37*sizeof(NTVERTEX));

		for (i = 0; i < 2; i++) {
			int ofs = 21 + i*4;
			int num = padno+1;
			if (!i) num /= 10;
			num = num % 10;
			if (num == 0) {
				vtx[ofs  ].tu = vtx[ofs+2].tu = vtx[ofs+8 ].tu = vtx[ofs+10].tu = 0;
				vtx[ofs+1].tu = vtx[ofs+3].tu = vtx[ofs+9 ].tu = vtx[ofs+11].tu = 0.1875f;
				vtx[ofs  ].tv = vtx[ofs+1].tv = vtx[ofs+8 ].tv = vtx[ofs+9 ].tv = 0.289f + 0.0035f;
				vtx[ofs+2].tv = vtx[ofs+3].tv = vtx[ofs+10].tv = vtx[ofs+11].tv = 0.0547f + 0.0035f;
			} else if (num < 6) {
				vtx[ofs+1].tu = vtx[ofs+3].tu = vtx[ofs+9 ].tu = vtx[ofs+11].tu = 0.1875f +
					(vtx[ofs  ].tu = vtx[ofs+2].tu = vtx[ofs+8 ].tu = vtx[ofs+10].tu = (num-1)*0.1875f);
				vtx[ofs  ].tv = vtx[ofs+1].tv = vtx[ofs+ 8].tv = vtx[ofs+ 9].tv = 0.9922f + 0.0035f;
				vtx[ofs+2].tv = vtx[ofs+3].tv = vtx[ofs+10].tv = vtx[ofs+11].tv = 0.7578f + 0.0035f;
			} else {
				vtx[ofs  ].tu = vtx[ofs+1].tu = vtx[ofs+ 8].tu = vtx[ofs+ 9].tu = 0.9922f + 0.0035f;
				vtx[ofs+2].tu = vtx[ofs+3].tu = vtx[ofs+10].tu = vtx[ofs+11].tu = 0.7578f + 0.0035f;
				vtx[ofs  ].tv = vtx[ofs+2].tv = vtx[ofs+ 8].tv = vtx[ofs+10].tv = 0.1875f +
					(vtx[ofs+1].tv = vtx[ofs+3].tv = vtx[ofs+9].tv = vtx[ofs+11].tv = (num-6)*0.1875f);
			}
		}

		NTVERTEX *vtx0 = vtx;
		for (i = 0; i < 37; i++, vtx++, src++) {
			vtx->x = cosr*src->x - sinr*src->z + relpos.x;
			vtx->y = scale.x*src->y + relpos.y;
			vtx->z = sinr*src->x + cosr*src->z + relpos.z;
		}
		MapToAltitude (vtx0, 37);

		for (i = 0; i < 96; i++) *idx++ = Idx[i] + iofs;
	}
}

// ==============================================================================
// class Runway: mesh for runway (no lighting)

Runway::Runway (const Base *_base): BaseObject (_base)
{
	ngrp = 1;
	nrwseg = 0;
	width = 10.0f;
	texid[0] = 0;
	ILSfreq[0] = ILSfreq[1] = 0.0f; // undefined
	dyndata = 0;
}

Runway::~Runway ()
{
	Deactivate();
	if (nrwseg) {
		delete []rwseg;
		rwseg = NULL;
	}
}

bool Runway::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	switch (grp) {
	case 0:
		nvtx = dyndata->nRwVtx;
		nidx = dyndata->nRwIdx;
		_texid = texid[0];
		undershadow = true;
		groundshadow = false;
		return true;
	default:
		return false;
	}
}

void Runway::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	DWORD i;
	WORD iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		memcpy (vtx, dyndata->RwVtx, dyndata->nRwVtx*sizeof(NTVERTEX));
		for (i = 0; i < dyndata->nRwIdx; i++) *idx++ = dyndata->RwIdx[i] + iofs;
		return;
	}
}

int Runway::Read (istream &is)
{
	char cbuf[256], *cp, label[32];
	int i;
	DWORD k;

	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "END1"))
			sscanf (cp+4, "%f%f%f", &end1.x, &end1.y, &end1.z);
		else if (!_stricmp (label, "END2"))
			sscanf (cp+4, "%f%f%f", &end2.x, &end2.y, &end2.z);
		else if (!_stricmp (label, "WIDTH")) {
			sscanf (cp+5, "%f", &width);
			width *= 0.5f;
		} else if (!_strnicmp (label, "ILS", 3)) {
			float freq;
			sscanf (cp+3, "%d%f", &i, &freq);
			ILSfreq[i-1] = freq;
		} else if (!_stricmp (label, "NRWSEG")) {
			if (nrwseg) {
				delete []rwseg;
				rwseg = NULL;
			}
			sscanf (cp+6, "%d", &nrwseg);
			if (nrwseg) {
				rwseg = new RWSEG[nrwseg]; TRACENEW
				for (k = 0; k < nrwseg; k++) {  // reset defaults
					rwseg[k].subseg = (k ? 0 : 4);
					rwseg[k].len    = (k ? 0.0f : 1.0f);
					rwseg[k].tu0    = 0.0f;
					rwseg[k].tu1    = 0.5f;
					rwseg[k].tv0    = 0.0f;
					rwseg[k].tv1    = 10.0f;
				}
			}
		} else if (!_strnicmp (label, "RWSEG", 5)) {
			D3DVALUE seglen, tu0, tu1, tv0, tv1;
			DWORD subseg;
			sscanf (cp+5, "%d%d%f%f%f%f%f", &i, &subseg, &seglen, &tu0, &tu1, &tv0, &tv1);
			if (--i >= 0 && i < (int)nrwseg) {
				rwseg[i].subseg = subseg;
				rwseg[i].len    = seglen;
				rwseg[i].tu0    = tu0;
				rwseg[i].tu1    = tu1;
				rwseg[i].tv0    = tv0;
				rwseg[i].tv1    = tv1;
			}
		} else if (!_stricmp (label, "RWTEX")) {
			sscanf (cp+5, "%s", label);
			texid[0] = NameToId (label);
		}
	} while (_stricmp (label, "END"));
	return 0;
}

void Runway::Activate ()
{
	if (dyndata) return; // active already
	dyndata = new struct DYNDATA; TRACENEW

	DWORD i, j, k, m;
	WORD ofs;

	dyndata->nRwVtx = dyndata->nRwIdx = 0;
	for (i = 0; i < nrwseg; i++) {
		if (rwseg[i].subseg) {
			dyndata->nRwVtx += (rwseg[i].subseg+1) << 1;
			dyndata->nRwIdx += rwseg[i].subseg * 6;
		}
	}
	dyndata->RwVtx = new NTVERTEX[dyndata->nRwVtx]; TRACENEW
	dyndata->RwIdx = new WORD[dyndata->nRwIdx]; TRACENEW

	D3DVALUE x, z, step;
	D3DVALUE dx = end2.x - end1.x;
	D3DVALUE dz = end2.z - end1.z;
	D3DVALUE len = D3DVAL(std::hypot (dx, dz));
	D3DVALUE dwx = (width/len) * dz;
	D3DVALUE dwz = (width/len) * dx;
	D3DVALUE s0, x0, z0, ddx, ddz;

	for (i = k = m = 0, s0 = 0.0f; i < nrwseg; i++) {
		x0 = dx*s0 + end1.x;
		z0 = dz*s0 + end1.z;
		ddx = rwseg[i].len * dx;
		ddz = rwseg[i].len * dz;
		for (j = 0; j < rwseg[i].subseg; j++) {
			ofs = (WORD)(k + j*2);
			dyndata->RwIdx[m++] = ofs;
			dyndata->RwIdx[m++] = ofs+1;
			dyndata->RwIdx[m++] = ofs+2;
			dyndata->RwIdx[m++] = ofs+3;
			dyndata->RwIdx[m++] = ofs+2;
			dyndata->RwIdx[m++] = ofs+1;
		}
		for (j = 0; j <= rwseg[i].subseg; j++) {
			step = D3DVAL(j)/D3DVAL(rwseg[i].subseg);
			x = x0 + step*ddx;
			z = z0 + step*ddz;
			dyndata->RwVtx[k].x = x + dwx;
			dyndata->RwVtx[k].y = 0.0f;
			dyndata->RwVtx[k].z = z - dwz;
			dyndata->RwVtx[k].nx = dyndata->RwVtx[k].nz = 0.0f;
			dyndata->RwVtx[k].ny = 1.0f;
			dyndata->RwVtx[k].tu = rwseg[i].tu0;
			dyndata->RwVtx[k].tv = ((j&1) ? rwseg[i].tv1 : rwseg[i].tv0);
			k++;
			dyndata->RwVtx[k].x = x - dwx;
			dyndata->RwVtx[k].y = 0.0f;
			dyndata->RwVtx[k].z = z + dwz;
			dyndata->RwVtx[k].nx = dyndata->RwVtx[k].nz = 0.0f;
			dyndata->RwVtx[k].ny = 1.0f;
			dyndata->RwVtx[k].tu = rwseg[i].tu1;
			dyndata->RwVtx[k].tv = dyndata->RwVtx[k-1].tv;
			k++;
		}
		s0 += rwseg[i].len;
	}
	MapToAltitude (dyndata->RwVtx, dyndata->nRwVtx);
	MapToCurvature (dyndata->RwVtx, dyndata->nRwVtx);
}

void Runway::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->RwVtx;
		dyndata->RwVtx = NULL;
		delete []dyndata->RwIdx;
		dyndata->RwIdx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

// ==============================================================================
// class RunwayLights: standard lighting for runways (using the same techniques
// as BeaconArray)

RunwayLights::RunwayLights (const Base *_base): BaseObject (_base)
{
	count1 = 40;
	width = 10.0;
	vasi = 0;
	papi = 0;
	dyndata = 0;
}

RunwayLights::~RunwayLights ()
{
	Deactivate();
	if (vasi) delete vasi;
	if (papi) delete papi;
}

int RunwayLights::Read (istream &is)
{
	char cbuf[256], *cp, label[32];

	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "END1"))
			sscanf (cp+4, "%f%f%f", &end1.x, &end1.y, &end1.z);
		else if (!_stricmp (label, "END2"))
			sscanf (cp+4, "%f%f%f", &end2.x, &end2.y, &end2.z);
		else if (!_stricmp (label, "COUNT1"))
			sscanf (cp+6, "%d", &count1);
		else if (!_stricmp (label, "WIDTH")) {
			sscanf (cp+5, "%f", &width);
			width *= 0.5f;
		} else if (!_stricmp (label, "PAPI")) {
			if (!papi) { papi = new struct PAPIDATA; TRACENEW }
			sscanf (cp+4, "%f%f%f", &papi->apprangle, &papi->aperture, &papi->ofs);
			papi->apprangle *= (float)RAD;
			papi->aperture *= (float)RAD;
		} else if (!_stricmp (label, "VASI")) {
			if (!vasi) { vasi = new struct VASIDATA; TRACENEW }
			sscanf (cp+4, "%f%f%f", &vasi->apprangle, &vasi->lightsep, &vasi->ofs);
			vasi->apprangle *= (float)RAD;
		}
	} while (_stricmp (label, "END"));
	return 0;
}

void RunwayLights::Setup ()
{
	relpos = Vector((end1.x+end2.x)*0.5, (end1.y+end2.y)*0.5, (end1.z+end2.z)*0.5);
	BaseObject::Setup();
	NTVERTEX vtx[2];
	vtx[0].x = end1.x, vtx[0].y = end1.y, vtx[0].z = end1.z;
	vtx[1].x = end2.x, vtx[1].y = end2.y, vtx[1].z = end2.z;
	MapToAltitude (vtx,2);
	MapToCurvature (vtx,2);
	end1.x = vtx[0].x, end1.y = vtx[0].y, end1.z = vtx[0].z;
	end2.x = vtx[1].x, end2.y = vtx[1].y, end2.z = vtx[1].z;
}

void RunwayLights::Render (LPDIRECT3DDEVICE7 dev, bool day)
{
}

void RunwayLights::VertexArray (DWORD count, const Vector &cpos, const Vector &pos, const Vector &ofs, double size, POSTEXVERTEX *&Vtx)
{
	size *= 0.15; // for distance resizing (see below bsize)

	DWORD i;
	double bsize, ap = g_camera->Aperture();
	Vector p(pos), bdir, v1, v2;

	for (i = 0; i < count; i++) {

		bdir.Set (p-cpos);
		// beacon position rel to camera

		bsize = size * sqrt((bdir.length()+1.0)*ap);
		// this resizes the beacons so they appear larger at greater distance
		// may need more thought

		if (!bdir.y || !bdir.z) {
			v1.Set (0,bsize,0);
			v2.Set (0,0,bsize);
		} else {
			v1.Set (bdir.z,0,-bdir.x);
			v2.Set (-bdir.x*bdir.y, bdir.z*bdir.z+bdir.x*bdir.x, -bdir.y*bdir.z);
			v1 *= bsize/v1.length();
			v2 *= bsize/v2.length();
		}
		// v1 and v2 are two orthogonal vectors perpendicular to the vector from
		// camera to beacon. Required to set up the billboard vertices

		// now construct the 4 vertices for the billboard
		Vtx->x = (float)(p.x - v1.x - v2.x);
		Vtx->y = (float)(p.y - v1.y - v2.y);
		Vtx->z = (float)(p.z - v1.z - v2.z);
		Vtx++;
		Vtx->x = (float)(p.x + v1.x - v2.x);
		Vtx->y = (float)(p.y + v1.y - v2.y);
		Vtx->z = (float)(p.z + v1.z - v2.z);
		Vtx++;
		Vtx->x = (float)(p.x + v1.x + v2.x);
		Vtx->y = (float)(p.y + v1.y + v2.y);
		Vtx->z = (float)(p.z + v1.z + v2.z);
		Vtx++;
		Vtx->x = (float)(p.x - v1.x + v2.x);
		Vtx->y = (float)(p.y - v1.y + v2.y);
		Vtx->z = (float)(p.z - v1.z + v2.z);
		Vtx++;

		// move position to next element in array
		p += ofs;
	}
}

void RunwayLights::Update ()
{
	Vector pos, ofs;
	POSTEXVERTEX *Vtx;

	// transform camera position and direction into local base coords
	Vector cpos (tmul (base->GRot(), g_camera->GPos()-base->GPos()));
	Vector cdir (tmul (base->GRot(), g_camera->Direction()));

	bool look12 = (dotp (cdir, dyndata->ref2-dyndata->ref1) >= 0);
	// camera looks in direction from ref1 to ref2

	if (papi) { // light configuration for PAPI indicator
		double dist, alpha, refx, refz;
		if (look12) {
			refx = dyndata->ref1.x + papi->ofs*dyndata->dir.x;
			refz = dyndata->ref1.z + papi->ofs*dyndata->dir.z;
		} else {
			refx = dyndata->ref2.x - papi->ofs*dyndata->dir.x;
			refz = dyndata->ref2.z - papi->ofs*dyndata->dir.z;
		}
		dist = (refz-cpos.z)*dyndata->dir.z + (refx-cpos.x)*dyndata->dir.x;
		alpha = (dist ? atan(cpos.y/fabs(dist)) : Pi05);
		int nwhite = (int)((alpha-papi->apprangle)/papi->aperture + 2.5);
		if      (nwhite < 0) nwhite = 0;
		else if (nwhite > 4) nwhite = 4;
		dyndata->PAPIwhite = (DWORD)nwhite;
	}

	dyndata->night = (*dyndata->csun < 0.2);
	// skip runway lights during daytime

	// generate billboard vertices for all light components

	if (dyndata->night) {
		Vtx = dyndata->Vtx_white_night;

		// 1. center line
		pos = dyndata->ref1;
		ofs = dyndata->ofs1;
		VertexArray (dyndata->nb_centre, cpos, pos, ofs, 0.5, Vtx);

		// 2. right side line
		pos += dyndata->ofs3;
		ofs *= 2.0;
		VertexArray (dyndata->nb_side, cpos, pos, ofs, 0.5, Vtx);

		// 3. left side line
		pos = dyndata->ref1 - dyndata->ofs3;
		VertexArray (dyndata->nb_side, cpos, pos, ofs, 0.5, Vtx);

	} else {
		// skip night-only lights
		Vtx = dyndata->Vtx_white_day;
	}

	// 4. approach line
	ofs = dyndata->ofs1 * (look12 ? -4.0 : 4.0);
	pos = (look12 ? dyndata->ref1 : dyndata->ref2) + ofs;
	VertexArray (dyndata->nb_approach-1, cpos, pos, ofs, 1.0, Vtx);
	// modify the last vertex to animate strobe
	if (td.SimT1 > dyndata->flashtime) {
		dyndata->flashtime = td.SimT1+0.1; // flash at 10 Hz
		if (--dyndata->flashpos < 0) dyndata->flashpos = 15;
	}
	VertexArray (1, cpos, pos + ofs*(dyndata->flashpos-0.5), ofs, 1.0, Vtx);

	// 5. VASI indicator white lights
	if (vasi) {
		if (look12) {
			pos = dyndata->ref1 + dyndata->ofs4;
			ofs = dyndata->nml * 7.0;
		} else {
			pos = dyndata->ref2 - dyndata->ofs4;
			ofs = dyndata->nml * -7.0;
		}
		pos.y += dyndata->vasi_wy;
		VertexArray (dyndata->nb_vasi_w, cpos, pos, ofs, 1.0, Vtx);
	}

	// 6. End-of-runway red lights
	ofs = dyndata->ofs3 / -1.5;
	pos = (look12 ? dyndata->ref2 : dyndata->ref1) + dyndata->ofs3*2.0;
	VertexArray (dyndata->nb_end, cpos, pos, ofs, 1.0, Vtx);

	// 7. VASI indicator red bar
	if (vasi) {
		if (look12) {
			pos = dyndata->ref1 + dyndata->ofs5;
			ofs = dyndata->nml * 60.0;
		} else {
			pos = dyndata->ref2 - dyndata->ofs5;
			ofs = dyndata->nml * -60.0;
		}
		pos.y += dyndata->vasi_ry;
		Vtx[0].x = Vtx[3].x = (float)pos.x;  Vtx[1].x = Vtx[2].x = (float)(pos.x + ofs.x);
		Vtx[0].z = Vtx[3].z = (float)pos.z;  Vtx[1].z = Vtx[2].z = (float)(pos.z + ofs.z);
		Vtx[0].y = Vtx[1].y = (float)(pos.y - 2.0);  Vtx[2].y = Vtx[3].y = (float)(pos.y + 2.0); Vtx += 4;
	}

	if (papi) {
	// 8. PAPI indicator lights right
		ofs = dyndata->nml * 20.0;
		pos = (look12 ? dyndata->ref1 + dyndata->dir*papi->ofs : dyndata->ref2 - dyndata->dir*papi->ofs) + dyndata->ofs3*3.0;
		VertexArray (4, cpos, pos, ofs, 1.0, Vtx);
	// 9. PAPI indicator lights left
		ofs = -ofs;
		pos -= dyndata->ofs3*6.0;
		VertexArray (4, cpos, pos, ofs, 1.0, Vtx);
	}
}

void RunwayLights::Activate ()
{
	if (dyndata) return; // active already

	DWORD i, j, ii, n, nv, ni, nbc, nbs, nba, nbe, nbvw, nbvr, nbwn, nbwd, nbr, nbp, nbt;
	POSTEXVERTEX *Vtx;
	WORD *Idx;

	dyndata = new struct DYNDATA; TRACENEW

	nbs  = dyndata->nb_side = count1;                    // number of lights at one runway side
	nbc  = dyndata->nb_centre = count1*2-1;              // number of lights along runway centre
	nba  = dyndata->nb_approach = 7;                     // number of lights along approach path
	nbe  = dyndata->nb_end = 7;                          // number of lights as end-of-runway markers
	nbvw = dyndata->nb_vasi_w = (vasi ? 3:0);            // number of white lights in VASI indicator
	nbvr = dyndata->nb_vasi_r = (vasi ? 1:0);            // number of red lights in VASI indicator
	nbwn = dyndata->nb_white_night = nbc + nbs*2;        // number of white lights (night only)
	nbwd = dyndata->nb_white_day = nba + nbvw;           // number of white lights (day and night)
	nbr  = dyndata->nb_red = nbe + nbvr;                 // number of red lights
	nbp  = (papi ? 8:0);                                 // number of precision approach lights
	nbt  = dyndata->nb_tot = nbwn + nbwd + nbr + nbp;    // total number of lights

	dyndata->ref1.Set (end1.x, end1.y, end1.z);
	dyndata->ref2.Set (end2.x, end2.y, end2.z);
	Vector dr(end2.x-end1.x, end2.y-end1.y, end2.z-end1.z);
	dyndata->ofs1.Set (dr/(nbc-1));
	dr.unify();
	dyndata->dir.Set (dr);
	dyndata->ofs3.Set (dr.z*width, 0, -dr.x*width);
	dyndata->nml.Set (dyndata->ofs3.unit());
	if (vasi) {
		dyndata->ofs5.Set (dr*vasi->ofs + dyndata->ofs3*2.0);
		//dyndata->ofs5.y += 2.0;
		dyndata->ofs4.Set (dyndata->ofs5 - dr*vasi->lightsep);
		dyndata->vasi_ry = 2.0;
		dyndata->vasi_wy = dyndata->vasi_ry + vasi->lightsep * tan(vasi->apprangle);
		//dyndata->ofs4.y += vasi->lightsep * tan(vasi->apprangle);
	}

	Vtx = dyndata->Vtx = new POSTEXVERTEX[dyndata->nVtx = nv = nbt*4]; TRACENEW
	ni = nbt*6; if (papi) ni += 8*6; // we have separate indices for white and red PAPI lights
	Idx = dyndata->Idx = new WORD[dyndata->nIdx = ni]; TRACENEW
	dyndata->Vtx_white_night = Vtx; dyndata->nVtx_white_night = nbwn*4;
	dyndata->Idx_white_night = Idx; dyndata->nIdx_white_night = nbwn*6;
	dyndata->Vtx_white_day   = dyndata->Vtx_white_night + nbwn*4; dyndata->nVtx_white_day = nbwd*4;
	dyndata->Idx_white_day   = dyndata->Idx_white_night + nbwn*6; dyndata->nIdx_white_day = nbwd*6;
	dyndata->Vtx_red         = dyndata->Vtx_white_day + nbwd*4;   dyndata->nVtx_red = nbr*4;
	dyndata->Idx_red         = dyndata->Idx_white_day + nbwd*6;   dyndata->nIdx_red = nbr*6;
	dyndata->flashpos = 15;
	dyndata->flashtime = td.SimT0;

	for (i = 0; i < nbt; i++) {
		Vtx[i*4  ].tu = Vtx[i*4+3].tu = Vtx[i*4  ].tv = Vtx[i*4+1].tv = 0.0;
		Vtx[i*4+1].tu = Vtx[i*4+2].tu = Vtx[i*4+2].tv = Vtx[i*4+3].tv = 1.0;
	}
	for (j = 0; j < 3; j++) {
		switch (j) {
		case 0: Idx = dyndata->Idx_white_night; n = nbwn; break;
		case 1: Idx = dyndata->Idx_white_day;   n = nbwd; break;
		case 2: Idx = dyndata->Idx_red;         n = nbr;  break;
		}
		for (i = ii = 0; i < n; i++) {
			WORD idx = (WORD)(i*4);
			Idx[ii++] = idx;
			Idx[ii++] = idx+2;
			Idx[ii++] = idx+1;
			Idx[ii++] = idx+3;
			Idx[ii++] = idx+2;
			Idx[ii++] = idx;
		}
	}
	if (papi) {
		dyndata->Vtx_PAPI = dyndata->Vtx_red + nbr*4;
		dyndata->Idx_PAPI_r = dyndata->Idx_red + nbr*6;
		dyndata->Idx_PAPI_w = dyndata->Idx_PAPI_r + 8*6;
		static WORD idx[96] = {0,2,1,3,2,0,16,18,17,19,18,16,       // red (inner lights)
			                   4,6,5,7,6,4,20,22,21,23,22,20,       // red (inner-mid lights)
							   8,10,9,11,10,8,24,26,25,27,26,24,    // red (outer-mid lights)
							   12,14,13,15,14,12,28,30,29,31,30,28, // red (outer lights)
							   12,14,13,15,14,12,28,30,29,31,30,28, // white (outer lights)
							   8,10,9,11,10,8,24,26,25,27,26,24,    // white (outer-mid lights)
			                   4,6,5,7,6,4,20,22,21,23,22,20,       // white (inner-mid-lights)
							   0,2,1,3,2,0,16,18,17,19,18,16};      // white (inner lights)
		memcpy (dyndata->Idx_PAPI_r, idx, 96*sizeof(WORD));
	}
	dyndata->csun = &base->SunDirectionBuffered()->y;

	static char texname[8] = {'B','A','L','L',0,0,0,0};
	SURFHANDLE dummy;
	base->GetGenericTexture (*(LONGLONG*)texname, (SURFHANDLE&)dyndata->tex, dummy);
}

void RunwayLights::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete []dyndata->Idx;
		dyndata->Idx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

// ==============================================================================
// class BeaconArray: line of emissive spheres (implemented as billboards) for
// runway lighting etc.

BeaconArray::BeaconArray (const Base *_base): BaseObject (_base)
{
	count = 10;
	size = 1.0;
	col_r = col_g = col_b = 1.0f;
}

int BeaconArray::Read (istream &is)
{
	char cbuf[256], *cp, label[32];
	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "END1"))
			sscanf (cp+4, "%f%f%f", &end1.x, &end1.y, &end1.z);
		else if (!_stricmp (label, "END2"))
			sscanf (cp+4, "%f%f%f", &end2.x, &end2.y, &end2.z);
		else if (!_stricmp (label, "COUNT"))
			sscanf (cp+5, "%d", &count);
		else if (!_stricmp (label, "SIZE"))
			sscanf (cp+4, "%lf", &size);
		else if (!_stricmp (label, "COL"))
			sscanf (cp+3, "%f%f%f", &col_r, &col_g, &col_b);
	} while (_stricmp (label, "END"));
	return 0;
}

void BeaconArray::Render (LPDIRECT3DDEVICE7 dev, bool day)
{
}

void BeaconArray::Update ()
{
	const double resize_fac = 0.1;

	DWORD i;
	Vector p, v1, v2;
	double bsize = size;

	// transform camera into local base coords
	Vector cdir (tmul (base->GRot(), g_camera->GPos()-base->GPos()));

	for (i = 0; i < count; i++) {
		Vector bdir (Pos[i]-cdir); // vector from camera to beacon i
		bsize = size * sqrt(bdir.length()+1.0)*resize_fac; // distance scaling of beacon size

		if (bdir.y == 0.0 && bdir.z == 0.0) {
			v1.Set (0,1,0);
			v2.Set (0,0,1);
		} else {
			v1.Set (bdir.z,0,-bdir.x);
			v2.Set (-bdir.x*bdir.y, bdir.z*bdir.z+bdir.x*bdir.x, -bdir.y*bdir.z);
			v1.unify(); v2.unify();
		}

		p.Set(Pos[i] - (v1+v2)*bsize);
		Vtx[i*4].x = (float)p.x;
		Vtx[i*4].y = (float)p.y;
		Vtx[i*4].z = (float)p.z;

		p.Set(Pos[i] + (v1-v2)*bsize);
		Vtx[i*4+1].x = (float)p.x;
		Vtx[i*4+1].y = (float)p.y;
		Vtx[i*4+1].z = (float)p.z;

		p.Set(Pos[i] + (v1+v2)*bsize);
		Vtx[i*4+2].x = (float)p.x;
		Vtx[i*4+2].y = (float)p.y;
		Vtx[i*4+2].z = (float)p.z;

		p.Set(Pos[i] - (v1-v2)*bsize);
		Vtx[i*4+3].x = (float)p.x;
		Vtx[i*4+3].y = (float)p.y;
		Vtx[i*4+3].z = (float)p.z;
	}
}

void BeaconArray::Activate ()
{
	DWORD i, ii;

	Vtx = new POSTEXVERTEX[nVtx = count*4]; TRACENEW
	Idx = new WORD[nIdx = count*6]; TRACENEW
	Pos = new Vector[count]; TRACENEW

	double ici, ic = 1.0/(count-1);
	for (i = 0; i < count; i++) {
		ici = ic*i;
		Pos[i].Set ((end2.x-end1.x)*ici + end1.x,
			        (end2.y-end1.y)*ici + end1.y,
					(end2.z-end1.z)*ici + end1.z);
	}

	for (i = 0; i < count; i++) {
		Vtx[i*4  ].tu = Vtx[i*4+3].tu = Vtx[i*4  ].tv = Vtx[i*4+1].tv = 0.0;
		Vtx[i*4+1].tu = Vtx[i*4+2].tu = Vtx[i*4+2].tv = Vtx[i*4+3].tv = 1.0;
	}

	for (i = ii = 0; i < count; i++) {
		WORD idx = (WORD)(i*4);
		Idx[ii++] = idx;
		Idx[ii++] = idx+2;
		Idx[ii++] = idx+1;
		Idx[ii++] = idx+3;
		Idx[ii++] = idx+2;
		Idx[ii++] = idx;
	}
	lightmat = new D3DMATERIAL7; TRACENEW
	lightmat->emissive.r = col_r;
	lightmat->emissive.g = col_g;
	lightmat->emissive.b = col_b;

	static LONGLONG texid = NameToId ("Ball");
	SURFHANDLE dummy;
	base->GetGenericTexture (texid, (SURFHANDLE&)tex, dummy);
}

void BeaconArray::Deactivate ()
{
	delete []Vtx;
	Vtx = NULL;
	delete []Idx;
	Idx = NULL;
	delete []Pos;
	Pos = NULL;
	delete lightmat;
}

// ==============================================================================
// class Train: base class for train-type objects (including track)

Train::Train (const Base *_base): BaseObject (_base)
{
	end1.x = end1.y = end1.z = 0.0f;
	end2.x = 1000.0f; end2.y = end2.z = 0.0f; // arbitrary end points
	maxspeed = 30.0f;
	slowzone = 100.0f;
}

void Train::Init (const D3DVECTOR &_end1, const D3DVECTOR &_end2)
{
	end1 = _end1;
	end2 = _end2;
	dir.x = end2.x - end1.x;
	dir.y = end2.y - end1.y;
	dir.z = end2.z - end1.z;
	length = (D3DVALUE)sqrt (dir.x*dir.x + dir.y*dir.y + dir.z*dir.z);
	dir.x /= length;
	dir.y /= length;
	dir.z /= length;
	minpos = 10.0f;
	maxpos = length-10.0f;
	speedfac = (maxspeed-1.0f)/(slowzone-minpos);
};

void Train::Setup ()
{
	double lng, lat;
	base->Rel_EquPos (Vector(end1.x, end1.y, end1.z), lng, lat);
	end1.y += base->RefPlanet()->Elevation (lng, lat)-base->Elevation();
	base->Rel_EquPos (Vector(end2.x, end2.y, end2.z), lng, lat);
	end2.y += base->RefPlanet()->Elevation (lng, lat)-base->Elevation();
}

void Train::SetCabin (DWORD nvtx, const NTVERTEX *ref, NTVERTEX *res, const D3DVECTOR &pos, const D3DVECTOR &ofs)
{
	// rotation matrix
	D3DVALUE sinth, costh, cosph, sinph, vx, vy, vz;
	costh = (D3DVALUE)cos (asin (sinth = (end2.y-end1.y)/length));
	double ph = atan2 (end2.x-end1.x, end2.z-end1.z);
	cosph = (D3DVALUE)cos(ph), sinph = (D3DVALUE)sin(ph);
	D3DVALUE r11 =  cosph, r12 = -sinph*sinth, r13 = sinph*costh;
	D3DVALUE r21 =  0.0,   r22 =  costh,       r23 = sinth;
	D3DVALUE r31 = -sinph, r32 = -cosph*sinth, r33 = cosph*costh;

	for (DWORD i = 0; i < nvtx; i++) {
		const NTVERTEX &vtx = ref[i];
		vx = vtx.x + ofs.x;
		vy = vtx.y + ofs.y;
		vz = vtx.z + ofs.z;
		res[i].x  = vx*r11 + vy*r12 + vz*r13 + pos.x;
		res[i].y  = vx*r21 + vy*r22 + vz*r23 + pos.y;
		res[i].z  = vx*r31 + vy*r32 + vz*r33 + pos.z;
	}
}

D3DVALUE Train::MoveCabin (D3DVALUE &pos, D3DVALUE &vel, bool &atmin)
{
	atmin = false;
	D3DVALUE ds = vel*(D3DVALUE)td.SimDT;
	if ((pos += ds) < slowzone) {
		if (pos < minpos) ds -= pos-minpos, pos = minpos, vel = 1.0, atmin = true;
		else vel = ((pos-minpos)*speedfac+1.0f) * (vel > 0.0 ? 1.0f:-1.0f);
	} else if (pos > length-slowzone) {
		if (pos > maxpos) ds -= pos-maxpos, pos = maxpos, vel = -1.0f;
		else vel = ((maxpos-pos)*speedfac+1.0f) * (vel > 0.0 ? 1.0f:-1.0f);
	}
	return ds;
}

// ======================================================================================
// class Train1: monorail-type train

static NTVERTEX cabin1[30] = {
	{ 2,  4.5, 0,       0,1,0,  0,0.75},
	{-2,  4.5, 0,       0,1,0,  0,0.5},
	{ 2,  4.5, 8,       0,1,0,  0.34375f,0.75f},
	{-2,  4.5, 8,       0,1,0,  0.34375f,0.5f},
	{ 3,  2,  10,       0,0,1,  0.516f,0.75f},
	{-3,  2,  10,       0,0,1,  0.516f,0.5f},
	{ 2.2f,0.5f, 9.5f,  0,-1,0, 0.625f,0.75f},
	{-2.2f,0.5f, 9.5f,  0,-1,0, 0.625f,0.5f},
	{ 2.2f,0.5f, 0.0f,  0,-1,0, 1,0.75f},
	{-2.2f,0.5f, 0.0f,  0,-1,0, 1,0.5f},
	{ 2.2f,0.5f,-9.5f,  0,-1,0, 0.625f,0.75f},
	{-2.2f,0.5f,-9.5f,  0,-1,0, 0.625f,0.5f},
	{ 3,  2, -10,       0,0,-1, 0.516f,0.75f},
	{-3,  2, -10,       0,0,-1, 0.516f,0.5f},
	{ 2,  4.5,-8,       0,1,0,  0.34375f,0.75f},
	{-2,  4.5,-8,       0,1,0,  0.34375f,0.5f},
	{ 2,  4.5, 0,       0,1,0,  0,0.75},
	{-2,  4.5, 0,       0,1,0,  0,0.5},
	{ 2,  4.5,-8,     0.866f,0.5,0,  0.1f,0.75f},
	{ 2,  4.5, 8,     0.866f,0.5,0,  0.9f,0.75f},
	{ 3,  2, -10,     1,0,0,        0,0.9f},
	{ 3,  2,  10,     1,0,0,        1,0.9f},
	{ 2.2f,0.5,-9.5,   0.866f,-0.5,0, 0.05f,1},
	{ 2.2f,0.5, 9.5,   0.866f,-0.5,0, 0.95f,1},
	{-2,  4.5, 8,    -0.866f,0.5,0,  0.1f,0.75f},
	{-2,  4.5,-8,    -0.866f,0.5,0,  0.9f,0.75f},
	{-3,  2,  10,    -1,0,0,        0,0.9f},
	{-3,  2, -10,    -1,0,0,        1,0.9f},
	{-2.2f,0.5, 9.5,  -0.866f,-0.5,0, 0.05f,1},
	{-2.2f,0.5,-9.5,  -0.866f,-0.5,0, 0.95f,1},
};

static WORD cabin1_idx[78] = {
	0,1,2,3,2,1,2,3,4,5,4,3,4,5,6,7,6,5,6,7,8,9,8,7,8,9,10,11,10,9,
	10,11,12,13,12,11,12,13,14,15,14,13,14,15,16,17,16,15,
	18,19,20,21,20,19,20,21,22,23,22,21,24,25,26,27,26,25,26,27,28,29,28,27
};

static NTVERTEX mrail1[8] = {
	{-2.5,0,0,  -0.707f,0.707f,0,   0,0},
	{-2.5,0,1,  -0.707f,0.707f,0,   6,0},
	{-1.5,1,0,   0,1,0,             0,0.15f},
	{-1.5,1,1,   0,1,0,             6,0.15f},
	{ 1.5,1,0,   0,1,0,             0,0.35f},
	{ 1.5,1,1,   0,1,0,             6,0.35f},
	{ 2.5,0,0,   0.707f,0.707f,0,   0,0.5},
	{ 2.5,0,1,   0.707f,0.707f,0,   6,0.5}
};

static WORD mrail1_idx[18] = {
	0,1,2,3,2,1,2,3,4,5,4,3,4,5,6,7,6,5
};

Train1::Train1 (const Base *_base): Train (_base)
{
	texid = 0;
	tuscale_track = 1.0f;
	ngrp = 2;
	dyndata = 0;
}

Train1::~Train1 ()
{
	Deactivate ();
}

int Train1::Read (istream &is)
{
	char cbuf[256], *cp, label[32];

	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "END1"))
			sscanf (cp+4, "%f%f%f", &end1.x, &end1.y, &end1.z);
		else if (!_stricmp (label, "END2"))
			sscanf (cp+4, "%f%f%f", &end2.x, &end2.y, &end2.z);
		else if (!_stricmp (label, "MAXSPEED"))
			sscanf (cp+8, "%f", &maxspeed);
		else if (!_stricmp (label, "SLOWZONE"))
			sscanf (cp+8, "%f", &slowzone);
		else if (!_stricmp (label, "TEX")) {
			sscanf (cp+3, "%s%f", label, &tuscale_track);
			texid = NameToId (label);
		}
	} while (_stricmp (label, "END"));

	Init (end1, end2);
	return 0;
}

void Train1::Activate ()
{
	if (dyndata) return; // already active

	DWORD i;
	dyndata = new struct DYNDATA; TRACENEW
	dyndata->Vtx = new NTVERTEX[38]; TRACENEW
	dyndata->tick = rand()%10000;
	shvtx = 0;
	NTVERTEX *Vtx = dyndata->Vtx;

	// rotation matrix
	D3DVALUE sinth, costh, cosph, sinph;
	costh = (D3DVALUE)cos (asin (sinth = (end2.y-end1.y)/length));
	double ph = atan2 (end2.x-end1.x, end2.z-end1.z);
	cosph = (D3DVALUE)cos(ph), sinph = (D3DVALUE)sin(ph);
	D3DVALUE r11 =  cosph, r12 = -sinph*sinth, r13 = sinph*costh;
	D3DVALUE r21 =  0.0,   r22 =  costh,       r23 = sinth;
	D3DVALUE r31 = -sinph, r32 = -cosph*sinth, r33 = cosph*costh;

	// cabin vertices
	for (i = 0; i < 30; i++) {
		NTVERTEX &src = cabin1[i];
		NTVERTEX &tgt = Vtx[i];
		tgt.nx = src.nx*r11 + src.ny*r12 + src.nz*r13;
		tgt.ny = src.nx*r21 + src.ny*r22 + src.nz*r23;
		tgt.nz = src.nx*r31 + src.ny*r32 + src.nz*r33;
		tgt.tu = src.tu;
		tgt.tv = src.tv;
	}

	// track vertices
	for (i = 0; i < 8; i++) {
		NTVERTEX &src = mrail1[i];
		NTVERTEX &tgt = Vtx[30+i];
		D3DVALUE vz = (i%2 ? length : 0.0f);
		tgt.x  = src.x*r11 + src.y*r12 + vz*r13 + end1.x;
		tgt.y  = src.x*r21 + src.y*r22 + vz*r23 + end1.y;
		tgt.z  = src.x*r31 + src.y*r32 + vz*r33 + end1.z;
		tgt.nx = src.nx*r11 + src.ny*r12;
		tgt.ny = src.nx*r21 + src.ny*r22;
		tgt.nz = src.nx*r31 + src.ny*r32;
		tgt.tu = (i%2 ? tuscale_track : 0.0f);
		tgt.tv = src.tv;
	}

	SURFHANDLE dummy;
	base->GetGenericTexture (texid, (SURFHANDLE&)dyndata->tex, dummy);

	cpos = minpos, cvel = 1.0;
	D3DVECTOR ofs; ofs.x = ofs.y = 0; ofs.z = minpos;
	SetCabin (30, cabin1, Vtx, end1, ofs);
}

void Train1::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->Vtx;
		dyndata->Vtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

bool Train1::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	switch (grp) {
	case 0:
		nvtx = 30;
		nidx = 78;
		_texid = texid;
		undershadow = false;
		groundshadow = true;
		return true;
	case 1:
		nvtx = 8;
		nidx = 18;
		_texid = texid;
		undershadow = true;
		groundshadow = false;
		return true;
	default:
		return false;
	}
}

void Train1::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	DWORD i;
	WORD iofs = (WORD)idx_ofs;

	switch (grp) {
	case 0:
		cabinvtx = vtx;
		memcpy (vtx, dyndata->Vtx, 30*sizeof(NTVERTEX));
		for (i = 0; i < 78; i++) *idx++ = cabin1_idx[i] + iofs;
		return;
	case 1:
		memcpy (vtx, dyndata->Vtx+30, 8*sizeof(NTVERTEX));
		for (i = 0; i < 18; i++) *idx++ = mrail1_idx[i] + iofs;
		return;
	}
}

bool Train1::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 8;
	nidx = 18;
	return true;
}

void Train1::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	WORD i;

	for (i = 0; i < 6; i++) {
		*idx++ = 0;
		*idx++ = i+2;
		*idx++ = i+1;
	}
	for (i = 0; i < 8; i++) vtx[i].y = 0.0;
	shvtx = vtx;
}

void Train1::Update ()
{
	DWORD i;
	// update cabin position
	bool reset;
	D3DVALUE shift = MoveCabin (cpos, cvel, reset);
	D3DVALUE dx = shift*dir.x, dy = shift*dir.y, dz = shift*dir.z;
	if (reset && ++dyndata->tick > 10000) {
		D3DVECTOR ofs; ofs.x = ofs.y = 0.0f; ofs.z = minpos;
		SetCabin (30, cabin1, cabinvtx, end1, ofs);
		dyndata->tick = 0;
	} else {
		NTVERTEX *cvtx = cabinvtx;
		for (i = 0; i < 30; i++)
			cvtx[i].x += dx, cvtx[i].y += dy, cvtx[i].z += dz;
	}
	// update cabin shadow
	VERTEX_XYZ *_shvtx = shvtx;
	if (_shvtx) 
		for (i = 0; i < 8; i++)
			_shvtx[i].x += dx, _shvtx[i].z += dz;
}

void Train1::UpdateShadow (Vector &fromsun, double az)
{
	static DWORD i, j, nCH, ii[12] = {2,3,4,5,6,7,10,11,12,13,14,15};
	static VECTOR2D proj[12];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	WORD *CHidx;
	NTVERTEX *cvtx = cabinvtx;
	VERTEX_XYZ *vptr = shvtx;

	// project bounding vertices onto y=0
	for (i = 0; i < 12; i++) {
		j = ii[i];
		proj[i].x = cvtx[j].x + dx*cvtx[j].y;
		proj[i].y = cvtx[j].z + dz*cvtx[j].y;
	}
	Find2DConvexHull(12, proj, &nCH, &CHidx);
	// we assume that nCH is 8
	for (i = 0; i < 8; i++) {
		vptr[i].x = proj[CHidx[i]].x;
		vptr[i].z = proj[CHidx[i]].y;
	}
}

// ======================================================================================
// class Train2: hangrail-type train

static NTVERTEX girder_template[12] = {
	{-10,0,0, 0,0,-1,  0.5,0.25},
	{-7,15,0, 0,0,-1,  0,0.25},
	{-5,11,0, 0,0,-1,  0,0.5},
	{7,15,0,  0,0,-1,  0.5,0.25},
	{5,11,0,  0,0,-1,  0.5,0.5},
	{10,0,0,  0,0,-1,  0,0.5},

	{10,0,0,  0,0, 1,  0.5,0.25},
	{7,15,0,  0,0, 1,  0,0.25},
	{5,11,0,  0,0, 1,  0,0.5},
	{-7,15,0, 0,0, 1,  0.5,0.25},
	{-5,11,0, 0,0, 1,  0.5,0.5},
	{-10,0,0, 0,0, 1,  0,0.5}
};

static NTVERTEX support_template[8] = {
	{ 5,0,0,  0, 1,0,  0,0.25},
	{-5,0,0,  0, 1,0,  0,0},
	{ 5,0,1,  0, 1,0,  6,0.25},
	{-5,0,1,  0, 1,0,  6,0},
	{-5,0,0,  0,-1,0,  0,0.25},
	{ 5,0,0,  0,-1,0,  0,0},
	{-5,0,1,  0,-1,0,  6,0.25},
	{ 5,0,1,  0,-1,0,  6,0}
};

static NTVERTEX cabin2[30] = {
	{ 2, 0,0, 0,1,0,              0,0.75},
	{-2, 0,0, 0,1,0,              0,0.5},
	{ 2, 0,5, 0,0.707f,0.707f,    0.34375f,0.75f},
	{-2, 0,5, 0,0.707f,0.707f,    0.34375f,0.5f},
	{ 2,-2,6, 0,0,1,              0.516f,0.75f},
	{-2,-2,6, 0,0,1,              0.516f,0.5f},
	{ 2,-3,5.5, 0,-0.707f,0.707f, 0.625f,0.75f},
	{-2,-3,5.5, 0,-0.707f,0.707f, 0.625f,0.5f},
	{ 2,-3,0, 0,-1,0,             1,0.75},
	{-2,-3,0, 0,-1,0,             1,0.5},
	{ 2,-3,-5.5, 0,-0.707f,-0.707f, 0.625f, 0.75f},
	{-2,-3,-5.5, 0,-0.707f,-0.707f, 0.625f, 0.5f},
	{ 2,-2,-6, 0,0,-1,            0.516f,0.75f},
	{-2,-2,-6, 0,0,-1,            0.516f,0.5f},
	{ 2, 0,-5, 0,0.707f,-0.707f,  0.34375f,0.75f},
	{-2, 0,-5, 0,0.707f,-0.707f,  0.34375f,0.5f},
	{ 2, 0,0, 0,1,0,              0,0.75f},
	{-2, 0,0, 0,1,0,              0,0.5f},

	{ 2,-2,-6,    1,0,0,          0,0.9f},
	{ 2, 0,-5,    1,0,0,          0.1f,0.75f},
	{ 2,-3,-5.5,  1,0,0,          0.05f,1},
	{ 2, 0,5,     1,0,0,          0.9f,0.75f},
	{ 2,-3,5.5,   1,0,0,          0.95f,1},
	{ 2,-2,6,     1,0,0,          1,0.9f},
	{-2,-2, 6,   -1,0,0,          0,0.9f},
	{-2, 0, 5,   -1,0,0,          0.1f,0.75f},
	{-2,-3, 5.5, -1,0,0,          0.05f,1},
	{-2, 0,-5,   -1,0,0,          0.9f,0.75f},
	{-2,-3,-5.5, -1,0,0,          0.95f,1},
	{-2,-2,-6,   -1,0,0,          1,0.9f}
};

Train2::Train2 (const Base *_base): Train (_base)
{
	texid = 0;
	tuscale_track = 1.0f;
	ngrp = 2;
	height = 11;
	dyndata = 0;
}

Train2::~Train2 ()
{
	Deactivate ();
}

int Train2::Read (istream &is)
{
	char cbuf[256], *cp, label[32];

	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "END1"))
			sscanf (cp+4, "%f%f%f", &end1.x, &end1.y, &end1.z);
		else if (!_stricmp (label, "END2"))
			sscanf (cp+4, "%f%f%f", &end2.x, &end2.y, &end2.z);
		else if (!_stricmp (label, "HEIGHT"))
			sscanf (cp+6, "%f", &height);
		else if (!_stricmp (label, "MAXSPEED"))
			sscanf (cp+8, "%f", &maxspeed);
		else if (!_stricmp (label, "SLOWZONE"))
			sscanf (cp+8, "%f", &slowzone);
		else if (!_stricmp (label, "TEX")) {
			sscanf (cp+3, "%s%f", label, &tuscale_track);
			texid = NameToId (label);
		}
	} while (_stricmp (label, "END"));

	Init (end1, end2);
	return 0;
}

void Train2::Activate ()
{
	static double gdist = 200.0; // approx. distance between girders - should be read in
	int i, j, ng;

	if (dyndata) return; // already active
	dyndata = new struct DYNDATA; TRACENEW
	dyndata->ng = ng = (int)(length / gdist) + 1;
	dyndata->rail = new NTVERTEX[(ng+1)*12 + ng*8]; TRACENEW
	dyndata->rshvtx = new VERTEX_XYZ[(ng+1)*2]; TRACENEW
	shvtx = 0;
	dyndata->tick = rand()%10000;

	D3DVALUE dx = (end2.x-end1.x)/(D3DVALUE)ng;
	D3DVALUE dy = (end2.y-end1.y)/(D3DVALUE)ng;
	D3DVALUE dz = (end2.z-end1.z)/(D3DVALUE)ng;

	// rotation matrix
	double ph = atan2 (end2.x-end1.x, end2.z-end1.z);
	D3DVALUE sinth = (end2.y-end1.y)/length, costh = (D3DVALUE)(cos(asin(sinth)));
	D3DVALUE cosph = (D3DVALUE)cos(ph), sinph = (D3DVALUE)sin(ph);
	D3DVALUE r11 =  cosph, r12 = -sinph*sinth, r13 = sinph*costh;
	D3DVALUE r21 =  0.0f,  r22 =  costh,       r23 = sinth;
	D3DVALUE r31 = -sinph, r32 = -cosph*sinth, r33 = cosph*costh;

	// first girder
	for (i = 0; i < 12; i++) {
		// note we don't tilt girders even if p1.y != p2.y
		NTVERTEX &src = girder_template[i];
		NTVERTEX &tgt = dyndata->rail[i];
		D3DVALUE vy = (i%2 ? (i==5 || i==11 ? 0:height+4) : (i==0 || i==6 ? 0:height));
		tgt.x  =  src.x*cosph + src.z*sinph + end1.x;
		tgt.y  =  vy + end1.y;
		tgt.z  = -src.x*sinph + src.z*cosph + end1.z;
		tgt.nx =  src.nx*cosph + src.nz*sinph;
		tgt.ny =  src.ny;
		tgt.nz = -src.nx*sinph + src.nz*cosph;
		tgt.tu =  src.tu;
		tgt.tv =  src.tv;
	}
	// the other girders
	for (i = 1; i <= ng; i++) {
		NTVERTEX *tgt = dyndata->rail+(i*12);
		memcpy (tgt, tgt-12, 12*sizeof(NTVERTEX));
		for (j = 0; j < 12; j++)
			tgt[j].x += dx, tgt[j].y += dy, tgt[j].z += dz;
	}

	// first traverse
	for (i = 0; i < 8; i++) { // rotate and move to start position
		NTVERTEX &src = support_template[i];
		NTVERTEX &tgt = dyndata->rail[(ng+1)*12+i];
		D3DVALUE vz = (i==2 || i==3 || i==6 || i==7 ? length/(D3DVALUE)dyndata->ng : 0.0f);
		tgt.x  = src.x*r11 + vz*r13 + end1.x;
		tgt.y  = src.x*r21 + vz*r23 + end1.y + height;
		tgt.z  = src.x*r31 + vz*r33 + end1.z;
		tgt.nx = src.ny*r12;
		tgt.ny = src.ny*r22;
		tgt.nz = src.ny*r32;
		tgt.tu = src.tu;
		tgt.tv = src.tv;
	}
	// the other traverses
	for (i = 1; i < ng; i++) {
		NTVERTEX *tgt = dyndata->rail+((ng+1)*12+i*8);
		memcpy (tgt, tgt-8, 8*sizeof(NTVERTEX));
		for (j = 0; j < 8; j++)
			tgt[j].x += dx, tgt[j].y += dy, tgt[j].z += dz;
	}

	// rail shadow
	D3DCOLOR shcol = D3DRGBA(0,0,0,0.3);
	for (i = 0; i < (ng+1)*2; i++) {
		dyndata->rshvtx[i].y = 0.0;
		//dyndata->rshvtx[i].col = shcol;
	}

	base->GetGenericTexture (texid, (SURFHANDLE&)dyndata->dtex, (SURFHANDLE&)dyndata->ntex);

	dyndata->cpos[0] = minpos, dyndata->cvel[0] =  1.0;
	dyndata->cpos[1] = maxpos, dyndata->cvel[1] = -1.0;
}

void Train2::Deactivate ()
{
	if (dyndata) {
		delete []dyndata->rail;
		dyndata->rail = NULL;
		delete []dyndata->rshvtx;
		dyndata->rshvtx = NULL;
		delete dyndata;
		dyndata = 0;
	}
}

bool Train2::GetGroupSpec (int grp, DWORD &nvtx, DWORD &nidx, LONGLONG &_texid,
	bool &undershadow, bool &groundshadow)
{
	switch (grp) {
	case 0:
	case 1:
		nvtx = 30;
		nidx = 78;
		_texid = texid;
		undershadow = false;
		groundshadow = true;
		return true;
	default:
		return false;
	}
}

void Train2::ExportGroup (int grp, NTVERTEX *vtx, WORD *idx, DWORD &idx_ofs)
{
	DWORD i;
	WORD iofs = (WORD)idx_ofs;
	D3DVECTOR ofs;

	// rotation matrix
	double ph = atan2 (end2.x-end1.x, end2.z-end1.z);
	D3DVALUE sinth = (end2.y-end1.y)/length, costh = (D3DVALUE)(cos(asin(sinth)));
	D3DVALUE cosph = (D3DVALUE)cos(ph), sinph = (D3DVALUE)sin(ph);
	D3DVALUE r11 =  cosph, r12 = -sinph*sinth, r13 = sinph*costh;
	D3DVALUE r21 =  0.0f,  r22 =  costh,       r23 = sinth;
	D3DVALUE r31 = -sinph, r32 = -cosph*sinth, r33 = cosph*costh;

	switch (grp) {
	case 0:
	case 1: // cabin vertices
		cabinvtx[grp] = vtx;
		for (i = 0; i < 30; i++) {
			NTVERTEX &src = cabin2[i];
			NTVERTEX &tgt = vtx[i];
			tgt.nx = src.nx*r11 + src.ny*r12 + src.nz*r13;
			tgt.ny = src.nx*r21 + src.ny*r22 + src.nz*r23;
			tgt.nz = src.nx*r31 + src.ny*r32 + src.nz*r33;
			tgt.tu = src.tu;
			tgt.tv = src.tv;
		}
		ofs.x = (grp ? -2.5f:2.5f);
		ofs.y = height-1.0f;
		ofs.z = (grp ? maxpos : minpos);
		SetCabin (30, cabin2, vtx, end1, ofs);
		for (i = 0; i < 78; i++) *idx++ = cabin1_idx[i] + iofs;
	}
}

bool Train2::GetShadowSpec (DWORD &nvtx, DWORD &nidx)
{
	nvtx = 16;
	nidx = 36;
	return true;
}

void Train2::ExportShadow (VERTEX_XYZ *vtx, WORD *idx)
{
	WORD i, j;

	for (j = 0; j < 2; j++) {
		for (i = 0; i < 6; i++) {
			*idx++ = j*8;
			*idx++ = j*8+i+2;
			*idx++ = j*8+i+1;
		}
	}
	for (i = 0; i < 16; i++) vtx[i].y = 0.0;
	shvtx = vtx;
}

void Train2::Update ()
{
	DWORD i, j;
	// update cabin position
	bool reset;
	for (j = 0; j < 2; j++) {
		D3DVALUE shift = MoveCabin (dyndata->cpos[j], dyndata->cvel[j], reset);
		D3DVALUE dx = shift*dir.x, dy = shift*dir.y, dz = shift*dir.z;
		if (reset && ++dyndata->tick > 10000) {
			D3DVECTOR ofs;
			ofs.x = (j ? -2.5f:2.5f);
			ofs.y = height-1.0f;
			ofs.z = minpos;
			SetCabin (30, cabin2, cabinvtx[j], end1, ofs);
			dyndata->tick = 0;
		} else {
			NTVERTEX *cvtx = cabinvtx[j];
			for (i = 0; i < 30; i++)
				cvtx[i].x += dx, cvtx[i].y += dy, cvtx[i].z += dz;
		}
		// update cabin shadow
		if (shvtx) {
			VERTEX_XYZ *_shvtx = shvtx + j*8;
			for (i = 0; i < 8; i++)
				_shvtx[i].x += dx, _shvtx[i].z += dz;
		}
	}
}

void Train2::UpdateShadow (Vector &fromsun, double az)
{
	static DWORD i, j, nCH, ii[12] = {2,3,4,5,6,7,10,11,12,13,14,15};
	static VECTOR2D proj[12];
	D3DVALUE dx = (D3DVALUE)fromsun.x, dz = (D3DVALUE)fromsun.z;
	WORD *CHidx;
	NTVERTEX *vtx = cabinvtx[0];
	VERTEX_XYZ *vptr = shvtx;
	D3DVALUE ddx = cabinvtx[1][0].x - vtx[0].x;
	D3DVALUE ddz = cabinvtx[1][0].z - vtx[0].z;

	// project bounding vertices onto y=0
	for (i = 0; i < 12; i++) {
		j = ii[i];
		proj[i].x = vtx[j].x + dx*vtx[j].y;
		proj[i].y = vtx[j].z + dz*vtx[j].y;
	}
	Find2DConvexHull(12, proj, &nCH, &CHidx);
	// we assume that nCH is 8
	for (i = 0; i < 8; i++) {
		vptr[i+8].x = (vptr[i].x = proj[CHidx[i]].x) + ddx;
		vptr[i+8].z = (vptr[i].z = proj[CHidx[i]].y) + ddz;
	}

	// update rail shadow
	ddx = (end2.x-end1.x)/(D3DVALUE)dyndata->ng;
	ddz = (end2.z-end1.z)/(D3DVALUE)dyndata->ng;
	WORD ng = dyndata->ng;
	for (i = 0; i < 2; i++) {
		vptr = dyndata->rshvtx+i;
		vtx = dyndata->rail+(ng+1)*12+i;
		vptr->x = vtx->x + dx*vtx->y;
		vptr->z = vtx->z + dz*vtx->y;
		for (j = 1; j <= ng; j++) {
			(vptr+2)->x = vptr->x + ddx;
			(vptr+2)->z = vptr->z + ddz;
			vptr += 2;
		}
	}
}

void Train2::Render (LPDIRECT3DDEVICE7 dev, bool day)
{
	int i;
	NTVERTEX *vtx = dyndata->rail;
	DWORD ambient;

	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	dev->SetTexture (0, day ? dyndata->dtex : dyndata->ntex);

	if (!day) {
		dev->GetRenderState (D3DRENDERSTATE_AMBIENT, &ambient);
		dev->SetRenderState (D3DRENDERSTATE_AMBIENT, ambient*16);
	}

	// render girders
	for (i = 0; i <= dyndata->ng; i++) {
		dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, vtx, 6, NULL); vtx += 6;
		dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, vtx, 6, NULL); vtx += 6;
	}
	// render traverses
	for (i = 0; i < dyndata->ng; i++) {
		dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, vtx, 4, NULL); vtx += 4;
		dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, vtx, 4, NULL); vtx += 4;
	}

	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	if (!day) dev->SetRenderState (D3DRENDERSTATE_AMBIENT, ambient);
}

void Train2::RenderShadow (LPDIRECT3DDEVICE7 dev)
{
	dev->DrawPrimitive (
		D3DPT_TRIANGLESTRIP, D3DFVF_XYZ /*| D3DFVF_DIFFUSE*/, dyndata->rshvtx, (dyndata->ng+1)*2, NULL);
}

// ==============================================================================
// class SolarPlant (self-aligning solar panel array)

SolarPlant::SolarPlant (const Base *_base): BaseObject (_base)
{
	pos.x = pos.y = pos.z = 0.0f;
	scale = 1.0f;
	rot = 0.0f;
	sepx = sepz = 40.0;
	nrow = 2;
	ncol = 2;
	texid = 0;
	tuscale = tvscale = 1.0f;
	npanel = nrow*ncol;
}

SolarPlant::~SolarPlant ()
{
	Deactivate();
}

int SolarPlant::Read (istream &is)
{
	char cbuf[256], *cp, label[32];
	int i;

	do {
		if (!is.getline (cbuf, 256)) return 1; // premature end of file
		cp = trim_string (cbuf);
		sscanf (cp, "%s", label);
		if (!_stricmp (label, "POS"))
			sscanf (cp+3, "%f%f%f", &pos.x, &pos.y, &pos.z);
		else if (!_stricmp (label, "SCALE"))
			sscanf (cp+5, "%f", &scale);
		else if (!_stricmp (label, "SPACING"))
			sscanf (cp+7, "%f%f", &sepx, &sepz);
		else if (!_stricmp (label, "GRID"))
			sscanf (cp+4, "%d%d", &nrow, &ncol);
		else if (!_stricmp (label, "ROT")) {
			sscanf (cp+3, "%f", &rot);
			rot *= (D3DVALUE)RAD;
		} else if (!_stricmp (label, "TEX")) {
			D3DVALUE su, sv;
			i = sscanf (cp+3, "%s%f%f", label, &su, &sv);
			texid = NameToId (label);
			if (i > 1) tuscale = su;
			if (i > 2) tvscale = sv;
		}
	} while (_stricmp (label, "END"));
	npanel = nrow*ncol;
	return 0;
}

void SolarPlant::Render (LPDIRECT3DDEVICE7 dev, bool)
{
	int i, j;

	// check for flashing panels
	Vector pc, cdir (tmul (base->GRot(), g_camera->GPos()-base->GPos()));
	bool anyflash = false;
	double alpha;
	for (i = 0; i < npanel; i++) {
		pc.Set (cdir.x - ppos[i].x, cdir.y - ppos[i].y, cdir.z - ppos[i].z);
		pc.unify();
		alpha = dotp (pc, nml);
		if (alpha > 0.999) {
			anyflash = flash[i] = true;
			for (j = i*4; j < (i+1)*4; j++) Vtx[j].tu += 0.25;
		}
	}

	// render panels and stands
	dev->SetTexture (0, tex);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	dev->DrawIndexedPrimitive (
		D3DPT_TRIANGLELIST, D3DFVF_VERTEX, Vtx, nVtx, Idx, nIdx, 0);
	if (anyflash)
		for (i = 0; i < npanel; i++)
			if (flash[i]) {
				for (j = i*4; j < (i+1)*4; j++) Vtx[j].tu -= 0.25;
				flash[i] = false;
			}
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
}

void SolarPlant::RenderShadow (LPDIRECT3DDEVICE7 dev)
{
	if (have_shadows)
		dev->DrawIndexedPrimitive (
			D3DPT_TRIANGLELIST, D3DFVF_XYZ /*| D3DFVF_DIFFUSE*/, ShVtx, nShVtx, ShIdx, nShIdx, 0);
}

void SolarPlant::Activate ()
{
	int i, j, idx, idx_ofs, vtx_ofs;
	DWORD n;
	D3DVALUE x, z;
	D3DVALUE crot = (D3DVALUE)cos(rot);
	D3DVALUE srot = (D3DVALUE)sin(rot);
	ppos = new D3DVECTOR[npanel]; TRACENEW
	for (i = idx = 0; i < nrow; i++) {
		x = sepx * (i-0.5f*(nrow-1));
		for (j = 0; j < ncol; j++) {
			z = sepz * (j-0.5f*(ncol-1));
			ppos[idx].x = pos.x + x*crot - z*srot;
			ppos[idx].y = pos.y + scale*10.0f;
			ppos[idx].z = pos.z + x*srot + z*crot;
			idx++;
		}
	}
	Vtx = new NTVERTEX[nVtx = npanel*12]; TRACENEW
	Idx = new WORD[nIdx = npanel*21]; TRACENEW
	flash = new bool[npanel]; TRACENEW

	// mesh for all panels
	idx_ofs = npanel*6, vtx_ofs = npanel*4;
	for (i = 0; i < npanel; i++) {
		flash[i] = false;
		Idx[i*6]             = i*4;
		Idx[i*6+1]           = i*4+1;
		Idx[i*6+2]           = i*4+2;
		Idx[i*6+3]           = i*4+2;
		Idx[i*6+4]           = i*4+3;
		Idx[i*6+5]           = i*4;
		Idx[idx_ofs + i*6]   = vtx_ofs + i*4+2;
		Idx[idx_ofs + i*6+1] = vtx_ofs + i*4+1;
		Idx[idx_ofs + i*6+2] = vtx_ofs + i*4;
		Idx[idx_ofs + i*6+3] = vtx_ofs + i*4;
		Idx[idx_ofs + i*6+4] = vtx_ofs + i*4+3;
		Idx[idx_ofs + i*6+5] = vtx_ofs + i*4+2;

		Vtx[i*4].tu   = 0.0;
		Vtx[i*4].tv   = 0.0;
		Vtx[i*4+1].tu = 0.25;
		Vtx[i*4+1].tv = 0.0;
		Vtx[i*4+2].tu = 0.25;
		Vtx[i*4+2].tv = 0.5;
		Vtx[i*4+3].tu = 0.0;
		Vtx[i*4+3].tv = 0.5;
		Vtx[vtx_ofs+i*4].tu   = 0.0;
		Vtx[vtx_ofs+i*4].tv   = 0.5;
		Vtx[vtx_ofs+i*4+1].tu = 0.25;
		Vtx[vtx_ofs+i*4+1].tv = 0.5;
		Vtx[vtx_ofs+i*4+2].tu = 0.25;
		Vtx[vtx_ofs+i*4+2].tv = 1.0;
		Vtx[vtx_ofs+i*4+3].tu = 0.0;
		Vtx[vtx_ofs+i*4+3].tv = 1.0;
	}
	// the panel stands
	idx_ofs = npanel*12; vtx_ofs = npanel*8;
	D3DVALUE v1 = 2.89f*scale, v2 = 2.5f*scale, v3 = 1.44f*scale;
	for (i = 0; i < npanel; i++) {
		Idx[idx_ofs++] = vtx_ofs;
		Idx[idx_ofs++] = vtx_ofs+1;
		Idx[idx_ofs++] = vtx_ofs+2;
		Idx[idx_ofs++] = vtx_ofs;
		Idx[idx_ofs++] = vtx_ofs+2;
		Idx[idx_ofs++] = vtx_ofs+3;
		Idx[idx_ofs++] = vtx_ofs;
		Idx[idx_ofs++] = vtx_ofs+3;
		Idx[idx_ofs++] = vtx_ofs+1;
		Vtx[vtx_ofs].x = ppos[i].x;           Vtx[vtx_ofs].nx = 0.0;        Vtx[vtx_ofs].tu = 0.375;
		Vtx[vtx_ofs].y = 14.0f*scale;	  	  Vtx[vtx_ofs].ny = 1.0f;       Vtx[vtx_ofs].tv = 0.5f;
		Vtx[vtx_ofs].z = ppos[i].z;           Vtx[vtx_ofs].nz = 0.0;
		Vtx[vtx_ofs+1].x = ppos[i].x + v2;    Vtx[vtx_ofs+1].nx = 0.87f;    Vtx[vtx_ofs+1].tu = 0.25f;
		Vtx[vtx_ofs+1].y = 0.0;               Vtx[vtx_ofs+1].ny = 0.0;      Vtx[vtx_ofs+1].tv = 1.0;
		Vtx[vtx_ofs+1].z = ppos[i].z - v3;    Vtx[vtx_ofs+1].nz = -0.5;
		Vtx[vtx_ofs+2].x = ppos[i].x - v2;    Vtx[vtx_ofs+2].nx = -0.87f;   Vtx[vtx_ofs+2].tu = 0.5f;
		Vtx[vtx_ofs+2].y = 0.0;               Vtx[vtx_ofs+2].ny = 0.0;      Vtx[vtx_ofs+2].tv = 1.0;
		Vtx[vtx_ofs+2].z = ppos[i].z - v3;    Vtx[vtx_ofs+2].nz = -0.5;
		Vtx[vtx_ofs+3].x = ppos[i].x;         Vtx[vtx_ofs+3].nx = 0.0;      Vtx[vtx_ofs+3].tu = 0.375;
		Vtx[vtx_ofs+3].y = 0.0;               Vtx[vtx_ofs+3].ny = 0.0;      Vtx[vtx_ofs+3].tv = 1.0;
		Vtx[vtx_ofs+3].z = ppos[i].z + v1;    Vtx[vtx_ofs+3].nz = 1.0;
		vtx_ofs += 4;
	}
	// shadow mesh
	if (g_pOrbiter->Cfg()->CfgVisualPrm.bShadows) {
		ShVtx = new VERTEX_XYZ[nShVtx = npanel*4]; TRACENEW
		ShIdx = new WORD[nShIdx = npanel*6]; TRACENEW
		Vector4 shadowCol = base->ShadowColor();
		D3DCOLOR shcol = D3DRGBA(shadowCol.x, shadowCol.y, shadowCol.z, shadowCol.w);
		for (n = 0; n < nShVtx; n++) {
			ShVtx[n].y = 0.0f;
			//ShVtx[n].col = shcol;
		}
		for (i = idx_ofs = 0; i < npanel; i++) {
			ShIdx[idx_ofs++] = i*4;
			ShIdx[idx_ofs++] = i*4+1;
			ShIdx[idx_ofs++] = i*4+2;
			ShIdx[idx_ofs++] = i*4+2;
			ShIdx[idx_ofs++] = i*4+3;
			ShIdx[idx_ofs++] = i*4;
		}
	}
	have_shadows = false; // don't use before updated
	SURFHANDLE dummy;
	base->GetGenericTexture (texid, (SURFHANDLE&)tex, dummy);
	updT = -1.0; // force update
}

void SolarPlant::Deactivate ()
{
	delete []ppos;
	ppos = NULL;
	delete []Vtx;
	Vtx = NULL;
	delete []Idx;
	Idx = NULL;
	delete []flash;
	flash = NULL;
	if (g_pOrbiter->Cfg()->CfgVisualPrm.bShadows) {
		delete []ShVtx;
		ShVtx = NULL;
		delete []ShIdx;
		ShIdx = NULL;
	}
}

void SolarPlant::Update ()
{
	if (td.SimT1 < updT) return;

	// panel orientation
	nml = *base->SunDirectionBuffered();
	if (nml.y < 0.0) {
		nml.y = 0.0;
		nml.unify();
	}

	int i, vtx_ofs = npanel*4;
	D3DVALUE dx = 8.0f*scale, dz = 4.0f*scale;
	double tht = acos (nml.y);         // tilt angle
	double phi = atan2 (nml.z, nml.x); // rotation angle
	D3DVALUE ctht = (D3DVALUE)cos(tht), stht = (D3DVALUE)sin(tht);
	D3DVALUE cphi = (D3DVALUE)cos(phi), sphi = (D3DVALUE)sin(phi);
	// rotation matrix
	D3DVALUE r11 = cphi*ctht, r12 = cphi*stht, r13 = -sphi;
	D3DVALUE r21 = -stht,     r22 = ctht,      r23 = 0.0f;
	D3DVALUE r31 = sphi*ctht, r32 = sphi*stht, r33 = cphi;

	for (i = 0; i < npanel; i++) { // rotate panels into sun
		Vtx[i*4].x   = Vtx[vtx_ofs+i*4].x   = ppos[i].x - r11*dx - r13*dz;
		Vtx[i*4].y   = Vtx[vtx_ofs+i*4].y   = ppos[i].y - r21*dx;
		Vtx[i*4].z   = Vtx[vtx_ofs+i*4].z   = ppos[i].z - r31*dx - r33*dz;
		Vtx[i*4+1].x = Vtx[vtx_ofs+i*4+1].x = ppos[i].x - r11*dx + r13*dz;
		Vtx[i*4+1].y = Vtx[vtx_ofs+i*4+1].y = ppos[i].y - r21*dx;
		Vtx[i*4+1].z = Vtx[vtx_ofs+i*4+1].z = ppos[i].z - r31*dx + r33*dz;
		Vtx[i*4+2].x = Vtx[vtx_ofs+i*4+2].x = ppos[i].x + r11*dx + r13*dz;
		Vtx[i*4+2].y = Vtx[vtx_ofs+i*4+2].y = ppos[i].y + r21*dx;
		Vtx[i*4+2].z = Vtx[vtx_ofs+i*4+2].z = ppos[i].z + r31*dx + r33*dz;
		Vtx[i*4+3].x = Vtx[vtx_ofs+i*4+3].x = ppos[i].x + r11*dx - r13*dz;
		Vtx[i*4+3].y = Vtx[vtx_ofs+i*4+3].y = ppos[i].y + r21*dx;
		Vtx[i*4+3].z = Vtx[vtx_ofs+i*4+3].z = ppos[i].z + r31*dx - r33*dz;
		Vtx[i*4].nx = Vtx[i*4+1].nx = Vtx[i*4+2].nx = Vtx[i*4+3].nx = (D3DVALUE)nml.x;
		Vtx[i*4].ny = Vtx[i*4+1].ny = Vtx[i*4+2].ny = Vtx[i*4+3].ny = (D3DVALUE)nml.y;
		Vtx[i*4].nz = Vtx[i*4+1].nz = Vtx[i*4+2].nz = Vtx[i*4+3].nz = (D3DVALUE)nml.z;
		Vtx[vtx_ofs+i*4].nx = Vtx[vtx_ofs+i*4+1].nx = Vtx[vtx_ofs+i*4+2].nx = Vtx[vtx_ofs+i*4+3].nx = -(D3DVALUE)nml.x;
		Vtx[vtx_ofs+i*4].ny = Vtx[vtx_ofs+i*4+1].ny = Vtx[vtx_ofs+i*4+2].ny = Vtx[vtx_ofs+i*4+3].ny = -(D3DVALUE)nml.x;
		Vtx[vtx_ofs+i*4].nz = Vtx[vtx_ofs+i*4+1].nz = Vtx[vtx_ofs+i*4+2].nz = Vtx[vtx_ofs+i*4+3].nz = -(D3DVALUE)nml.x;
	}
	updT = td.SimT1 + 60.0;
}

void SolarPlant::UpdateShadow (Vector &fromsun, double az)
{
	if (!g_pOrbiter->Cfg()->CfgVisualPrm.bShadows) return;
	if (have_shadows = (nml.y > 0.2)) {
		int i, j;
		double a;
		D3DVALUE anx, anz;
		for (i = 0; i < 4; i++) {
			a = Vtx[i].y/nml.y;
			anx = (D3DVALUE)(a*nml.x), anz = (D3DVALUE)(a*nml.z);
			for (j = 0; j < npanel; j++) {
				ShVtx[j*4+i].x = Vtx[j*4+i].x - anx;
				ShVtx[j*4+i].z = Vtx[j*4+i].z - anz;
			}
		}
	}
}