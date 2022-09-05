// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include "OrbiterAPI.h"
#include "QTree.h"
#include "ToolBox.h"
#include "gcCoreAPI.h"
#include <stack>

// ==================================================================================
//
QTree::QTree(gcCore2 *pC, OBJHANDLE hP)
{
	memset(pChild, 0, sizeof(pChild));
	pCore = pC;
	pParent = NULL;
	pElev = NULL;
	hMask = NULL;
	hTex = NULL;
	hMgr = pCore->GetPlanetManager(hP);
	Bounds = { -PI, PI05, PI, -PI05 };
	clng = (Bounds.left + Bounds.right) * 0.5;
	clat = (Bounds.top + Bounds.bottom) * 0.5;
	nlng = nlat = 1;
	ilng = ilat = 0;
	level = -1;		// Root is at level -1
	quarant = -1;
}


// ==================================================================================
//
QTree::QTree(QTree *pPar, int q)
{
	assert(pPar);
	memset(pChild, 0, sizeof(pChild));
	pParent = pPar;
	pCore = pPar->pCore;
	hMgr = pPar->hMgr;
	hTex = NULL;
	pElev = NULL;
	hMask = NULL;

	quarant = q;
	level = pParent->level + 1;
	
	if (level == 0) {

		nlng = 2; nlat = 1;	ilat = 0;

		switch (q) {
		case 0: Bounds = { -PI, PI05, 0, -PI05 }; ilng = 0; break;
		case 1: Bounds = { 0, PI05, PI, -PI05 }; ilng = 1; break;
		default: assert(false); break;
		}

		clng = (Bounds.left + Bounds.right) * 0.5;
		clat = (Bounds.top + Bounds.bottom) * 0.5;

		return;
	}
	
	nlng = pPar->nlng * 2;
	nlat = pPar->nlat * 2;
	ilng = pPar->ilng * 2;
	ilat = pPar->ilat * 2;

	if (q & 1) ilng += 1;
	if (q & 2) ilat += 1;

	switch (q) {
	case 0: Bounds = { pPar->Bounds.left, pPar->Bounds.top, pPar->clng, pPar->clat }; break;
	case 1: Bounds = { pPar->clng, pPar->Bounds.top, pPar->Bounds.right, pPar->clat }; break;
	case 2: Bounds = { pPar->Bounds.left, pPar->clat, pPar->clng, pPar->Bounds.bottom }; break;
	case 3: Bounds = { pPar->clng, pPar->clat, pPar->Bounds.right, pPar->Bounds.bottom }; break;
	default: assert(false); break;
	}

	clng = (Bounds.left + Bounds.right) * 0.5;
	clat = (Bounds.top + Bounds.bottom) * 0.5;
}

// ==================================================================================
//
QTree::~QTree()
{
	if (hTex) oapiReleaseTexture(hTex);
	if (pElev) delete[] pElev;
	for (int i = 0; i < 4; i++) if (pChild[i]) delete pChild[i];
}


// ==================================================================================
//
SubTex QTree::GetSubTexRange(int flags)
{
	SubTex st;
	st.range = { 0, 0, 512, 512 };
	
	QTree *pP = this;
	stack<QTree *> buf;	// Need to revert the order

	while (pP) {
		if (pCore->HasTileData(hMgr, pP->ilng, pP->ilat, pP->level, flags)) {	st.pNode = pP; break; }
		buf.push(pP);
		pP = pP->pParent;
	}

	while (buf.size()) {
		int q = buf.top()->quarant;
		buf.pop();
		if (q & 1) st.range.left += (st.range.right - st.range.left) / 2;
		else	   st.range.right = (st.range.right + st.range.left) / 2;
		if (q & 2) st.range.top += (st.range.bottom - st.range.top) / 2;
		else	   st.range.bottom = (st.range.bottom + st.range.top) / 2;
	}

	return st;
}

// ==================================================================================
//
SURFHANDLE QTree::GetTexture(int flags)
{	
	assert(flags != 0);

	if (flags & gcTileFlags::TEXTURE) {
		if (!hTex) hTex = pCore->SeekTileTexture(hMgr, ilng, ilat, level, flags);
		return hTex;
	}
	if (flags & gcTileFlags::MASK) {
		if (!hMask) hMask = pCore->SeekTileTexture(hMgr, ilng, ilat, level, flags);
		return hMask;
	}
	return NULL;
}

// ==================================================================================
//
INT16 * QTree::GetElevation()
{
	gcCore::ElevInfo ei;
	if (!pElev) pElev = (INT16 *)pCore->SeekTileElevation(hMgr, ilng, ilat, level, 0, &ei);
	return pElev;
}

// ==================================================================================
//
bool QTree::HasOwnTex(int flags)
{
	assert (flags != 0);
	return pCore->HasTileData(hMgr, ilng, ilat, level, flags);
}

// ==================================================================================
//
QTree *	QTree::FindNode(double lng, double lat, int lvl)
{
	if (level == lvl) return this;
	return GetNextNode(lng, lat)->FindNode(lng, lat, lvl);
}


// ==================================================================================
//
QTree *	QTree::GetNextNode(double lng, double lat)
{
	if (level == -1) {
		if (lng < 0) {
			if (!pChild[0]) pChild[0] = new QTree(this, 0);
			return pChild[0];
		}
		else {
			if (!pChild[1]) pChild[1] = new QTree(this, 1);
			return pChild[1];
		}
	}
	else {
		DWORD q = 0;
		if (lng > clng) q |= 1;
		if (lat < clat) q |= 2;
		if (!pChild[q]) pChild[q] = new QTree(this, q);
		return pChild[q];
	}
}


// ==================================================================================
//
QTree *	QTree::GetChild(int idx)
{
	if (level == -1) {
			if (!pChild[idx]) pChild[idx] = new QTree(this, idx);
			return pChild[idx];
	}
	else {
		if (!pChild[idx]) pChild[idx] = new QTree(this, idx);
		return pChild[idx];
	}
}


// ==================================================================================
//
QTree * QTree::GetTextureOwner()
{
	int flags = gcTileFlags::TREE | gcTileFlags::TEXTURE | gcTileFlags::CACHE;
	QTree *pP = this;
	while (pP) {
		if (pCore->HasTileData(hMgr, pP->ilng, pP->ilat, pP->level, flags)) return pP;
		pP = pP->pParent;
	}
	return NULL;
}


// ==================================================================================
//
QTree * QTree::HighestOwn(int flags, double lng, double lat)
{
	if (level > 21) return this; // Just in case of an error

	bool bHas = false;
	QTree *pNode = NULL;

	// Get a child node from this location
	pNode = GetNextNode(lng, lat);

	// Does the next node downwards has own texture
	bHas = pCore->HasTileData(hMgr, pNode->ilng, pNode->ilat, pNode->level, flags);
	
	if (bHas) return pNode->HighestOwn(flags, lng, lat);	// Go to next level down in the QTree
	
	if (level < 0) return NULL; // We got no data at all

	return this; // this one has the highest resolution texture/elevation
}


// ==================================================================================
//
bool QTree::Intersect(DRECT q, DRECT *s, DRECT *d) const
{
	if (q.left >= Bounds.right) return false;
	if (q.right <= Bounds.left) return false;
	if (q.top <= Bounds.bottom) return false;
	if (q.bottom >= Bounds.top) return false;

	double tw = fabs(Bounds.left - Bounds.right);
	double th = fabs(Bounds.top - Bounds.bottom);
	double ow = fabs(q.left - q.right);
	double oh = fabs(q.top - q.bottom);

	if (ow > PI) ow = PI2 - ow;
	if (tw > PI) tw = PI2 - tw;

	double l = max(Bounds.left, q.left);
	double t = min(Bounds.top, q.top);
	double r = min(Bounds.right, q.right);
	double b = max(Bounds.bottom, q.bottom);

	s->left = (l - q.left) / ow;
	s->top = (q.top - t) / oh;
	s->right = (r - q.left) / ow;
	s->bottom = (q.top - b) / oh;

	d->left = (l - Bounds.left) / tw;
	d->top = (Bounds.top - t) / th;
	d->right = (r - Bounds.left) / tw;
	d->bottom = (Bounds.top - b) / th;

	return true;
}


// ==================================================================================
//
bool QTree::MapRect(DRECT OvlRect, SURFHANDLE hOvlSrf, RECT &src, RECT &tgt)
{
	DRECT dsrc, dtgt;
	if (Intersect(OvlRect, &dsrc, &dtgt)) {

		gcCore::SurfaceSpecs sp;
		pCore->GetSurfaceSpecs(hOvlSrf, &sp, sizeof(gcCore::SurfaceSpecs));

		src.left = LONG(round(dsrc.left * double(sp.Width)));
		src.top = LONG(round(dsrc.top * double(sp.Height)));
		src.right = LONG(round(dsrc.right * double(sp.Width)));
		src.bottom = LONG(round(dsrc.bottom * double(sp.Height)));

		tgt.left = LONG(round(dtgt.left * 512.0));
		tgt.top = LONG(round(dtgt.top * 512.0));
		tgt.right = LONG(round(dtgt.right * 512.0));
		tgt.bottom = LONG(round(dtgt.bottom * 512.0));
		return true;
	}
	return false;
}


// ==================================================================================
//
int QTree::SaveTile(int flags, SURFHANDLE hSurf, SURFHANDLE hTemp, DRECT bounds, int maxlvl, float alpha)
{
	if (level > maxlvl && maxlvl != -1) return 0;

	RECT src = { 0,0,0,0 };
	RECT tgt = { 0,0,0,0 };
	DRECT dsrc;
	DRECT dtgt;

	// Check if intersect with overlay and get a sub rectangle
	//
	if (Intersect(bounds, &dsrc, &dtgt)) {

		gcCore::SurfaceSpecs sp;
		pCore->GetSurfaceSpecs(hSurf, &sp, sizeof(gcCore::SurfaceSpecs));
		
		src.left = LONG(round(dsrc.left * double(sp.Width)));
		src.top = LONG(round(dsrc.top * double(sp.Height)));
		src.right = LONG(round(dsrc.right * double(sp.Width)));
		src.bottom = LONG(round(dsrc.bottom * double(sp.Height)));

		tgt.left = LONG(round(dtgt.left * 512.0));
		tgt.top = LONG(round(dtgt.top * 512.0));
		tgt.right = LONG(round(dtgt.right * 512.0));
		tgt.bottom = LONG(round(dtgt.bottom * 512.0));
	}
	else return 0;

	char dir[8];
	if (flags & gcTileFlags::TEXTURE) strcpy_s(dir, 8, "Surf");
	if (flags & gcTileFlags::MASK) strcpy_s(dir, 8, "Mask");
	

	char test[MAX_PATH];
	char name[MAX_PATH];

	// ---------------------------------------------------------

	DWORD fa = GetFileAttributesA("TerrainToolBox");
	if (fa == INVALID_FILE_ATTRIBUTES) CreateDirectoryA("TerrainToolBox", NULL);
	else if ((fa & FILE_ATTRIBUTE_DIRECTORY) == 0) return -1;
	
	// ---------------------------------------------------------

	sprintf_s(name, 63, "TerrainToolBox\\%s", dir);
	fa = GetFileAttributesA(name);
	if (fa == INVALID_FILE_ATTRIBUTES) CreateDirectoryA(name, NULL);
	else if ((fa & FILE_ATTRIBUTE_DIRECTORY) == 0) return -2;

	// ---------------------------------------------------------

	sprintf_s(name, 63, "TerrainToolBox\\%s\\%02d", dir, level + 4);
	fa = GetFileAttributesA(name);
	if (fa == INVALID_FILE_ATTRIBUTES) CreateDirectoryA(name, NULL);
	else if ((fa & FILE_ATTRIBUTE_DIRECTORY) == 0) return -3;

	// ---------------------------------------------------------

	sprintf_s(name, 63, "TerrainToolBox\\%s\\%02d\\%06d", dir, level +4, ilat);
	fa = GetFileAttributesA(name);
	if (fa == INVALID_FILE_ATTRIBUTES) CreateDirectoryA(name, NULL);
	else if ((fa & FILE_ATTRIBUTE_DIRECTORY) == 0) return -4;


	//----------------------------------------------------------

	sprintf_s(name, 63, "TerrainToolBox\\%s\\%02d\\%06d\\%06d.dds", dir, level + 4, ilat, ilng);

	Sketchpad *pSkp = oapiGetSketchpad(hTemp);

	SubTex st; memset(&st, 0, sizeof(SubTex));

	if (pSkp) 
	{
		pSkp->SetBlendState(Sketchpad::BlendState::COPY);

		st = GetSubTexRange(flags); // Find a parent with data and give a "this" tile rect into it.

		if (st.pNode) {
			SURFHANDLE hTex = st.pNode->GetTexture(flags);
			if (hTex) {
				RECT t = { 0, 0, 512, 512 };
				pSkp->StretchRect(hTex, &st.range, &t); // Copy parent data into this tile
			}
		}

		pSkp->StretchRect(hSurf, &src, &tgt);
		
		oapiReleaseSketchpad(pSkp);
	
		SURFHANDLE hFile = pCore->CompressSurface(hTemp, OAPISURFACE_PF_DXT1 | OAPISURFACE_MIPMAPS | OAPISURFACE_SYSMEM);
	
		if (!hFile) {
			oapiWriteLogV("Failed to compress a file");
			return -7;
		}

		if (!pCore->SaveSurface(name, hFile))	{
			oapiWriteLogV("Failed to create a file [%s]",name);
			return -6;
		}

		oapiReleaseTexture(hFile);
	}
	else {
		return -5;
	}

	// ---------------------------------------------------------

	int sw = src.right - src.left;
	int sh = src.bottom - src.top;
	int tw = tgt.right - tgt.left;
	int th = tgt.bottom - tgt.top;

	int sl = 0;
	if (st.pNode) sl = st.pNode->level + 4;

	sprintf_s(test, MAX_PATH, "%s\\%02d\\%06d\\%06d.dds", dir, level + 4, ilat, ilng);
	oapiWriteLogV("BAKING.. Name = %s,  Tgt(x=%d, y=%d, w=%d, h=%d) Src(x=%d, y=%d, w=%d, h=%d) SubTexLvl=%d", test, tgt.left, tgt.top, tw, th, src.left, src.top, sw, sh, sl);

	return 0;
}

	
