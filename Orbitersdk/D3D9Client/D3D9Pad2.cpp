
// =================================================================================================================================
//
// Copyright (C) 2012-2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#include "D3D9Pad.h"
#include "D3D9TextMgr.h"
#include "D3D9Surface.h"
#include "Scene.h"
#include "Mesh.h"
#include <d3dx9.h>

// ===============================================================================================
//
void D3D9Pad::GetRenderSurfaceSize(LPSIZE size)
{
	size->cx = tgt_desc.Width;
	size->cy = tgt_desc.Height;
}


// ===============================================================================================
//
void D3D9Pad::QuickPen(DWORD color, float width, DWORD style)
{
	cpen = NULL;
	QPen.bEnabled = true;
	QPen.style = style;
	QPen.width = width;
	pencolor = SkpColor(color);
	bPenChange = true;
}


// ===============================================================================================
//
void D3D9Pad::QuickBrush(DWORD color)
{
	cbrush = NULL;
	QBrush.bEnabled = true;
	brushcolor = SkpColor(color);
	bPenChange = true;
}


// ===============================================================================================
//
void D3D9Pad::AddRectIdx(WORD aV)
{
	Idx[iI++] = aV;
	Idx[iI++] = aV + 1;
	Idx[iI++] = aV + 2;
	Idx[iI++] = aV;
	Idx[iI++] = aV + 2;
	Idx[iI++] = aV + 3;
}


// ===============================================================================================
//
void D3D9Pad::CheckRect(SURFHANDLE hSrc, LPRECT *s)
{
	*s = &src;
	src.left = 0;
	src.top = 0;
	src.right = SURFACE(hSrc)->GetWidth();
	src.bottom = SURFACE(hSrc)->GetHeight();
}


// ===============================================================================================
//
void D3D9Pad::CopyRect(SURFHANDLE hSrc, LPRECT s, int tx, int ty)
{
	TexChange(hSrc);

	if (!s) CheckRect(hSrc, &s);

	int h = s->bottom - s->top;
	int w = s->right - s->left;

	AddRectIdx(vI);

	SkpVtxII(Vtx[vI++], tx, ty, s->left, s->top);
	SkpVtxII(Vtx[vI++], tx, ty + h, s->left, s->bottom);
	SkpVtxII(Vtx[vI++], tx + w, ty + h, s->right, s->bottom);
	SkpVtxII(Vtx[vI++], tx + w, ty, s->right, s->top);

	Vtx[vI - 1].fnc = SKPSW_TEXTURE;
	Vtx[vI - 2].fnc = SKPSW_TEXTURE;
	Vtx[vI - 3].fnc = SKPSW_TEXTURE;
	Vtx[vI - 4].fnc = SKPSW_TEXTURE;
}


// ===============================================================================================
//
void D3D9Pad::StretchRect(SURFHANDLE hSrc, LPRECT s, LPRECT t)
{
	TexChange(hSrc);

	if (!s) CheckRect(hSrc, &s);

	AddRectIdx(vI);

	SkpVtxII(Vtx[vI++], t->left, t->top, s->left, s->top);
	SkpVtxII(Vtx[vI++], t->left, t->bottom, s->left, s->bottom);
	SkpVtxII(Vtx[vI++], t->right, t->bottom, s->right, s->bottom);
	SkpVtxII(Vtx[vI++], t->right, t->top, s->right, s->top);

	Vtx[vI - 1].fnc = SKPSW_TEXTURE;
	Vtx[vI - 2].fnc = SKPSW_TEXTURE;
	Vtx[vI - 3].fnc = SKPSW_TEXTURE;
	Vtx[vI - 4].fnc = SKPSW_TEXTURE;
}


// ===============================================================================================
//
void D3D9Pad::RotateRect(SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, float angle, float sw, float sh)
{
	TexChange(hSrc);

	if (!s) CheckRect(hSrc, &s);

	float w = float(s->right - s->left) * sw;
	float h = float(s->bottom - s->top) * sh;

	float san = sin(angle) * 0.5f;
	float can = cos(angle) * 0.5f;

	float ax = float(tcx) + (-w * can + h * san);
	float ay = float(tcy) + (-w * san - h * can);

	float bx = float(tcx) + (-w * can - h * san);
	float by = float(tcy) + (-w * san + h * can);

	float cx = float(tcx) + (+w * can - h * san);
	float cy = float(tcy) + (+w * san + h * can);

	float dx = float(tcx) + (+w * can + h * san);
	float dy = float(tcy) + (+w * san - h * can);

	AddRectIdx(vI);

	SkpVtxFI(Vtx[vI++], ax, ay, s->left, s->top);
	SkpVtxFI(Vtx[vI++], bx, by, s->left, s->bottom);
	SkpVtxFI(Vtx[vI++], cx, cy, s->right, s->bottom);
	SkpVtxFI(Vtx[vI++], dx, dy, s->right, s->top);

	Vtx[vI - 1].fnc = SKPSW_TEXTURE;
	Vtx[vI - 2].fnc = SKPSW_TEXTURE;
	Vtx[vI - 3].fnc = SKPSW_TEXTURE;
	Vtx[vI - 4].fnc = SKPSW_TEXTURE;
}


// ===============================================================================================
//
void D3D9Pad::ColorKey(SURFHANDLE hSrc, LPRECT s, int tx, int ty)
{
	TexChange(hSrc);

	if (!s) CheckRect(hSrc, &s);

	int h = s->bottom - s->top;
	int w = s->right - s->left;

	AddRectIdx(vI);

	SkpVtxII(Vtx[vI++], tx, ty, s->left, s->top);
	SkpVtxII(Vtx[vI++], tx, ty + h, s->left, s->bottom);
	SkpVtxII(Vtx[vI++], tx + w, ty + h, s->right, s->bottom);
	SkpVtxII(Vtx[vI++], tx + w, ty, s->right, s->top);

	DWORD f = SKPSW_TEXTURE | SKPSW_COLORKEY;

	Vtx[vI - 1].fnc = f;
	Vtx[vI - 2].fnc = f;
	Vtx[vI - 3].fnc = f;
	Vtx[vI - 4].fnc = f;
}


// ===============================================================================================
//
void D3D9Pad::TextEx(float x, float y, const char *str, float scale, float angle)
{
	if (cfont == NULL) return;

	D3D9Text *pText = static_cast<D3D9PadFont *>(cfont)->pFont;

	switch (tah) {
		default:
		case LEFT:   pText->SetTextHAlign(0); break;
		case CENTER: pText->SetTextHAlign(1); break;
		case RIGHT:  pText->SetTextHAlign(2); break;
	}

	switch (tav) {
		default:
		case TOP:      pText->SetTextVAlign(0); break;
		case BASELINE: pText->SetTextVAlign(1); break;
		case BOTTOM:   pText->SetTextVAlign(2); break;
	}

	pText->SetRotation(angle);
	pText->SetScaling(scale);
	pText->PrintSkp(this, x - 1.0f, y - 1.0f, str, (bkmode == OPAQUE));
}


// ===============================================================================================
//
void D3D9Pad::ClipRect(LPRECT clip)
{
	// Flush all out before a state change
	FlushPrimitives();

	if (clip) {
		pDev->SetScissorRect(clip);
		pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 1);
	}
	else {
		pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 0);
	}
}


// ===============================================================================================
//
void D3D9Pad::Clipper(int idx, const VECTOR3 *uDir, double cos_angle, double dist)
{
	if (idx < 0) idx = 0;
	if (idx > 1) idx = 1;

	if (uDir) {
		ClipData[idx].uDir = D3DXVEC(*uDir);
		ClipData[idx].ca = float(cos_angle);
		ClipData[idx].dst = float(dist);
		ClipData[idx].bEnable = true;
	}
	else {
		ClipData[idx].uDir = D3DXVECTOR3(0,0,1);
		ClipData[idx].ca = 2.0f;
		ClipData[idx].dst = 0.0f;
		ClipData[idx].bEnable = false;
	}
	
	InitClipping();
}


// ===============================================================================================
//
void D3D9Pad::InitClipping()
{
	FlushPrimitives();

	if (ClipData[0].bEnable || ClipData[1].bEnable) {
		HR(FX->SetValue(ePos,  &ClipData[0].uDir, sizeof(D3DXVECTOR3)));
		HR(FX->SetValue(ePos2, &ClipData[1].uDir, sizeof(D3DXVECTOR3)));
		HR(FX->SetValue(eCov, &D3DXVECTOR4(ClipData[0].ca, ClipData[0].dst, ClipData[1].ca, ClipData[1].dst), sizeof(D3DXVECTOR4)));
		HR(FX->SetBool(eCovEn, true));
	}
	else {
		HR(FX->SetBool(eCovEn, false));
	}
}


// ===============================================================================================
//
void D3D9Pad::DepthEnable(bool bEnable)
{

}


// ===============================================================================================
//
FMATRIX4 *D3D9Pad::ViewMatrix()
{
	return (FMATRIX4*)&mV;
}


// ===============================================================================================
//
FMATRIX4 *D3D9Pad::ProjectionMatrix()
{
	return (FMATRIX4*)&mP;
}


// ===============================================================================================
//
const FMATRIX4 *D3D9Pad::GetViewProjectionMatrix()
{
	D3DXMatrixMultiply(&mVP, &mV, &mP);
	return (const FMATRIX4 *)&mVP;
}


// ===============================================================================================
//
void D3D9Pad::SetViewMode(SkpView mode)
{
	vmode = mode;
	EndDrawing();
}


// ===============================================================================================
//
LPD3DXMATRIX D3D9Pad::WorldMatrix()
{
	bViewChange = true;
	return &mW;
}


// ===============================================================================================
//
void D3D9Pad::SetWorldTransform2D(float scale, float rot, IVECTOR2 *c, IVECTOR2 *t)
{
	D3DXVECTOR2 ctr = D3DXVECTOR2(0, 0);
	D3DXVECTOR2 trl = D3DXVECTOR2(0, 0);

	if (c) ctr = D3DXVECTOR2(float(c->x), float(c->y));
	if (t) trl = D3DXVECTOR2(float(t->x), float(t->y));

	D3DXMatrixAffineTransformation2D(&mW, scale, &ctr, rot, &trl);

	bViewChange = true;
}


// ===============================================================================================
//
void D3D9Pad::SetWorldTransform(const FMATRIX4 *pWT)
{
	if (pWT) memcpy(mW, pWT, sizeof(FMATRIX4));
	else D3DXMatrixIdentity(&mW);

	bViewChange = true;
}


// ===============================================================================================
//
void D3D9Pad::SetGlobalLineScale(float width, float pat)
{
	linescale = width;
	pattern = pat;
	bPenChange = true;
}


// ===============================================================================================
//
void D3D9Pad::TexChangeNative(LPDIRECT3DTEXTURE9 hNew)
{
//	float srw, srh;

	Flush(SKPTECH_BLIT);

	if (hNew == hPrevSrc) return;

	FlushPrimitives();

	if (hNew) {
		HR(FX->SetTexture(eTex0, hNew));
	}
//	else {
//		srw = 1.0f;
//		srh = 1.0f;
//	}

	HR(FX->SetVector(eSize, &D3DXVECTOR4(1, 1, 1, 1)));
	HR(FX->SetValue(eKey, &D3DXCOLOR(0,0,0,0), sizeof(D3DXCOLOR)));
	HR(FX->SetBool(eTexEn, (hNew != NULL)));
	HR(FX->SetBool(eKeyEn, false));
	HR(FX->SetBool(eWide, false));
	HR(FX->CommitChanges());

	hPrevSrc = hNew;
}


// ===============================================================================================
//
void D3D9Pad::TexChange(SURFHANDLE hNew)
{
	float srw, srh;
	static bool bOnce = true;

	Flush(SKPTECH_BLIT);

	if (hNew == hPrevSrc) return;

	FlushPrimitives();

	if (!SURFACE(hNew)->IsTexture() && bOnce) {
		LogErr("Sketchpad2: Source is not a texture");
		bOnce = false;
	}

	if (hNew) {
		srw = 1.0f / float(SURFACE(hNew)->desc.Width);
		srh = 1.0f / float(SURFACE(hNew)->desc.Height);

		HR(FX->SetTexture(eTex0, SURFACE(hNew)->GetTexture()));
		HR(FX->SetBool(eKeyEn, (SURFACE(hNew)->IsColorKeyEnabled())));
	}
	else {
		srw = 1.0f;
		srh = 1.0f;
	}

	HR(FX->SetVector(eSize, &D3DXVECTOR4(srw, srh, 1, 1)));
	HR(FX->SetValue(eKey, &SURFACE(hNew)->ClrKey, sizeof(D3DXCOLOR)));
	HR(FX->SetBool(eTexEn, (hNew != NULL)));
	HR(FX->SetBool(eWide, false));
	HR(FX->CommitChanges());

	hPrevSrc = hNew;
}


// ===============================================================================================
//
void D3D9Pad::BeginDraw()
{
	bViewChange = true;
	bPenChange = true;

	UINT numPasses;
	assert(FX->SetTechnique(eSketch)==S_OK);
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	if (vmode == ORTHO) {
		HR(FX->BeginPass(0));
	}
	else {
		HR(FX->BeginPass(1));
	}
	HR(pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE));
	HR(pDev->SetVertexDeclaration(pSketchpadDecl));
}


// ===============================================================================================
//
void D3D9Pad::EndDraw()
{
	HR(pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW));
	HR(FX->EndPass());
	HR(FX->End());
	hPrevSrc = NULL;
}


// ===============================================================================================
//
void D3D9Pad::BeginMesh()
{
	bViewChange = true;
	bPenChange = true;

	hOldMesh = NULL;

	UINT num;
	pDev->SetVertexDeclaration(pNTVertexDecl);
	HR(FX->SetTechnique(eDrawMesh));
	HR(FX->Begin(&num, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
}


// ===============================================================================================
//
void D3D9Pad::EndMesh()
{
	HR(FX->EndPass());
	HR(FX->End());
}


// ===============================================================================================
//
void D3D9Pad::EndDrawing(bool bFlush)
{
	if (CurrentTech == SKPTECH_MESH) EndMesh();

	if (CurrentTech == SKPTECH_DRAW) {
		if (bFlush) FlushPrimitives();
		EndDraw();
	}

	if (CurrentTech == SKPTECH_BLIT) {
		if (bFlush) FlushPrimitives();
		EndDraw();
	}

	if (CurrentTech == SKPTECH_POLY) {
		EndDraw();
	}

	hOldMesh = NULL;
	CurrentTech = 0;
}


// ===============================================================================================
//
void D3D9Pad::FlushPrimitives()
{
	if (iI > 0) {
		
		hOldMesh = NULL;

		if (bTriangles) {
			assert(pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, vI, iI / 3, Idx, D3DFMT_INDEX16, Vtx, sizeof(SkpVtx)) == S_OK);
		}
		else {
			assert(pDev->DrawIndexedPrimitiveUP(D3DPT_LINELIST, 0, vI, iI / 2, Idx, D3DFMT_INDEX16, Vtx, sizeof(SkpVtx)) == S_OK);
		}
		iI = vI = 0;
	}
}


// ===============================================================================================
//
int D3D9Pad::DrawSketchMesh(SKETCHMESH _hMesh, DWORD grp, DWORD flags, SURFHANDLE hTex)
{
	SketchMesh *hMesh = static_cast<SketchMesh *>(_hMesh);

	// Flush existing artwork out before starting a new one ------
	//
	Flush(SKPTECH_MESH);

	// Do we have a new mesh -------------------------------------
	//
	if (hOldMesh != hMesh) {
		hMesh->Init();
		hOldMesh = hMesh;
	}

	// Draw a mesh group ----------------------------------------
	//
	SURFHANDLE pTex = hMesh->GetTexture(grp);
	D3DXCOLOR   Mat = hMesh->GetMaterial(grp);

	if (hTex) {
		HR(FX->SetTexture(eTex0, SURFACE(hTex)->GetTexture()));
		HR(FX->SetBool(eTexEn, true));
	}
	else {
		if (pTex) {
			HR(FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture()));
			HR(FX->SetBool(eTexEn, true));
		}
		else {
			HR(FX->SetBool(eTexEn, false));
		}
	}

	HR(FX->SetBool(eShade, (flags&MF_SMOOTH_SHADE)!=0));
	HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(D3DXCOLOR)));
	HR(FX->SetValue(eMtrl, &Mat, sizeof(D3DXCOLOR)));
	HR(FX->CommitChanges());

	if (flags&MF_CULL_NONE) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	else				    pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	if (grp < hMesh->GroupCount()) {
		hMesh->RenderGroup(grp);
		return hMesh->GroupCount();
	}
	return -1;
}


// ===============================================================================================
//
int D3D9Pad::DrawMeshGroup(MESHHANDLE hMesh, DWORD grp, DWORD flags, SURFHANDLE hTex)
{
	// Flush existing artwork out before starting a new one ------
	//
	Flush(SKPTECH_MESH);

	MESHGROUP *gr = oapiMeshGroup(hMesh, grp);

	if (!gr) return -1;

	if (!hTex && gr->TexIdx>0) hTex = oapiGetTextureHandle(hMesh, gr->TexIdx);

	MATERIAL *mat = oapiMeshMaterial(hMesh, gr->MtrlIdx);
	
	if (mat) {
		HR(FX->SetValue(eMtrl, &mat->diffuse, sizeof(D3DXCOLOR)));
	}
	else {
		HR(FX->SetValue(eMtrl, &D3DXCOLOR(1,1,1,1), sizeof(D3DXCOLOR)));
	}

	if (hTex) {
		HR(FX->SetTexture(eTex0, SURFACE(hTex)->GetTexture()));
		HR(FX->SetBool(eTexEn, true));
	}
	else {
		HR(FX->SetBool(eTexEn, false));
	}

	HR(FX->SetBool(eShade, (flags&MF_SMOOTH_SHADE) != 0));
	HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(D3DXCOLOR)));

	HR(FX->CommitChanges());

	if (flags&MF_CULL_NONE) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	else				    pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, gr->nVtx, gr->nIdx/3, gr->Idx, D3DFMT_INDEX16, gr->Vtx, sizeof(NTVERTEX));

	return oapiMeshGroupCount(hMesh);
}


// ===============================================================================================
//
bool D3D9Pad::Flush(int iTech)
{

	bool bStateChnage = bPenChange || bViewChange || bFontChange || (iTech != CurrentTech);


	// Is the drawing queue full ? -------------------------------------
	//
	if (iI >= WORD(nIndexMax - 1800)) FlushPrimitives();
	

	// Has something changed ?
	//
	if (!bStateChnage) return false;

	FlushPrimitives();


	// Check needed primitive type -------------------------------------
	//
	bTriangles = false;

	if (HasBrush()) bTriangles = true;
	if (GetPenWidth() > 1.1f) bTriangles = true;
	if (iTech == SKPTECH_BLIT) bTriangles = true;
	if (iTech == SKPTECH_POLY) bTriangles = true;
	if (vmode == USER) bTriangles = true;


	// Do we need a new tech ? -----------------------------------------
	//
	if (iTech != CurrentTech) {

		hOldMesh = NULL;

		// Switch from  -------------------------------------------------
		//
		if (CurrentTech == SKPTECH_MESH) EndMesh();

		if (iTech == SKPTECH_MESH) {
			if (CurrentTech == SKPTECH_DRAW) EndDraw();
			if (CurrentTech == SKPTECH_BLIT) EndDraw();
			if (CurrentTech == SKPTECH_POLY) EndDraw();
		}
		
		// Switch to  ----------------------------------------------------
		//
		if (iTech == SKPTECH_MESH) BeginMesh();

		if (CurrentTech == SKPTECH_MESH || CurrentTech == 0) {
			if (iTech == SKPTECH_DRAW) BeginDraw();
			if (iTech == SKPTECH_BLIT) BeginDraw();
			if (iTech == SKPTECH_POLY) BeginDraw();
		}
	}
	

	// Apply a new setup -----------------------------------------------
	//
	if (bViewChange) {

		if (vmode == ORTHO) {
			D3DXMATRIX mWVP;
			D3DXMatrixMultiply(&mWVP, &mW, &mO);
			HR(FX->SetMatrix(eWVP, &mWVP));
			HR(FX->SetMatrix(eVP, &mO));
			HR(FX->SetMatrix(eW, &mW));
			HR(FX->SetBool(eCovEn, false));
		}
		else {
			float d = float(tgt_desc.Height) * mP._22;
			float f = atan(1.0f / d) * 1.7f;
			D3DXMatrixMultiply(&mVP, &mV, &mP);
			HR(FX->SetMatrix(eVP, &mVP));
			HR(FX->SetMatrix(eW, &mW));
			HR(FX->SetFloat(eFov, f));
			HR(FX->SetBool(eCovEn, ClipData[0].bEnable || ClipData[1].bEnable));
		}

		bViewChange = false;
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (bPenChange) {
		float offset = 0.0f;
		int w = int(ceil(GetPenWidth()));
		if ((w&1)==0) offset = 0.5f;
		HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(D3DXCOLOR)));
		HR(FX->SetBool(eDashEn, IsDashed()));
		HR(FX->SetValue(eWidth, &D3DXVECTOR3(GetPenWidth(), pattern*0.13f, offset), sizeof(D3DXVECTOR3)));
		bPenChange = false;
	}


	// Apply a new setup -----------------------------------------------------------------
	//
	if (bFontChange) {
		DWORD Quality = static_cast<D3D9PadFont *>(cfont)->GetQuality();
		HR(FX->SetBool(eClearEn, Quality == CLEARTYPE_QUALITY));
		bFontChange = false;
	}

	HR(FX->SetBool(eWide, bTriangles));

	HR(FX->CommitChanges());

	CurrentTech = iTech;
	
	return true;
}







// ======================================================================================
// SketchMesh Interface
// ======================================================================================

SketchMesh::SketchMesh(LPDIRECT3DDEVICE9 _pDev) :
	pDev(_pDev),
	Tex(NULL),
	Grp(NULL),
	Mtrl(NULL)
{
}


// ===============================================================================================
//
SketchMesh::~SketchMesh()
{
	SAFE_DELETEA(Mtrl);
	SAFE_DELETEA(Tex);
	SAFE_DELETEA(Grp);
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pIB);
}

// ===============================================================================================
//
bool SketchMesh::LoadMesh(const char *name)
{
	MESHHANDLE hMesh = oapiLoadMesh(name);

	if (hMesh) {
		bool bRet = LoadMeshFromHandle(hMesh);
		oapiDeleteMesh(hMesh);
		return bRet;
	}

	oapiWriteLogV("gcLoadSketchMesh(%s): Mesh not found", name);
		
	return false;
}

// ===============================================================================================
//
bool SketchMesh::LoadMeshFromHandle(MESHHANDLE hMesh)
{
	pVB = NULL;
	pIB = NULL;
	Mtrl = NULL;
	Tex = NULL;
	Grp = NULL;

	MaxVert = MaxIdx = 0;

	nGrp = oapiMeshGroupCount(hMesh);
	if (nGrp == 0) return false;

	Grp = new SKETCHGRP[nGrp];
	memset2(Grp, 0, sizeof(SKETCHGRP) * nGrp);

	// -----------------------------------------------------------------------

	nTex = oapiMeshTextureCount(hMesh) + 1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	for (DWORD i = 1; i<nTex; i++) Tex[i] = SURFACE(oapiGetTextureHandle(hMesh, i));

	// -----------------------------------------------------------------------

	nMtrl = oapiMeshMaterialCount(hMesh);
	if (nMtrl) Mtrl = new D3DXCOLOR[nMtrl];
	for (DWORD i = 0; i < nMtrl; i++) {
		MATERIAL *pMat = oapiMeshMaterial(hMesh, i);
		if (pMat) {
			Mtrl[i].r = pMat->diffuse.r;
			Mtrl[i].g = pMat->diffuse.g;
			Mtrl[i].b = pMat->diffuse.b;
			Mtrl[i].a = pMat->diffuse.a;
		}
	}

	// -----------------------------------------------------------------------

	for (DWORD i = 0; i < nGrp; i++) {
		MESHGROUPEX *pEx = oapiMeshGroupEx(hMesh, i);
		Grp[i].MtrlIdx = pEx->MtrlIdx;
		Grp[i].TexIdx = pEx->TexIdx;
		Grp[i].nVert = pEx->nVtx;
		Grp[i].nIdx = pEx->nIdx;
		Grp[i].VertOff = MaxVert;
		Grp[i].IdxOff = MaxIdx;
		MaxVert += pEx->nVtx;
		MaxIdx += pEx->nIdx;
	}

	if (MaxVert == 0 || MaxIdx == 0) return false;

	// -----------------------------------------------------------------------

	if (Grp[0].MtrlIdx == SPEC_INHERIT) Grp[0].MtrlIdx = SPEC_DEFAULT;
	if (Grp[0].TexIdx == SPEC_INHERIT) Grp[0].TexIdx = SPEC_DEFAULT;
	
	for (DWORD i = 0; i<nGrp; i++) {

		if (Grp[i].MtrlIdx == SPEC_INHERIT) Grp[i].MtrlIdx = Grp[i - 1].MtrlIdx;

		if (Grp[i].TexIdx == SPEC_DEFAULT) Grp[i].TexIdx = 0;
		else if (Grp[i].TexIdx == SPEC_INHERIT) Grp[i].TexIdx = Grp[i - 1].TexIdx;
		else Grp[i].TexIdx++;
	}

	// -----------------------------------------------------------------------

	HR(pDev->CreateVertexBuffer(MaxVert * sizeof(NTVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxIdx * sizeof(WORD), 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	NTVERTEX *pVert = NULL;
	WORD *pIndex = NULL;

	for (DWORD i = 0; i < nGrp; i++) {
		MESHGROUPEX *pEx = oapiMeshGroupEx(hMesh, i);
		HR(pIB->Lock(Grp[i].IdxOff * sizeof(WORD), Grp[i].nIdx * sizeof(WORD), (LPVOID*)&pIndex, 0));
		HR(pVB->Lock(Grp[i].VertOff * sizeof(NTVERTEX), Grp[i].nVert * sizeof(NTVERTEX), (LPVOID*)&pVert, 0));
		memcpy(pIndex, pEx->Idx, sizeof(WORD) * pEx->nIdx);
		memcpy(pVert, pEx->Vtx, sizeof(NTVERTEX) * pEx->nVtx);
		HR(pIB->Unlock());
		HR(pVB->Unlock());
	}

	return true;
}


// ===============================================================================================
//
void SketchMesh::Init()
{
	pDev->SetStreamSource(0, pVB, 0, sizeof(NTVERTEX));
	pDev->SetIndices(pIB);
}


// ===============================================================================================
//
void SketchMesh::RenderGroup(DWORD idx)
{
	if (!pVB) return;
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[idx].VertOff, 0, Grp[idx].nVert, Grp[idx].IdxOff, Grp[idx].nIdx/3);
}


// ===============================================================================================
//
SURFHANDLE SketchMesh::GetTexture(DWORD idx)
{
	assert(idx < nGrp);
	if (Grp[idx].TexIdx) return Tex[Grp[idx].TexIdx];
	return NULL;
}


// ===============================================================================================
//
D3DXCOLOR SketchMesh::GetMaterial(DWORD idx)
{
	assert(idx < nGrp);
	if (Grp[idx].MtrlIdx!= SPEC_DEFAULT && Mtrl) return Mtrl[Grp[idx].MtrlIdx];
	return D3DXCOLOR(1,1,1,1);
}







// ======================================================================================
// Polyline Interface
// ======================================================================================



D3D9PolyLine::D3D9PolyLine(LPDIRECT3DDEVICE9 pDev, const FVECTOR2 *pt, int npt, bool bConnect)
{
	nPt = npt + 2;
	nVtx = 2 * nPt;
	nIdx = 6 * nPt;

	HR(pDev->CreateVertexBuffer(nVtx * sizeof(SkpVtx), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(nIdx * sizeof(WORD), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	WORD *Idx = NULL;

	HR(pIB->Lock(0, 0, (LPVOID*)&Idx, D3DLOCK_DISCARD));

	iI = 0;

	for (WORD i=0,p=0;p<nPt;p++) {
		Idx[iI++] = i+0;	
		Idx[iI++] = i+1;
		Idx[iI++] = i+2;	
		Idx[iI++] = i+1;
		Idx[iI++] = i+3;	  
		Idx[iI++] = i+2;
		i += 2;
	}

	HR(pIB->Unlock());

	if (pt) Update(pt, npt, bConnect);
}


// ===============================================================================================
//
D3D9PolyLine::~D3D9PolyLine()
{
	
}


// ===============================================================================================
//
void D3D9PolyLine::Release()
{
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pIB);
}


// ===============================================================================================
//
void D3D9PolyLine::Draw(LPDIRECT3DDEVICE9 pDev, DWORD flags)
{
	pDev->SetStreamSource(0, pVB, 0, sizeof(SkpVtx));
	pDev->SetIndices(pIB);
	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, vI, 0, vI-2);
}


// ===============================================================================================
//
void D3D9PolyLine::Update(const FVECTOR2 *_pt, int _npt, bool bConnect)
{
	SkpVtx *Vtx = NULL;
	D3DXVECTOR2 *pt = (D3DXVECTOR2 *)_pt;

	HR(pVB->Lock(0, 0, (LPVOID*)&Vtx, D3DLOCK_DISCARD));

	WORD npt = WORD(_npt);
	WORD li = WORD(npt - 1);

	vI = 0;
	float length = 0.0f;

	D3DXVECTOR2 pp; // Prev point
	D3DXVECTOR2 np;	// Next point

	bLoop = bConnect;

	// Line Init ------------------------------------------------------------
	//
	if (bLoop) pp = pt[li];
	else	   pp = pt[0] * 2.0 - pt[1];

	// Create line segments -------------------------------------------------
	//
	for (WORD i = 0; i<npt; i++) {

		if (i != li)	np = pt[i + 1];
		else {
			if (bLoop)	np = pt[0];
			else		np = pt[i] * 2 - pt[i - 1];
		}

		WORD vII = vI + 1;

		// --------------------------------------
		Vtx[vI].x = Vtx[vII].x = float(pt[i].x);
		Vtx[vI].y = Vtx[vII].y = float(pt[i].y);
		Vtx[vI].nx = Vtx[vII].nx = np.x;
		Vtx[vI].ny = Vtx[vII].ny = np.y;
		Vtx[vI].px = Vtx[vII].px = pp.x;
		Vtx[vI].py = Vtx[vII].py = pp.y;
		Vtx[vI].l = Vtx[vII].l = length;
		// --------------------------------------
		Vtx[vI].fnc = SKPSW_WIDEPEN_L | SKPSW_EXTCOLOR;
		Vtx[vII].fnc = SKPSW_WIDEPEN_R | SKPSW_EXTCOLOR;
		vI+=2;
		// --------------------------------------
		pp = pt[i];
		length += D3DXVec2Length(&(np - pp));
	}

	if (bLoop) {
		Vtx[vI++] = Vtx[0];
		Vtx[vI++] = Vtx[1];
	}

	HR(pVB->Unlock());
}