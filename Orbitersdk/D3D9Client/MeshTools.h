// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================



#include "D3D9Client.h"
#include "D3D9Effect.h"
#include "Mesh.h"
#include "D3D9Util.h"
#include <d3dx9.h>
#include <vector>
#include <list>

#define ADMESH_NT_VTX	0x1		///<  Native Orbiter format
#define ADMESH_NM_VTX	0x2		///<  D3D9 specific format with tangents

typedef struct {
	D3DXVECTOR3 pos;
	D3DXVECTOR3 nrm;
	float tu, tv;
} NATIVE;

typedef struct {
	WORD edge[12];
} EDGELIST;

typedef struct {
	WORD Vtx[2];
	WORD Face[2];
} EDGE;

typedef struct {
	WORD Vtx[4];
} FACE;

typedef class AdGroup * HGROUP;


// ===============================================================================================
//
class AdGroup 
{

public:
						AdGroup();
						~AdGroup();

	// -------------------------------------------------------------------------------------------
	DWORD				Append(const void *pV, DWORD fmt, const WORD *pI, DWORD nV, DWORD nI, const LPD3DXMATRIX pW=NULL);
	DWORD				Append(HMESH hMesh, DWORD grp, LPD3DXMATRIX pW=NULL);
	DWORD				Append(const HGROUP hSrc, const LPD3DXMATRIX pW=NULL);
	DWORD				Append(MESHHANDLE hMesh, DWORD grp, const LPD3DXMATRIX pW=NULL);

	// -------------------------------------------------------------------------------------------
	DWORD				GetVertexCount() const;
	DWORD				GetIndexCount() const;

	// -------------------------------------------------------------------------------------------
	void				GetVertices(NMVERTEX *pData);
	void				GetIndices(WORD *pData);

	// -------------------------------------------------------------------------------------------
	void				NormalizeGroupSize();
	void				NormalizeNormals();
	void				ComputeBounds();
	
	// -------------------------------------------------------------------------------------------
	std::vector<NATIVE>	Vtx;
	std::vector<WORD>	Idx;

	D3DXVECTOR3			vBSc;			//<! Bounding sphere centre
	float				vBSr;			//<! Bounging sphere radius
	DWORD				Mtrl;			//<! Material index
	DWORD				Tex;			//<! Texture index, zero = no texture
};



// ===============================================================================================
//
class AdMesh 
{

public:
						AdMesh();
						AdMesh(MESHHANDLE hMesh);
						AdMesh(const char *file, bool bValidate=false);
						~AdMesh();

	// -------------------------------------------------------------------------------------------
	HGROUP				AddGroup();
	HGROUP				AddGroup(const void *pV, DWORD fmt, const WORD *pI, DWORD nV, DWORD nI, const LPD3DXMATRIX pW=NULL);
	HGROUP				AddGroup(HMESH hMesh, DWORD grp, LPD3DXMATRIX pW=NULL);
	HGROUP				AddGroup(const HGROUP hSrc, const LPD3DXMATRIX pW=NULL);
	HGROUP				AddGroup(MESHHANDLE hMesh, DWORD grp, const LPD3DXMATRIX pW=NULL);
	
	// -------------------------------------------------------------------------------------------
	void				DelGroup(HGROUP hGrp);
	HGROUP				GetGroup(DWORD index);

	// -------------------------------------------------------------------------------------------
	DWORD				GetGroupCount() const { return Groups.size(); }
	DWORD				GetVertexCount() const;
	DWORD				GetIndexCount() const;

	// -------------------------------------------------------------------------------------------
	std::vector<HGROUP>			Groups;
	std::vector<MATERIAL>		Materials;
	std::vector<SURFHANDLE>		Textures;
};



// ===============================================================================================
//
inline NATIVE _NT(NTVERTEX *v)
{
	NATIVE n;
	n.pos = D3DXVECTOR3(v->x, v->y, v->z);
	n.nrm = D3DXVECTOR3(v->nx, v->ny, v->nz);
	n.tu  = v->tu;
	n.tv  = v->tv;
	return n;
}

// ===============================================================================================
//
inline NATIVE _NM(NMVERTEX *v)
{
	NATIVE n;
	n.pos = D3DXVECTOR3(v->x, v->y, v->z);
	n.nrm = D3DXVECTOR3(v->nx, v->ny, v->nz);
	n.tu  = v->u;
	n.tv  = v->v;
	return n;
}

// ===============================================================================================
//
inline void TransformVertex(NATIVE *Vrt, const LPD3DXMATRIX pW)
{
	D3DXVec3TransformCoord(&Vrt->pos, &Vrt->pos, pW);
	D3DXVec3TransformNormal(&Vrt->nrm, &Vrt->nrm, pW);
}