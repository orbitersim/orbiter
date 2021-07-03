//-----------------------------------------------------------------------------
// File: Shadow.h
//
// Desc: Header for stencil shadow example
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DEnum.h"
#include "assert.h"
#include "resource.h"


// Most objects are spheres, this defines several user-selectable levels of
// tesselation
#define NUMTESSLEVELS 4
extern WORD  g_TessLevs[NUMTESSLEVELS];
extern DWORD g_CurTessLevel;


struct COLORVERTEX
{
    D3DVECTOR p;
    D3DCOLOR  c;
};


// These are the shadow volumes
struct SHADOW
{
	LPDIRECT3DVERTEXBUFFER7 VB;    // holds vertices of shadow volumes
	WORD*   pwShadVolIndices;      // tri indices into vertex buffer VB for DrawPrim
	WORD*   pwShadVolSideIndices;  // ptrs into main index array pwShadVolIndices for Side tris of shadow volume
	WORD*   pwShadVolCapIndices;   // ptrs into main index array pwShadVolIndices for cap tris of shadow volume
	DWORD   dwNumVertices;
	DWORD   dwNumSideIndices;
	DWORD   dwNumCapIndices;
};

// Objects that cast shadows
struct SHADOWCASTER
{
    D3DVERTEX* pVertices;
	D3DVERTEX* pRVertices;
    DWORD      dwNumVertices;
	DWORD      dwNumIndices;
    WORD*      pIndices;
    D3DVECTOR  vCenter;  // un-xformed
};


extern VOID Find2DConvexHull( DWORD nverts, COLORVERTEX* pntptr,
							  DWORD* cNumOutIdxs, WORD** OutHullIdxs );
extern VOID RotateVertexInX( FLOAT, DWORD, D3DVERTEX*, D3DVERTEX* );
extern VOID RotateVertexInY( FLOAT, DWORD, D3DVERTEX*, D3DVERTEX* );
extern VOID RotateVertexInZ( FLOAT, DWORD, D3DVERTEX*, D3DVERTEX* );
extern VOID TransRotateVertexInX( D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                  D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices );
extern VOID TransRotateVertexInY( D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                  D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices );
extern VOID TransRotateVertexInZ( D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                  D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices );
extern BOOL GenerateSphere( SHADOWCASTER *,D3DVECTOR&, FLOAT, WORD, WORD,
						    FLOAT, FLOAT, FLOAT );



