// ---------------------------------------------------------------------------------------------
// DirectX 9 and Generic Utility Toolkit
// Copyright (c) 2010-2012 Jarmo Nikkanen
//
// Permission granted for use and redistribution with ORBITER VISUALISATION PROJECT (OVP)
// ---------------------------------------------------------------------------------------------

#include <d3d9.h> 
#include <d3dx9.h>

#ifdef D3D9TK_EXPORTS
#define D3D9TK __declspec(dllexport)
#else
#define D3D9TK __declspec(dllimport)
#endif

#ifndef __D3D9TK_H
#define __D3D9TK_H

typedef struct 
{
	D3DXVECTOR4 min, max, bs, a, b, c;
} D9BBox;

D3D9TK float	D9NearPlane(LPDIRECT3DDEVICE9 pDev, float zmin, float zmax, float dmax, const D3DXMATRIX *pProj, bool bReduced);
D3D9TK int		D9ComputeMinMaxDistance(LPDIRECT3DDEVICE9 pDev, const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F, float *zmin, float *zmax, float *dmin);
D3D9TK void		D9AddAABB(const D9BBox *in, const D3DXMATRIX *pM, D9BBox *out, bool bReset=false);
D3D9TK void		D9UpdateAABB(D9BBox *box, const D3DXMATRIX *pFisrt=NULL, const D3DXMATRIX *pSecond=NULL);
D3D9TK void		D9ZeroAABB(D9BBox *box);
D3D9TK void		D9InitAABB(D9BBox *box);
D3D9TK void		D9AddPointAABB(D9BBox *box, LPD3DXVECTOR3 point);
D3D9TK bool		D9IsAABBVisible(const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F);
D3D9TK bool		D9IsBSVisible(const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F);

D3D9TK D3DXVECTOR4 D9LinearFieldOfView(const D3DXMATRIX *pProj);
D3D9TK D3DXVECTOR4 D9OffsetRange(double R, double r);

#endif