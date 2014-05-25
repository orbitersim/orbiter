// ==============================================================
// VStar.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2007 Martin Schweiger
// ==============================================================

// ==============================================================
// class vStar (interface)
//
// Renders the central star as a billboard mesh.
// ==============================================================

#ifndef __VSTAR_H
#define __VSTAR_H

#include "VObject.h"

class D3D9Mesh;

class vStar: public vObject {
public:
	vStar (OBJHANDLE _hObj, const Scene *scene);
	~vStar ();

	static void GlobalInit (oapi::D3D9Client *gc);
	static void GlobalExit ();

	bool Update ();
	bool Render (LPDIRECT3DDEVICE9 dev);

private:
	double size;       ///< physical size of central body
	double maxdist;    ///< max render distance
	//D3DMATERIAL9 mtrl; ///< Material definition for rendering
	//static D3D9Mesh *billboard;         ///< visual
	static LPD3D9CLIENTSURFACE deftex; ///< default texture
};

#endif // !__VSTAR_H