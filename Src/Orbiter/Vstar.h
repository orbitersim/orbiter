// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class VStar
// Visual for star objects

#ifndef __VSTAR_H
#define __VSTAR_H

#include "Vobject.h"
#include "Mesh.h"

class Planet;
class Camera;

class VStar: public VObject {
public:
	VStar (const Star *_star);
	~VStar ();
	void Update (bool moving, bool force);
	void CheckResolution (double iar) {}
	void Render (LPDIRECT3DDEVICE7 dev);
	void RenderVectors (LPDIRECT3DDEVICE7 dev) {}
	COLORREF CenterPixelColor();

	void BlindColour (const Camera *cam, Vector &col);
	// returns background colour including blind effect
	// depending on camera direction
	// Vector is interpreted as RGB (0-1)

protected:
	static Mesh BillboardMesh;   // lores visual

private:
	void SetMaterial ();
	const Star *star;
	int resolution;
	D3DMATERIAL7 mtrl;
};

#endif // !__VSTAR_H