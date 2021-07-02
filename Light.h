// =======================================================================
// Class Light
// Light visual object

#include <d3d.h>

#ifndef __LIGHT_H
#define __LIGHT_H

class Light {
public:
	Light();
	~Light();

	LPDIRECT3DLIGHT LObj () const { return lobj; }
	D3DLIGHT2 *Spec() { return &spec; }

	LPDIRECT3DLIGHT Create (LPDIRECT3DDEVICE3 dev);
	// create a light with current specs and associate with dev

	void Delete ();
	// delete the light (but retain current specs)

	void SetDirection (D3DVALUE xdir, D3DVALUE ydir, D3DVALUE zdir);
	// set light direction for directional light types

private:
	D3DLIGHT2 spec;
	LPDIRECT3DLIGHT lobj;
};

#endif // !__LIGHT_H