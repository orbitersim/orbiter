#include "Light.h"
#include "Vecmat.h"

Light::Light ()
{
	lobj = 0;

	ZeroMemory (&spec, sizeof(D3DLIGHT2));
    spec.dwSize       = sizeof(D3DLIGHT2);
	// some default settings
    spec.dltType       = D3DLIGHT_DIRECTIONAL;
    spec.dcvColor.r    = 1.0f;
    spec.dcvColor.g    = 1.0f;
    spec.dcvColor.b    = 1.0f;
	spec.dvPosition.x  = 0.0f;
	spec.dvPosition.y  = 0.0f;
	spec.dvPosition.z  = 0.0f;
    spec.dvDirection.x = 0.0f;
    spec.dvDirection.y = 0.0f;
    spec.dvDirection.z = 1.0f;
	spec.dvRange       = D3DLIGHT_RANGE_MAX;
	spec.dvFalloff     = 1.0;
	spec.dvAttenuation0 = 1.0; // constant term
	spec.dvAttenuation1 = 0.0; // linear term
	spec.dvAttenuation2 = 0.0; // quadratic term
	// note: "Attenuation" is in fact flux density!
	spec.dvTheta = (FLOAT)Pi05; // spotlight inner cone
	spec.dvPhi   = (FLOAT)Pi05; // spotlight outer cone
	spec.dwFlags = D3DLIGHT_ACTIVE | D3DLIGHT_NO_SPECULAR;
}

Light::~Light ()
{
	if (lobj) lobj->Release();
}

LPDIRECT3DLIGHT Light::Create (LPDIRECT3DDEVICE3 dev)
{
    LPDIRECT3D3 D3D;
    dev->GetDirect3D (&D3D);
    D3D->Release();
	D3D->CreateLight (&lobj, NULL);
	lobj->SetLight ((D3DLIGHT*)&spec); // is this ok?
	return lobj;
}

void Light::Delete ()
{
	lobj->Release();
	lobj = 0;
}

void Light::SetDirection (D3DVALUE xdir, D3DVALUE ydir, D3DVALUE zdir)
{
    spec.dvDirection.x = xdir;
    spec.dvDirection.y = ydir;
    spec.dvDirection.z = zdir;
	if (lobj) lobj->SetLight ((D3DLIGHT*)&spec);
}