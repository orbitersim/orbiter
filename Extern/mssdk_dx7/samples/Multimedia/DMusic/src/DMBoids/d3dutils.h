#ifndef D3DUTILS_H
#define D3DUTILS_H
/*
**-----------------------------------------------------------------------------
** Name:    D3DUtils.h
** Purpose: Various D3D utility functions
** Notes:
**
** Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

#define D3D_OVERLOADS
#include <d3d.h>
#include <d3dtypes.h>

extern const float pi;

/*
**-----------------------------------------------------------------------------
** Function Prototypes
**-----------------------------------------------------------------------------
*/

// generic simple matrix routines
extern D3DMATRIX ZeroMatrix(void);
extern D3DMATRIX IdentityMatrix(void);

extern D3DMATRIX ProjectionMatrix(const float near_plane, const float far_plane, const float fov);
extern D3DMATRIX ViewMatrix(const D3DVECTOR & from, const D3DVECTOR & at, 
							const D3DVECTOR & up, const float roll=0.0f);

extern D3DMATRIX RotateXMatrix(const float rads);
extern D3DMATRIX RotateYMatrix(const float rads);
extern D3DMATRIX RotateZMatrix(const float rads);
extern D3DMATRIX TranslateMatrix(const float dx, const float dy, const float dz);
extern D3DMATRIX TranslateMatrix(const D3DVECTOR & v);
extern D3DMATRIX ScaleMatrix(const float size);
extern D3DMATRIX ScaleMatrix(const float a, const float b, const float c);
extern D3DMATRIX ScaleMatrix(const D3DVECTOR & v);

extern D3DMATRIX MatrixMult(const D3DMATRIX & a, const D3DMATRIX & b);
extern D3DMATRIX MatrixInverse(const D3DMATRIX & m);
extern D3DMATRIX MatrixTranspose(const D3DMATRIX & m);

extern D3DVECTOR TransformVector(const D3DVECTOR & v, const D3DMATRIX & m);
extern D3DVECTOR TransformNormal(const D3DVECTOR & v, const D3DMATRIX & m);
extern D3DVECTOR TransformNormalPureRotation(const D3DVECTOR & v, const D3DMATRIX & m);

// Other stuff
extern float rnd(void);


/*
**-----------------------------------------------------------------------------
** Classes
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**	Class:		Light
**	Purpose:	
**-----------------------------------------------------------------------------
*/

class Light {
protected:
public:
	D3DLIGHT2			light;		// structure defining the light
	LPDIRECT3DLIGHT		lpD3DLight;	// object pointer for the light
	int					changed;
public:
	Light(LPDIRECT3D2 lpD3D);
	~Light();

	HRESULT		AddToViewport(LPDIRECT3DVIEWPORT2 lpView);
	HRESULT		RemoveFromViewport(LPDIRECT3DVIEWPORT2 lpView);
	HRESULT		Set(void);

	void		SetColor(const D3DVECTOR& color);
	D3DVECTOR	GetColor(void) const;
	void		SetPosition(const D3DVECTOR& position);
	D3DVECTOR	GetPosition(void) const;
	void		SetDirection(const D3DVECTOR& direction);
	D3DVECTOR	GetDirection(void) const;
	void		SetAttenuation(const D3DVECTOR& attenuation);
	D3DVECTOR	GetAttenuation(void) const;
	void		SetRange(const float range);
	float		GetRange(void) const;
	void		SetFalloff(const float falloff);
	float		GetFalloff(void) const;
	void		SetUmbra(const float falloff);
	float		GetUmbra(void) const;
	void		SetPenumbra(const float falloff);
	float		GetPenumbra(void) const;
	void		SetFlags(const DWORD flags);
	DWORD		GetFlags(void) const;
}; // End Light


/*
**-----------------------------------------------------------------------------
**	Class:		PointLight
**	Purpose:	
**-----------------------------------------------------------------------------
*/

class PointLight : public Light {
	public:
		PointLight(LPDIRECT3D2 lpD3D, 
					const D3DVECTOR & color, 
					const D3DVECTOR & position);
}; // End PointLight


/*
**-----------------------------------------------------------------------------
**	Class:		SpotLight
**	Purpose:	
**-----------------------------------------------------------------------------
*/

class SpotLight : public Light {
	public:
		SpotLight(LPDIRECT3D2 lpD3D, 
				const D3DVECTOR& color, 
				const D3DVECTOR& position, 
				const D3DVECTOR& direction, 
				const float umbra_angle, 
				const float penumbra_angle);
}; // End SpotLight



/*
**-----------------------------------------------------------------------------
**	Class:		DirectionalLight
**	Purpose:	
**-----------------------------------------------------------------------------
*/

class DirectionalLight : public Light {
	public:
		DirectionalLight(LPDIRECT3D2 lpD3D, 
			const D3DVECTOR& color, const D3DVECTOR& direction);
};



/*
//-----------------------------------------------------------------------------
//	Class:		ParallelPointLight
//	Purpose:	
//-----------------------------------------------------------------------------

class ParallelPointLight : public Light {
	public:
		ParallelPointLight(LPDIRECT3D2 lpD3D, const D3DVECTOR& color, const D3DVECTOR& position);
}; // End ParallelPointLight
*/



/*
**-----------------------------------------------------------------------------
**	Class:		Material
**	Purpose:	
**-----------------------------------------------------------------------------
*/

class Material 
{
protected:
	D3DMATERIAL			Mat;
    D3DMATERIALHANDLE	hMat;
	LPDIRECT3DMATERIAL2 lpMat;
	int					changed;
public:
	Material(LPDIRECT3D2 lpD3D, LPDIRECT3DDEVICE2 lpDev);
	~Material();

	HRESULT		SetAsCurrent(LPDIRECT3DDEVICE2 lpDev);
	HRESULT		SetAsBackground(LPDIRECT3DVIEWPORT2 lpView);
	HRESULT		Set(void);

	void		SetDiffuse(const D3DVECTOR& color);
	D3DVECTOR	GetDiffuse(void) const;
	void		SetAlpha(const float& alpha);
	float		GetAlpha(void) const;
	void		SetAmbient(const D3DVECTOR& color);
	D3DVECTOR	GetAmbient(void) const;
	void		SetEmissive(const D3DVECTOR& color);
	D3DVECTOR	GetEmissive(void) const;
	void		SetSpecular(const D3DVECTOR& color);
	D3DVECTOR	GetSpecular(void) const;
	void		SetPower(const D3DVALUE& power);
	D3DVALUE	GetPower(void) const;
	void		SetRampSize(const DWORD& ramp);
	DWORD		GetRampSize(void) const;
	void		SetTextureHandle(const D3DTEXTUREHANDLE& hTexture);
	D3DTEXTUREHANDLE	GetTextureHandle(void);
}; // End Material


//
// include the inline functions for the classes
//
#include "d3dutils.inl"


/*
**-----------------------------------------------------------------------------
**	End of File
**-----------------------------------------------------------------------------
*/
#endif // D3DUTILS_H


