/*
**-----------------------------------------------------------------------------
** Name:    D3DUtils.inl
** Purpose: Various D3D utility functions
** Notes:   This defines inline functions only
**
** Copyright (c) 1995 - 1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**	Light Inline Methods
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:	Light::AddToViewport
**-----------------------------------------------------------------------------
*/

inline HRESULT
Light::AddToViewport (LPDIRECT3DVIEWPORT2 lpView)
{
	Set();
	return lpView->AddLight(lpD3DLight);
} // end Light::AddToViewport


/*
**-----------------------------------------------------------------------------
**  Name:	Light::RemoveFromViewport
**-----------------------------------------------------------------------------
*/

inline HRESULT
Light::RemoveFromViewport (LPDIRECT3DVIEWPORT2 lpView)
{
	return lpView->DeleteLight(lpD3DLight);
} // end Light::RemoveFromViewport


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::Set
**-----------------------------------------------------------------------------
*/

inline HRESULT	
Light::Set (void)
{	
	if (changed) {
		changed = 0;
		return lpD3DLight->SetLight((D3DLIGHT *)&light);
	} 

	return D3D_OK;
} // end Light::Set


/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetColor
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetColor (const D3DVECTOR & color)
{
    light.dcvColor.r = color[0];
    light.dcvColor.g = color[1];
    light.dcvColor.b = color[2];
	changed = 1;
} // end Light::SetColor


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetColor
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Light::GetColor (void) const
{
	D3DVECTOR	ret;
	ret[0] = light.dcvColor.r;
	ret[1] = light.dcvColor.g;
	ret[2] = light.dcvColor.b;
	return ret;
} // end Light::GetColor



/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetPosition
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetPosition (const D3DVECTOR & position)
{ 
    light.dvPosition = position;
	changed = 1;
} // end Light::SetPosition


/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetPosition
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Light::GetPosition(void) const
{ 
	return light.dvPosition;
} // end Light::GetPosition


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetDirection
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetDirection (const D3DVECTOR & direction)
{ 
    light.dvDirection = direction;
	changed = 1;
}



/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetDirection
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Light::GetDirection(void) const
{ 
	return light.dvDirection;
}


/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetAttenuation
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetAttenuation (const D3DVECTOR & attenuation)
{ 
	light.dvAttenuation0 = attenuation[0];
	light.dvAttenuation1 = attenuation[1];
	light.dvAttenuation2 = attenuation[2];
	changed = 1;
} // end Light::SetAttenuation



/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetAttenuation
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Light::GetAttenuation(void) const
{ 
	D3DVECTOR	ret;
	ret[0] = light.dvAttenuation0;
	ret[1] = light.dvAttenuation1;
	ret[2] = light.dvAttenuation2;
	return ret;
} // end Light::GetAttenuation


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetRange
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetRange(const float range)
{
	light.dvRange = range;
	changed = 1;
} // end Light::SetRange


/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetRange
**-----------------------------------------------------------------------------
*/

inline float		
Light::GetRange(void) const
{
	return light.dvRange;
} // end Light::GetRange


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetFalloff
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetFalloff(const float falloff)
{
	light.dvFalloff = falloff;
	changed = 1;
} // end Light::SetFalloff


  
/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetFalloff
**-----------------------------------------------------------------------------
*/

inline float		
Light::GetFalloff(void) const
{
	return light.dvFalloff;
} // end Light::GetFalloff



/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetUmbra
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetUmbra(const float umbra)
{
	light.dvTheta = umbra;
	changed = 1;
} // end Light::SetUmbra



/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetUmbra
**-----------------------------------------------------------------------------
*/

inline float		
Light::GetUmbra(void) const
{
	return light.dvTheta;
} // end Light::GetUmbra



/*
**-----------------------------------------------------------------------------
**  Name:	Light::SetPenumbra
**-----------------------------------------------------------------------------
*/

inline void		
Light::SetPenumbra(const float penumbra)
{
	light.dvPhi = penumbra;
	changed = 1;
} // end Light::SetPenumbra



/*
**-----------------------------------------------------------------------------
**  Name:	Light::GetPenumbra
**-----------------------------------------------------------------------------
*/

inline float		
Light::GetPenumbra(void) const
{
	return light.dvPhi;
} // end Light::GetPenumbra


inline void		
Light::SetFlags(const DWORD flags)
{
	light.dwFlags = flags;
	changed = 1;
}	// end of Light::SetFlags()

inline DWORD		
Light::GetFlags(void) const
{
	return light.dwFlags;
}	// end of Light::GtFlags()

/*
**-----------------------------------------------------------------------------
**	Material inline methods
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetAsCurrent
**-----------------------------------------------------------------------------
*/

inline HRESULT
Material::SetAsCurrent(LPDIRECT3DDEVICE2 lpDev)
{
	HRESULT hResult;

	Set();

    hResult = lpDev->SetLightState(D3DLIGHTSTATE_MATERIAL, hMat);
	if (hResult != D3D_OK)
		return hResult;

    hResult = lpDev->SetRenderState(D3DRENDERSTATE_TEXTUREHANDLE, Mat.hTexture);
	if (hResult != D3D_OK)
        return hResult;

	// Success
	return D3D_OK;
} // end Material::SetAsCurrent


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::Set
**-----------------------------------------------------------------------------
*/

inline HRESULT
Material::Set(void)
{
	if (changed) {
		changed = 0;
		return lpMat->SetMaterial (&Mat);
	} 

	return D3D_OK;
} // end Material::Set



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetAsBackground
**-----------------------------------------------------------------------------
*/

inline HRESULT
Material::SetAsBackground (LPDIRECT3DVIEWPORT2 lpView)
{
	Mat.dwRampSize = 1;
	changed = 1;
	Set();
	return lpView->SetBackground(hMat);
} // end Material::SetAsBackground



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetDiffuse
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetDiffuse(const D3DVECTOR& color)
{
	Mat.diffuse.r = color[0];
	Mat.diffuse.g = color[1];
	Mat.diffuse.b = color[2];
	changed = 1;
} // end Material::SetDiffuse


/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetDiffuse
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Material::GetDiffuse(void) const
{
	D3DVECTOR	ret;
	ret[0] = Mat.diffuse.r;
	ret[1] = Mat.diffuse.g;
	ret[2] = Mat.diffuse.b;
	return ret;
} // end Material::GetDiffuse

inline void		
Material::SetAlpha(const float& alpha)
{
	Mat.diffuse.a = alpha;
	changed = 1;
}	// end of Material::SetAlpha()

inline float	
Material::GetAlpha(void) const
{
	return Mat.diffuse.a;
}	// end of Material::GetAlpha()



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetAmbient
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetAmbient(const D3DVECTOR& color)
{
	Mat.ambient.r = color[0];
	Mat.ambient.g = color[1];
	Mat.ambient.b = color[2];
	changed = 1;
} // end Material::SetAmbient


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetAmbient
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Material::GetAmbient(void) const
{
	D3DVECTOR	ret;
	ret[0] = Mat.ambient.r;
	ret[1] = Mat.ambient.g;
	ret[2] = Mat.ambient.b;
	return ret;
} // end Material::GetAmbient



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetEmissive
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetEmissive(const D3DVECTOR& color)
{
	Mat.emissive.r = color[0];
	Mat.emissive.g = color[1];
	Mat.emissive.b = color[2];
	changed = 1;
} // end Material::SetEmissive



/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetEmissive
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Material::GetEmissive(void) const
{
	D3DVECTOR	ret;
	ret[0] = Mat.emissive.r;
	ret[1] = Mat.emissive.g;
	ret[2] = Mat.emissive.b;
	return ret;
} // end Material::GetEmissive


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetSpecular
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetSpecular(const D3DVECTOR& color)
{
	Mat.specular.r = color[0];
	Mat.specular.g = color[1];
	Mat.specular.b = color[2];
	changed = 1;
} // end Material::SetSpecular


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetSpecular
**-----------------------------------------------------------------------------
*/

inline D3DVECTOR	
Material::GetSpecular(void) const
{
	D3DVECTOR	ret;
	ret[0] = Mat.specular.r;
	ret[1] = Mat.specular.g;
	ret[2] = Mat.specular.b;
	return ret;
} // end Material::GetSpecular



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetPower
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetPower(const D3DVALUE& power)
{
	Mat.power = power;
	changed = 1;
} // end Material::SetPower


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetPower
**-----------------------------------------------------------------------------
*/

inline D3DVALUE	
Material::GetPower(void) const
{
	return Mat.power;
} // end Material::GetPower


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetRampSize
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetRampSize(const DWORD& ramp)
{
	Mat.dwRampSize = ramp;
	changed = 1;
} // end Material::SetRampSize


  
/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetRampSize
**-----------------------------------------------------------------------------
*/

inline DWORD		
Material::GetRampSize(void) const
{
	return Mat.dwRampSize;
} // end Material::GetRampSize



/*
**-----------------------------------------------------------------------------
**  Name:	Material::SetTextureHandle
**-----------------------------------------------------------------------------
*/

inline void		
Material::SetTextureHandle(const D3DTEXTUREHANDLE& hTexture)
{
	Mat.hTexture = hTexture;
	changed = 1;
} // end Material::SetTextureHandle


/*
**-----------------------------------------------------------------------------
**  Name:	Material::GetTextureHandle
**-----------------------------------------------------------------------------
*/

inline D3DTEXTUREHANDLE	
Material::GetTextureHandle(void)
{
	Set();
	return Mat.hTexture;
} // end Material::GetTextureHandle

  
/*
**-----------------------------------------------------------------------------
**  end of File
**-----------------------------------------------------------------------------
*/
