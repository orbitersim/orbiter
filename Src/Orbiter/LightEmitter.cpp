// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// class LightEmitter: Logical (device-independent) definition of light-emitting objects
// Interface in OrbiterAPI.h
//-----------------------------------------------------------------------------

#define OAPI_IMPLEMENTATION
#include "OrbiterAPI.h"
#include "Vessel.h"

extern char DBG_MSG[256];

//-----------------------------------------------------------------------------

LightEmitter::LightEmitter ()
{
	ltype = LT_NONE;
	visibility = VIS_EXTERNAL;
	hRef = NULL;
	active = false;
	col_diff.r = col_diff.g = col_diff.b = col_diff.a = 1.0f;
	col_spec.r = col_spec.g = col_spec.b = col_spec.a = 1.0f;
	col_ambi.r = col_ambi.g = col_ambi.b = col_ambi.a = 1.0f;
	lpos = _V(0,0,0);  pos = &lpos;
	ldir = _V(0,0,1);  dir = &ldir;
	lintens = 1.0;     intens = &lintens;
}

//-----------------------------------------------------------------------------

LightEmitter::LightEmitter (COLOUR4 diffuse, COLOUR4 specular, COLOUR4 ambient)
{
	ltype = LT_NONE;
	visibility = VIS_EXTERNAL;
	hRef = NULL;
	active = false;
	col_diff = diffuse;
	col_spec = specular;
	col_ambi = ambient;
	lpos = _V(0,0,0);  pos = &lpos;
	ldir = _V(0,0,1);  dir = &ldir;
	lintens = 1.0;     intens = &lintens;
}

//-----------------------------------------------------------------------------

void LightEmitter::Activate (bool act)
{
	if (act != active) {
		active = act;
		if (hRef && ((Body*)hRef)->Type() == OBJTP_VESSEL) {
			((Vessel*)hRef)->LightEmitterState (this, 0, (void*)&active);
		}
	}
}

//-----------------------------------------------------------------------------

bool LightEmitter::IsActive () const
{
	return active;
}

//-----------------------------------------------------------------------------

OBJHANDLE LightEmitter::Attach (OBJHANDLE hObj)
{
	OBJHANDLE hOld = hRef;
	hRef = hObj;
	return hOld;
}

//-----------------------------------------------------------------------------

OBJHANDLE LightEmitter::Detach ()
{
	OBJHANDLE hOld = hRef;
	hRef = NULL;
	return hOld;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetPosition (const VECTOR3 &p)
{
	lpos = p;
	pos = &lpos;
}

//-----------------------------------------------------------------------------

void LightEmitter::ShiftExplicitPosition (const VECTOR3 &ofs)
{
	lpos += ofs;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetPositionRef (const VECTOR3 *p)
{
	pos = p;
}

//-----------------------------------------------------------------------------

const VECTOR3 *LightEmitter::GetPositionRef () const
{
	return pos;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetDirection (const VECTOR3 &d)
{
	ldir = d;
	dir = &ldir;
}

//-----------------------------------------------------------------------------

VECTOR3 LightEmitter::GetDirection () const
{
	return *dir;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetDirectionRef (const VECTOR3 *d)
{
	dir = d;
}

//-----------------------------------------------------------------------------

const VECTOR3 *LightEmitter::GetDirectionRef () const
{
	return dir;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetIntensity (double in)
{
	lintens = in;
	intens = &lintens;
}

//-----------------------------------------------------------------------------

double LightEmitter::GetIntensity () const
{
	return *intens;
}

//-----------------------------------------------------------------------------

void LightEmitter::SetIntensityRef (double *pin)
{
	intens = pin;
}

//-----------------------------------------------------------------------------

const double *LightEmitter::GetIntensityRef () const
{
	return intens;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

PointLight::PointLight (OBJHANDLE hObj, const VECTOR3 &_pos, double _range, double att0, double att1, double att2)
: LightEmitter ()
{
	ltype = LT_POINT;
	Attach (hObj);
	SetPosition (_pos);
	range = _range;
	att[0] = att0;
	att[1] = att1;
	att[2] = att2;
	Activate (true);
}

//-----------------------------------------------------------------------------

PointLight::PointLight (OBJHANDLE hObj, const VECTOR3 &_pos, double _range, double att0, double att1, double att2, COLOUR4 diffuse, COLOUR4 specular, COLOUR4 ambient)
: LightEmitter (diffuse, specular, ambient)
{
	ltype = LT_POINT;
	Attach (hObj);
	SetPosition (_pos);
	range = _range;
	att[0] = att0;
	att[1] = att1;
	att[2] = att2;
	Activate (true);
}

//-----------------------------------------------------------------------------

void PointLight::SetRange (double _range)
{
	range = _range;
}

//-----------------------------------------------------------------------------

void PointLight::SetAttenuation (double att0, double att1, double att2)
{
	att[0] = att0;
	att[1] = att1;
	att[2] = att2;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

SpotLight::SpotLight (OBJHANDLE hObj, const VECTOR3 &_pos, const VECTOR3 &_dir, double _range, double att0, double att1, double att2, double _umbra, double _penumbra)
: PointLight (hObj, _pos, _range, att0, att1, att2)
{
	ltype = LT_SPOT;
	SetDirection (_dir);
	umbra = _umbra;
	penumbra = _penumbra;
}

//-----------------------------------------------------------------------------

SpotLight::SpotLight (OBJHANDLE hObj, const VECTOR3 &_pos, const VECTOR3 &_dir, double _range, double att0, double att1, double att2, double _umbra, double _penumbra, COLOUR4 diffuse, COLOUR4 specular, COLOUR4 ambient)
: PointLight (hObj, _pos, _range, att0, att1, att2, diffuse, specular, ambient)
{
	ltype = LT_SPOT;
	SetDirection (_dir);
	umbra = _umbra;
	penumbra = _penumbra;
}

//-----------------------------------------------------------------------------

void SpotLight::SetAperture (double _umbra, double _penumbra)
{
	umbra = _umbra;
	penumbra = _penumbra;
}
