// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Particle system for exhaust contrails

#ifndef __PARTICLE_H
#define __PARTICLE_H

#include "OGraphics.h"
#include "Vecmat.h"
#include "D3dmath.h"

#define MAXPARTICLE 3000

class Vessel;
class Planet;

struct ParticleSpec {
	VECTOR3 pos, vel;
	double size;
	double alpha0;  // alpha value at creation
	double t0;
	int texidx;
	DWORD flag;
	ParticleSpec *prev, *next;
};

class D3D7ParticleStream: public oapi::ParticleStream {
public:
	D3D7ParticleStream (oapi::GraphicsClient *_gc, PARTICLESTREAMSPEC *pss = 0);
	virtual ~D3D7ParticleStream();

	void SetObserverRef (const VECTOR3 *cam);
	void SetSourceRef (const VECTOR3 *src);
	void SetSourceOffset (const VECTOR3 &ofs);
	void SetIntensityLevelRef (double *lvl);

	void Activate (bool _active) { active = _active; }
	// activate/deactivate the particle source

	void Timejump ();
	// register a discontinuity

	bool Expired () const { return !level && !np; }
	// stream is dead

	double MinCameraDist() const;
	// return the minimum distance of any particle from the camera

	ParticleSpec *CreateParticle (const VECTOR3 &pos, const VECTOR3 &vel, double size, double alpha);
	void DeleteParticle (ParticleSpec *p);
	virtual void Update ();
	void Render (LPDIRECT3DDEVICE7 dev, LPDIRECTDRAWSURFACE7 &prevtex);
	virtual void RenderGroundShadow (LPDIRECT3DDEVICE7 dev, LPDIRECTDRAWSURFACE7 &prevtex) {}

protected:
	void SetSpecs (PARTICLESTREAMSPEC *pss);
	void SetParticleHalflife (double pht);
	double Level2Alpha (double level) const; // map a level (0..1) to alpha (0..1) for given mapping
	double Atm2Alpha (double prm) const; // map atmospheric parameter (e.g. density) to alpha (0..1) for given mapping
	void SetDParticleCoords (const VECTOR3 &ppos, double scale, D3DVERTEX *vtx);
	void SetEParticleCoords (const VECTOR3 &ppos, double scale, POSTEXVERTEX *vtx);
	void SetShadowCoords (const VECTOR3 &ppos, const VECTOR3 &cdir, double scale, POSTEXVERTEX *vtx);
	void CalcNormals (const VECTOR3 &ppos, D3DVERTEX *vtx);
	virtual void SetMaterial (D3DCOLORVALUE &col) { col.r = col.g = col.b = 1; }
	void RenderDiffuse (LPDIRECT3DDEVICE7 dev);
	void RenderEmissive (LPDIRECT3DDEVICE7 dev);
	const VECTOR3 *cam_ref;
	const VECTOR3 *src_ref;
	VECTOR3 src_ofs;
	double interval;
	double exp_rate;
	double pdensity;
	double speed; // emission velocity
	double vrand; // velocity randomisation
	double alpha; // particle growth rate
	double beta;  // atmospheric slowdown rate
	double size0;  // particle base size at creation
	double t0; // time of last particle created
	bool active;   // source emitting particles?
	bool diffuse; // particles have diffuse component (need normals)
	mutable double min_cam_dist; // minimum camera distance of any particle in the stream

	PARTICLESTREAMSPEC::LEVELMAP lmap; // level mapping method
	double lmin, lmax;                 // used for level mapping
	PARTICLESTREAMSPEC::ATMSMAP amap;  // atmosphere mapping method
	double amin, afac;                 // used for atmosphere mapping

	ParticleSpec *pfirst, *plast;
	int np; // number of current particles
	int stride; // number of particles rendered simultaneously
	D3DMATRIX mWorld;
	LPDIRECTDRAWSURFACE7 tex; // particle texture

//private:
	double ipht2;
};

class ExhaustStream: public D3D7ParticleStream {
public:
	ExhaustStream (oapi::GraphicsClient *_gc, OBJHANDLE hV,
		const double *srclevel, const VECTOR3 *thref, const VECTOR3 *thdir,
		PARTICLESTREAMSPEC *pss = 0);
	ExhaustStream (oapi::GraphicsClient *_gc, OBJHANDLE hV,
		const double *srclevel, const VECTOR3 &ref, const VECTOR3 &_dir,
		PARTICLESTREAMSPEC *pss = 0);
	void RenderGroundShadow (LPDIRECT3DDEVICE7 dev, LPDIRECTDRAWSURFACE7 &prevtex);
	void Update ();

private:
	OBJHANDLE hPlanet;
};

class ReentryStream: public D3D7ParticleStream {
public:
	ReentryStream (oapi::GraphicsClient *_gc, OBJHANDLE hV,
		PARTICLESTREAMSPEC *pss = 0);
	void Update ();

protected:
	void SetMaterial (D3DCOLORVALUE &col);

private:
	OBJHANDLE hPlanet;
	double llevel;
};

#endif // !__PARTICLE_H