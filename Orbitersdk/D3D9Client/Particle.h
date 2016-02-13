// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __PARTICLE_H
#define __PARTICLE_H

#include "D3D9Effect.h"
#include "D3D9Client.h"
#include "D3D9Util.h"

#define MAXPARTICLE 3000

struct ParticleSpec {
	VECTOR3 pos, vel;
	double size;
	double alpha0;  // alpha value at creation
	double t0;
	int texidx;
	DWORD flag;
	ParticleSpec *prev, *next;
};

class D3D9ParticleStream : public oapi::ParticleStream, public D3D9Effect
{

public:
	/**
	 * \brief Constructs a new particle stream object
	 * \param _gc pointer to graphics client
	 * \param pss particle stream parameters
	 */
	D3D9ParticleStream (oapi::GraphicsClient *_gc, PARTICLESTREAMSPEC *pss = 0);

	/**
	 * \brief Destroys the particle stream object
	 */
	virtual ~D3D9ParticleStream();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient pointer to graphics client
	 */
	static void GlobalInit(oapi::D3D9Client *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	void SetObserverRef (const VECTOR3 *cam);
	void SetSourceRef (const VECTOR3 *src);
	void SetSourceOffset (const VECTOR3 &ofs);
	void SetIntensityLevelRef (double *lvl);

	//void Activate (bool _active) { active = _active; }
	// activate/deactivate the particle source

	bool IsActive() { return (pfirst!=NULL); }

	void Timejump ();
	// register a discontinuity

	bool Expired () const { return !level && !np; }
	// stream is dead

	ParticleSpec *CreateParticle (const VECTOR3 &pos, const VECTOR3 &vel, double size, double alpha);

	void DeleteParticle (ParticleSpec *p);
	virtual void Update ();

	void   Render(LPDIRECT3DDEVICE9 dev);
	//void Render(LPDIRECT3DDEVICE9 dev, LPDIRECT3DTEXTURE9 &prevtex);
	
	virtual void RenderGroundShadow (LPDIRECT3DDEVICE9 dev, LPDIRECT3DTEXTURE9 &prevtex) {}


	ParticleSpec * GetPlast() { return plast; }

protected:

	void SetSpecs (PARTICLESTREAMSPEC *pss);
	void SetParticleHalflife (double pht);
	double Level2Alpha (double level) const; // map a level (0..1) to alpha (0..1) for given mapping
	double Atm2Alpha (double prm) const; // map atmospheric parameter (e.g. density) to alpha (0..1) for given mapping
	void SetDParticleCoords(const VECTOR3 &ppos, double scale, NTVERTEX *vtx);
	void SetEParticleCoords(const VECTOR3 &ppos, double scale, VERTEX_XYZ_TEX *vtx);
	void SetShadowCoords(const VECTOR3 &ppos, const VECTOR3 &cdir, double scale, VERTEX_XYZ_TEX *vtx);
	void CalcNormals(const VECTOR3 &ppos, NTVERTEX *vtx);
	virtual void SetMaterial (D3DCOLORVALUE &col) { col.r = col.g = col.b = 1; }
	void RenderDiffuse (LPDIRECT3DDEVICE9 dev);
	void RenderEmissive (LPDIRECT3DDEVICE9 dev);
	//const VECTOR3 *cam_ref;
	//const VECTOR3 *src_ref;
	//VECTOR3 src_ofs;
	double interval;
	double exp_rate;
	double pdensity;
	double speed; // emission velocity
	double vrand; // velocity randomisation
	double alpha; // particle growth rate
	double beta;  // atmospheric slowdown rate
	double size0;  // particle base size at creation
	double t0; // time of last particle created
	//bool active;   // source emitting particles?
	bool diffuse; // particles have diffuse component (need normals)

	PARTICLESTREAMSPEC::LEVELMAP lmap; // level mapping method
	double lmin, lmax;                 // used for level mapping
	PARTICLESTREAMSPEC::ATMSMAP amap;  // atmosphere mapping method
	double amin, afac;                 // used for atmosphere mapping

	ParticleSpec *pfirst, *plast;
	int np; // number of current particles
	int stride; // number of particles rendered simultaneously
	D3DXMATRIX mWorld; // ground shadow related matrix
	LPD3D9CLIENTSURFACE tex; // particle texture
	double ipht2;

protected:
	oapi::D3D9Client *pGC;					// pointer to graphics client
	static LPD3D9CLIENTSURFACE deftex;		// default particle texture
	static LPD3D9CLIENTSURFACE deftexems;	// default particle texture
	static bool bShadows;					// render particle shadows
};

class ExhaustStream: public D3D9ParticleStream {
public:
	ExhaustStream (oapi::GraphicsClient *_gc, OBJHANDLE hV,
		const double *srclevel, const VECTOR3 *thref, const VECTOR3 *thdir,
		PARTICLESTREAMSPEC *pss = 0);
	ExhaustStream (oapi::GraphicsClient *_gc, OBJHANDLE hV,
		const double *srclevel, const VECTOR3 &ref, const VECTOR3 &_dir,
		PARTICLESTREAMSPEC *pss = 0);
	void RenderGroundShadow (LPDIRECT3DDEVICE9 dev, LPDIRECT3DTEXTURE9 &prevtex);
	void Update ();

private:
	OBJHANDLE hPlanet;
};

class ReentryStream: public D3D9ParticleStream {
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