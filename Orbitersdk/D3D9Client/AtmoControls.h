// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __ATMOCONTROLS_H
#define __ATMOCONTROLS_H

#define ATM_SLIDER_COUNT 20


typedef struct ScatterParams {
	ScatterParams();			///< Defaut c'tor
	union {
		double data[ATM_SLIDER_COUNT];  // ATTENTION: Order of params must match with slider indexes
		struct {
			double red;			///< Red wave length
			double green;		///< Green wavw length
			double blue;		///< Blue wave length
			double rpow;		///< Rayleigh power
			double rin;			///< in-scatter strength
			double rout;		///< out-scatter strenght
			double rphase;		///< Rayleigh phase
			double mie;			///< scale factor for mie scattering
			double mphase;		///< g-constant in HG phase function
			double height;		///< atmospheric scale height
			double aux2;		///< auxiliary parameter
			double depth;		///< Cloud layer intensity
			double mpow;		///< Mie power
			double expo;		///< exposure for terrain
			double aux1;		///< auxiliary parameter
			double aux3;		///< auxiliary parameter
			double tgamma;		///< Terrain gamma
			double hazec;		///< Haze color
			double hazei;		///< Haze intensity
			double agamma;		///< Atmosphere gamma	
		};
	};
	bool orbit;
} ScatterParams;

class vPlanet;
class vObject;

// ==============================================================

namespace AtmoControls {

	void		Create();
	void		Release();

	void		OpenDlgClbk(void *context);
	void		SetVisual(vObject *vo);
	vPlanet *	GetVisual();
	bool		IsActive();

	double		GetValue(int id);
	void		UpdateSlider(int id, bool bSetPos = true);
	void		ConfigSlider(int id, double min, double max, int style=0);
	void		SetSlider(int id, WORD pos);
	void		UpdateSliders();

	INT_PTR CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};




/*
class Scatter {

public:
				Scatter(ScatterParams *prm, OBJHANDLE hPlanet, DWORD dAmbient);
				~Scatter();

	double		AngleCoEff(double cos_dir);

	void		ComputeSunLightColorMap(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DTEXTURE9 *pOutSM, bool bSave=false);
	void		ComputeInscatterColorMap(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DTEXTURE9 *pOutSM, bool bSave=false);

private:

	double		fRadius;
	double		fAtmRad;
	double		fMaxDepth;
	double		fHorizonAlt;
	double		fScaleHeight;
	double		fInvScaleHeight;
	double		fCoEff[12];
	double		fMieA, fMieB, fMieC;

	float		fGlobalAmb, fSunAppRad, fAmbient0;

	D3DXVECTOR3	vLambda4, vLambda1;
	D3DXVECTOR3	vRayTot, vMieTot, vRaySun, vRayIns, vRaySrf;

	const ATMCONST *atm;
};*/



#endif // !__ATMOCONTROLS_H
