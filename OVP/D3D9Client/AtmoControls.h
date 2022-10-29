// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __ATMOCONTROLS_H
#define __ATMOCONTROLS_H

#define ATM_SLIDER_COUNT 19


typedef struct ScatterParams {
	ScatterParams();			///< Defaut c'tor
	union {
		double data[ATM_SLIDER_COUNT];  // ATTENTION: Order of params must match with slider indexes
		struct {
			double tw_dst;		///< 0 Red wave length
			double tw_haze;		///< 1 Green wavw length
			double tw_bri;		///< 2 Blue wave length
			double rpow;		///< 3 Rayleigh power
			double rayin;		///< 4 Rayleigh in-scatter strength
			double ray;			///< 5 Rayleigh out-scatter strength
			double rphase;		///< 6 Rayleigh phase
			double mie;			///< 7 scale factor for mie out-scattering
			double mphase;		///< 8 g-constant in HG phase function
			double rheight;		///< 9 atmospheric rayleigh scale height
			double aux2;		///< 10 auxiliary parameter
			double mheight;		///< 11 Mie scale height
			double mpow;		///< 12 Mie power
			double trb;			///< 13 Terrain brightness
			double miein;		///< 14 Mie in-scatter strenght
			double aux3;		///< 15 auxiliary parameter
			double tgamma;		///< 16 Terrain gamma
			double mphaseb;		///< 17 MiePhase-B
			double hazei;		///< 18 Haze intensity
		};
	};
	double orbalt;
	double visalt;
	double red;
	double green;
	double blue;
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
	bool		Visualize();

	INT_PTR CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__ATMOCONTROLS_H
