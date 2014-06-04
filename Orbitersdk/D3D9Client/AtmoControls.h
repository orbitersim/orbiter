// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================

#ifndef __ATMOCONTROLS_H
#define __ATMOCONTROLS_H

#define ATM_SLIDER_COUNT 17


typedef struct ScatterParams {
	ScatterParams();			///< Defaut c'tor
	union {
		double data[ATM_SLIDER_COUNT];  // ATTENTION: Order of params must match with slider indexes
		struct {
			double red;			///< Red wave length
			double green;		///< Green wavw length
			double blue;		///< Blue wave length
			double wavepow;		///< lambda power
			double rin;			///< in-scatter strength
			double rout;		///< out-scatter strenght
			double rphase;		///< Rayleigh phase
			double mie;			///< scale factor for mie scattering
			double mphase;		///< g-constant in HG phase function
			double balance;		///< Optical depth balance between viewing ray and sunlight
			double height;		///< atmospheric scale height
			double sun;			///< sun intensity for surface lighting
			double depth;		///< depth clamp
			double srfclr;		///< surface color fine tune
			double expo;		///< exposure
			double aux1;		///< unused auxiliary parameter
			double aux2;		///< unused auxiliary parameter
		};
	};
	int mode;
	bool oversat;
} ScatterParams;

class vPlanet;
class vObject;

// ==============================================================

namespace AtmoControls {

	void		Create();
	void		Release();

	void		InitDialog();
	void		OpenDlgClbk(void *context);
	void		SetVisual(vObject *vo);
	vPlanet *	GetVisual();
	bool		IsActive();

	double		GetValue(int id);
	void		UpdateSlider(int id, bool bSetPos = true);
	void		ConfigSlider(int id, double min, double max, int style=0);
	void		SetSlider(int id, WORD pos);
	void		UpdateSliders();

	BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__ATMOCONTROLS_H
