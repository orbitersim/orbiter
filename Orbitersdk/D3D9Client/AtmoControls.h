// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================

#ifndef __ATMOCONTROLS_H
#define __ATMOCONTROLS_H

#define ATM_SLIDER_COUNT 14
#define SctPwr 1.0

typedef struct {
	union {
		double data[ATM_SLIDER_COUNT];  // ATTENTION: Order of params must match with slider indexes
		struct {
			double red;			// Red wave length
			double green;		// Green wavw length
			double blue;		// Blue wave length
			double wavepow;		// lambda power
			double rin;			// in-scatter strength
			double rout;		// out-scatter strenght
			double rphase;		// Rayleigh phase
			double mie;			// 
			double mphase;		// g-constant in HG phase function
			double balance;		// Optical depth balance between viewing ray and sunlight
			double height;		// atmospheric scale height
			double sun;			// sun intensity for surface lighting
			double rsun;		// sun intensity for rayleigh inscattering
			double srfclr;		// surface color fine tune
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

#endif // !__DEBUGCONTROLS_H
