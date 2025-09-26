// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __ATMOCONTROLS_H
#define __ATMOCONTROLS_H

#define ATM_SLIDER_COUNT 20
#define ATM_DATA_COUNT 24


typedef struct ScatterParams {
	ScatterParams();			///< Defaut c'tor
	union {
		double data[ATM_DATA_COUNT];  // ATTENTION: Order of params must match with slider indexes
		struct {
			double tw_dst;		///< 0 Twilight distance
			double green;		///< 1 Green wavw length
			double tw_bri;		///< 2 Twilight brightness
			double rpow;		///< 3 Rayleigh power
			double rayrat;		///< 4 Rayleigh ratio
			double ray;			///< 5 Rayleigh out-scatter strength
			double tw_bld;		///< 6 Building ambient level at twilight
			double mie;			///< 7 scale factor for mie out-scattering
			double mphase;		///< 8 g-constant in HG phase function
			double rheight;		///< 9 atmospheric rayleigh scale height
			double aux2;		///< 10 cloud lighting altitude [km]
			double mheight;		///< 11 Mie scale height
			double mpow;		///< 12 Mie power
			double trb;			///< 13 Terrain brightness
			double mierat;		///< 14 Mie ratio
			double aux3;		///< 15 auxiliary parameter
			double tgamma;		///< 16 Terrain gamma
			double mphaseb;		///< 17 MiePhase-B
			double hazei;		///< 18 cloud intensity
			double tr3D;		///< 19 Terrain light and shadow boost
			// PAGE 1
			double wnrml;		///< 20 Water normal strength
			double wspec;		///< 21 Water specular color
			double wtrans;		///< 22 Water color / transparency
			double wboost;		///< 23 Terrain light and shadow boost	
		};
	};
	double orbalt;
	double visalt;
	double red;
	double blue;
	double suni;
	FVECTOR3 wcolor;	// Water color
	FVECTOR3 zcolor;	// sun-glare color at zenith (camera at sealevel)
	FVECTOR3 hcolor;	// sun-glare color at horizon (camera at sealevel)
	FVECTOR3 acolor;	// Abmient color at sealevel
	double cfg_alt;
	double cfg_halt;
} ScatterParams;

struct sValue {
	double min, max;
	WORD id;			// User id
	WORD sprm;		// Value index (ScatterTable)
	WORD style;
	WORD page;
	WORD slider;
	string lbl;
	string tooltip;
};

struct sSlider {
	HWND hWnd;
	HWND hwndTip;	// ToolTip
	int res;		// Slider resource id
	int dsp;		// Slider display resource id
	int lbl;		// Slider label resource id 
	sValue* val;
};


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

	void		InitPage(int p);
	double		GetValue(int id);
	void		UpdateSlider(sSlider& s, bool bSetPos = true);
	void		ConfigValue(int sid, int vid, int pid, string lbl, double min, double max, int style = 0);
	void		SetSlider(sSlider& s);
	void		UpdateSliders();
	bool		Visualize();

	INT_PTR CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__ATMOCONTROLS_H
