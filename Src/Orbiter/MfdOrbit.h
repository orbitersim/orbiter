// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Orbit
// orbital information

#ifndef __MFD_ORBIT_H
#define __MFD_ORBIT_H

#include "Mfd.h"

class Instrument_Orbit: public Instrument {
public:
	enum ProjectionMode { PRJ_FRM, PRJ_SHIP, PRJ_TGT };
	enum FrameMode { FRM_ECL, FRM_EQU };
	enum DistMode { DIST_RAD, DIST_ALT };
	enum DisplayMode { DISP_LIST, DISP_GRAPH, DISP_BOTH };
	Instrument_Orbit (Pane *_pane, INT_PTR id, const Spec &spec, Vessel *_vessel);
	virtual ~Instrument_Orbit ();
	int Type () const { return MFD_ORBIT; }
	char ModeSelKey () const { return 'O'; }
	HELPCONTEXT *HelpTopic () const;
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	void UpdateDraw (oapi::Sketchpad *skp);
	int ProcessMessage (int msg, void *data);
	void SetSize (const Spec &spec);

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	void DisplayOrbit (oapi::Sketchpad *skp, int which, oapi::IVECTOR2 *p);
	void DisplayElements (oapi::Sketchpad *skp, const Elements *el, int x, int y);
	void SetRef (const CelestialBody *ref);
	bool SelectRef (char *str);
	void SelectAutoRef ();
	bool SelectTarget (char *str);
	void UnselectTarget ();
	void CopyToHUD () const;
	static bool ClbkEnter_Tgt (Select*, int, char*, void*);
	static bool ClbkEnter_Ref (Select *menu, int item, char *str, void *data);

	int ICNTX, ICNTY; // instrument centre
	char title_str[40], proj_str[20];
	oapi::IVECTOR2 o_pt1[ELN+5], o_pt2[ELN+5]; // sampling points for orbit ellipses

	Elements *shpel;              // ship elements (private to allow customisation)
	Elements *tgtel;              // target elements
	const CelestialBody *elref;   // element reference body
	const Body *tgt;              // current target object


	ProjectionMode projmode; // plane into which to project orbit displays
	FrameMode frmmode;       // frame of reference for elements
	DistMode dstmode;        // distance displays: radius or altitude
	DisplayMode dispmode;
	bool instable; // true if periapsis distance is smaller than planet radius
	double scale;  // scaling factor for world->screen mapping
	int pixrad;    // max pixel radius for orbit display
	int elref_rad; // pixel radius of central object

	// GDI resources
	oapi::Brush *brush[2];

	static struct SavePrm {
		Vessel *usr;
		const CelestialBody *elref;
		const Body *tgt;
		ProjectionMode projmode;
		FrameMode frmmode;
		DistMode dstmode;
		DisplayMode dispmode;
	} saveprm;
};

#endif // !__MFD_ORBIT_H
