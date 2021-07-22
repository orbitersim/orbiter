// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Map
// planet map with information about selected landing site

#ifndef __MFD_MAP_H
#define __MFD_MAP_H

#include "Mfd.h"

#define INSTRMAP_NPROJPT 64  // number of points for orbit projections

class Instrument_MapOld: public Instrument {
public:
	Instrument_MapOld (Pane *_pane, int _id, const Spec &spec, Vessel *_vessel);
	~Instrument_MapOld();
	int Type () const { return MFD_MAP; }
	char ModeSelKey () const { return 'M'; }
	HELPCONTEXT *HelpTopic () const;
	bool KeyBuffered (DWORD key);
	bool KeyImmediate (char *kstate);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	void UpdateMap ();
	void UpdateBlt ();
	void UpdateDraw (oapi::Sketchpad *skp);
	int ProcessMessage (int msg, void *data);
	void SetSize (const Spec &spec);

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	void LoadMap (Planet *p);
	SURFHANDLE LoadBitmap (const char *cbuf, int *w, int *h);
	void SetZoom (bool _zoom);
	void ToggleTrack ();
	void CalcCoords (double lng, double lat, int &x, int &y);
	void CalcTargetCoords();
	void CalcOrbitProj (const Elements *el, const Planet *planet, oapi::IVECTOR2 *p);
	bool CalcIntersect (const Elements *el, const Planet *planet, double rad, oapi::IVECTOR2 *is1, oapi::IVECTOR2 *is2);
	bool SelectTarget (const char *str);
	bool SelectMap (char *str);
	char title[50];
	char datastr[2][50];
	Planet *refplanet;
	const RigidBody *otgt;// target orbiter
	const Base *btgt;     // target base
	SURFHANDLE map, bgmap;
	bool needmap;      // request map load
	bool zoom;         // map zoomed in?
	bool track;        // track ship?
	int mapw, maph;    // map surface size
	int mapx, mapy;    // map pixel coordinates of top left corner of displayed area
	int tgtx, tgty, shpx, shpy;
	//oapi::Pen *pen[4];
	oapi::Brush *brush[2];
	POINT proj1[INSTRMAP_NPROJPT+2];
	double cosp[INSTRMAP_NPROJPT], sinp[INSTRMAP_NPROJPT];
	static bool ClbkSubmn_Target (Select*, int, char*, void*);
	static bool ClbkEnter_Target (Select*, int, char*, void*);
	static bool ClbkName_Target (InputBox*, char *str, void *data);
	static bool ClbkEnter_Map (Select *menu, int item, char *str, void *data);

	static struct SavePrm { // instrument exit status
		Vessel *usr;
		Planet *ref;
		const Base *btgt;
		const RigidBody *otgt;
		bool zoom;
		bool track;
		float cx, cy;
	} saveprm;
};

#endif // !__MFD_MAP_H