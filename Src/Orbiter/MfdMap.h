// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Map
// planet map with information about selected landing site

#ifndef __MFD_MAP2_H
#define __MFD_MAP2_H

#include "Mfd.h"
#include "VectorMap.h"

#define INSTRMAP_NPROJPT 64  // number of points for orbit projections

class Instrument_Map: public Instrument {
public:
	Instrument_Map (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel);
	~Instrument_Map();
	int Type () const { return MFD_MAP; }
	char ModeSelKey () const { return 'M'; }
	HELPCONTEXT *HelpTopic () const;
	int ProcessMessage (int msg, void *data);
	bool KeyBuffered (DWORD key);
	bool KeyImmediate (char *kstate);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	bool Update (double upDTscale);
	void UpdateDraw (oapi::Sketchpad *skp);
	void UpdateBlt ();
	void SetSize (const Spec &spec);

protected:
	void UpdateDraw_Map (oapi::Sketchpad *skp);
	void UpdateDraw_Dispprm (oapi::Sketchpad *skp);

	bool SelectMap (char *str);
	bool SelectTarget (char *str);
	bool UnselectTarget ();
	void ZoomOut();
	void ZoomIn();
	void ToggleTrack();
	bool ToggleDispParam (int which);

	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	static bool ClbkEnter_Map (Select *menu, int item, char *str, void *data);
	static bool ClbkSubmn_Target (Select*, int, char*, void*);
	static bool ClbkEnter_Target (Select*, int, char*, void*);
	static bool ClbkName_Target (InputBox*, char *str, void *data);

	VectorMap *map;
	const Planet *refplanet;
	int zoom;
	bool track;
	char title[50];
	double scroll_t0, scroll_tp;  // time of scroll start (for acceleration computation)

	int disp_mode;   // MFD display mode: 0=map, 1=display parameter page
	DWORD dispflag;  // bitflags for display elements
	int disp_sel;    // current selection for editing display parameters
	int ndispprm;    // total number of selectable parameters
	int dispprm_top; // index of topmost list entry (for scrolling)

	static struct SavePrm { // instrument exit status
		Vessel *usr;
		const Planet *ref;
		VectorMap::OBJTYPE sel;
		int zoom;
		bool track;
		double lng, lat;
		DWORD dispflag;
		DWORD mkrflag;
	} saveprm;
};

#endif // !__MFD_MAP2_H