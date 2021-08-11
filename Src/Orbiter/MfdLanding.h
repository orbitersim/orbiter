// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Landing
// instrument landing system for final approach

#ifndef __MFD_LANDING_H
#define __MFD_LANDING_H

#include "Mfd.h"

class Instrument_Landing: public Instrument {
public:
	Instrument_Landing (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore);
	virtual ~Instrument_Landing();
	int Type () const { return MFD_LANDING; }
	char ModeSelKey () const { return 'L'; }
	HELPCONTEXT *HelpTopic () const;
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	void  UpdateDraw (oapi::Sketchpad *skp);
	void SetSize (const Spec &spec);

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	DWORD nv; // slaved NAV receiver
	char title[50];
	int circx, circy, circr, bar0, barh, barw;
	oapi::Brush *brush[3];

	static struct SavePrm {
		Vessel *usr;
		DWORD nv;
	} saveprm;
};

#endif // !__MFD_LANDING_H