// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_OPlaneAlign
// instrument for alignment of orbital planes

#ifndef __MFD_ALIGN_H
#define __MFD_ALIGN_H

#include "Mfd.h"

class Instrument_OPlaneAlign: public Instrument {
public:
	Instrument_OPlaneAlign (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore);
	virtual ~Instrument_OPlaneAlign();
	int Type () const { return MFD_OPLANEALIGN; }
	char ModeSelKey () const { return 'A'; }
	HELPCONTEXT *HelpTopic () const;
	void SetCustomEls (double i, double theta); // set custom target elements
	void UpdateDraw (oapi::Sketchpad *skp);
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	int ProcessMessage (int msg, void *data);

protected:
	bool GetTimingsFromSurface(double &Tan, double &Aan, double &Tdn, double &Adn, double &VSurf);
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	static bool ClbkEnter_Ref (Select *menu, int item, char *str, void *data);
	static bool ClbkEnter_Tgt (Select *menu, int item, char *str, void *data);
	static bool CallbackElements (InputBox *inp, char *str, void *data);
	bool SelectRef (char *name);
	void SelectAutoRef ();
	void SetRef (const CelestialBody *cbody);
	bool SelectTarget (char *name);
	void SetTarget (const Body *target);        // set target body for target elements
	void CycleModes();

	enum Mode { ORBIT, BALLISTIC, SURFACE } mode;
	bool automode;
	Elements *shpel;              // ship elements (private to allow customisation)
	Elements *tgtel;              // target elements
	const CelestialBody *elref;   // element reference body
	const Body *tgt;              // current target object (0 for custom elements)

	double pphi;
	double preli;
	bool customel; // use custom elements as target?
	bool engage;   // true if thrusters should currently be engaged
	bool isasc;    // current node is ascending
	double refrad; // radius of reference body

	static struct SavePrm {
		Vessel *usr;
		const CelestialBody *elref;
		const Body *tgt;
		bool customel;
		double i, theta;
	} saveprm;
};

#endif // !__MFD_ALIGN_H