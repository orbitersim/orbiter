// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_OSync
// Synchronise orbit with target object (assuming same orbital plane)

#ifndef __MFD_SYNC_H
#define __MFD_SYNC_H

#include "Mfd.h"

class Instrument_OSync: public Instrument {
	friend class Pane;
public:
	Instrument_OSync (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore);
	virtual ~Instrument_OSync ();
	int Type () const { return MFD_OSYNC; }
	char ModeSelKey () const { return 'Y'; }
	HELPCONTEXT *HelpTopic () const;
	void UpdateDraw (oapi::Sketchpad *skp);
	void SetSize (const Spec &spec);
	bool KeyBuffered (DWORD key);
	bool KeyImmediate (char *kstate);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	enum Mode { MODE_INTERSECT1, MODE_INTERSECT2, MODE_SHIP_PA, MODE_SHIP_AA, MODE_TGT_PA, MODE_TGT_AA, MODE_MANUAL };
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	int ProcessMessage (int msg, void *data);

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	static bool ClbkEnter_Tgt (Select *menu, int item, char *str, void *data);
	static bool CallbackNorbit (InputBox *inp, char *str, void *data);
	void SetTarget (const RigidBody *target);        // set target body for target elements
	const RigidBody *tgt;        // target object
	int ICNTX, ICNTY, pixrad;    // instrument centre, orbit radius [pixel]
	int norbit;                  // number of orbit transit times to list
	Mode mode;
	double man_rlng, man_sinr, man_cosr; // manual reference direction

	static struct SavePrm {
		Vessel *usr;
		const RigidBody *tgt;
		Mode mode;
		double man_rlng;
		int norbit;
	} saveprm;
};

#endif // !__MFD_SYNC_H