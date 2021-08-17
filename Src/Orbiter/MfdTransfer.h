// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Transfer
// Transfer trajectory

#ifndef __MFD_TRANSFER_H
#define __MFD_TRANSFER_H

#include "Mfd.h"

class Instrument_Transfer: public Instrument {
public:
	Instrument_Transfer (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel);
	virtual ~Instrument_Transfer ();
	int Type() const { return MFD_TRANSFER; }
	char ModeSelKey () const { return 'X'; }
	HELPCONTEXT *HelpTopic () const;
	void UpdateDraw (oapi::Sketchpad *skp);
	void SetSize (const Spec &spec);
	bool KeyBuffered (DWORD key);
	bool KeyImmediate (char *kstate);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	int ProcessMessage (int msg, void *data);

protected:
	bool Update (double upDTscale);
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	double CalcElements (const Elements *el1, Elements *el2, double lng, double a);
	void DisplayOrbit (oapi::Sketchpad *skp, oapi::IVECTOR2 *p) const;
	//bool CalcNumTrajectory (const Elements *el, double T);
	bool CalcStep ();
	bool InitNumTrajectory (const Elements *el);
	bool SelectTarget (char *str);
	bool SelectRef (char *str);
	bool SelectSrc (char *str);
	bool SetNstep (int np);
	static bool ClbkEnter_Tgt (Select*, int, char*, void*);
	static bool ClbkEnter_Ref (Select *menu, int item, char *str, void *data);
	static bool ClbkEnter_Src (Select *menu, int item, char *str, void *data);
	static bool ClbkNstep (InputBox *inp, char *str, void *data);

	const CelestialBody *elref; // element reference body
	const RigidBody *src;   // target orbit body
	const RigidBody *tgt;   // transfer target
	Elements *shpel;   // orbital elements of ship (private copy - allows arbitrary reference body)
	Elements *shpel2;  // hypothetical ship orbit
	Elements *tgtel;   // orbital elements of target (private copy)
	bool enable_hyp;   // toggle hypothetical orbit
	double l_eject;    // longitude of ejection point
	double deltav;     // ejection delta-v
	double hto_a;      // semi-major axis of transfer orbit
	int ICNTX, ICNTY, pixrad;    // instrument centre, orbit radius [pixel]
	double tbdown, tbpress;      // time of last button down/button press event
	bool dv_manip;               // manual manipulation of delta-v?

	// numerical trajectory data
	bool enable_num; // toggle numerical trajectory
	bool process_num;
	int nstep;    // number of time steps
	int step_curr;
	double step_scale;
	double step_t, step_0;
	Vector step_gpos, step_gvel;
	Vector *path; // trajectory path
	oapi::IVECTOR2 *pathp; // screen mapping of trajectory path

	static struct SavePrm {
		Vessel *usr;
		const CelestialBody *elref;
		const RigidBody *src;
		const RigidBody *tgt;
		double l_eject;
		double deltav;
		bool enable_hyp;
		bool enable_num;
	} saveprm;
};

#endif // !__MFD_TRANSFER_H