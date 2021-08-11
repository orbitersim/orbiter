// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Docking
// instrument for docking at orbital stations

#ifndef __MFD_DOCKING_H
#define __MFD_DOCKING_H

#include "Mfd.h"

class Instrument_Docking: public Instrument {
public:
	Instrument_Docking (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore);
	virtual ~Instrument_Docking ();
	int Type () const { return MFD_DOCKING; }
	char ModeSelKey () const { return 'D'; }
	HELPCONTEXT *HelpTopic () const;
	void SetReference (Vessel *_vessel, DWORD dock);
	void UpdateDraw (oapi::Sketchpad *skp);
	int ProcessMessage (int msg, void *data);
	void SetSize (const Spec &spec);
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;

	enum SensorMode {NAV,VIS,DIRECT}; // data acquisition method (radio signal/visual sensors/direct selection (legacy)

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	void SetupVessel();            // initialise MFD for vessel
	void CopyToHUD () const;
	bool SetTarget (char *str);    // set docking target
	void DrawArrow(int x0, int y0, int dir, oapi::Sketchpad *skp);
	static bool ClbkSelection_Target (Select *menu, int item, char *str, void *data);
	static bool ClbkEnter_Target (Select *menu, int item, char *str, void *data);
	static bool CallbackTarget (InputBox *inp, char *str, void *data);
	SensorMode smode;              // data acquisition method
	int scale;                     // indicator resolution. 0=coarse, 1=fine
	DWORD refdock;                 // reference (source) docking port
	DWORD ndock;                   // number of docking ports on the source
	DWORD nv;                      // slaved NAV receiver
	const Body *Legacy_ref;        // target station/vessel (legacy method)
	int Legacy_port;               // target docking port (legacy method)
	char title[50];
	int circx, circy, circr, bar0, barh, barw, bar0x, bar1x;
	Vector adpos;                  // previous station location in ship approach frame
	double dst;                    // previous station distance
	Matrix dockframe;              // rotation matrix ship local -> ship approach frame
	oapi::Brush *brush[4];

	static struct SavePrm {
		Vessel *usr;
		SensorMode smode;
		DWORD nv;
		const Body *Legacy_ref;
		int Legacy_port;
	} saveprm;
};

#endif // !__MFD_DOCKING_H
