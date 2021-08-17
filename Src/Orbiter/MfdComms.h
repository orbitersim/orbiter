// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Comms
// navigation radio manipulation

#ifndef __MFD_COMMS_H
#define __MFD_COMMS_H

#include "Mfd.h"

class Instrument_Comms: public Instrument {
public:
	Instrument_Comms (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel);
	virtual ~Instrument_Comms ();
	int Type () const { return MFD_COMMS; }
	char ModeSelKey () const { return 'C'; }
	HELPCONTEXT *HelpTopic () const;
	bool Update (double upDTscale);
	void UpdateDraw (oapi::Sketchpad *skp);
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;

protected:
	bool ReadParams (std::ifstream &ifs) { return true; }
	void WriteParams (std::ostream &ofs) const;
	void SwitchFreq (DWORD line, int step, bool minor);

	DWORD sel; // selected radio
	DWORD nsel; // number of selection lines
	int scanning; // -1,1=scanning in progress; 0=no scanning
	double t_scan; // time of last step during scanning

private:
	oapi::Brush *brush[2];
};

#endif // !__MFD_COMMS_H