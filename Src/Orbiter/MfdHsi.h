// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_HSI
// Horizontal situation indicator: instrument for VOR and ILS navigation

#ifndef __MFD_HSI_H
#define __MFD_HSI_H

#include "Mfd.h"

class Instrument_HSI: public Instrument {
public:
	Instrument_HSI (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel);
	~Instrument_HSI();
	int Type () const { return MFD_HSI; }
	char ModeSelKey () const { return 'H'; }
	HELPCONTEXT *HelpTopic () const;
	void  UpdateDraw (oapi::Sketchpad *skp);
	void SetSize (const Spec &spec);
	bool KeyBuffered (DWORD key);
	bool KeyImmediate (char *kstate);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;

protected:
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	int focus;     // current focus HSI display (0,1)
	struct HSI {
		DWORD nv;   // slaved NAV receiver
		double obs; // omni bearing selector angle
	} hsi[2];
	int CNTX[2], CNTY[2], U1, U2, U3, U4, R0, R1, YLN1, YLN2, YLN3, YLN4;
	POINT plabel[12];
	oapi::IVECTOR2 pt[118], *psym, *pgyro, *parrow;
	oapi::Pen *pen[2];
	oapi::Brush *brush;
	static struct SavePrm {
		Vessel *usr;
		HSI hsi[2];
	} saveprm;
};

#endif // !__MFD_HSI_H
