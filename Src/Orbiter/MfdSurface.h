// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Instrument_Surface
// vessel information related to planetary surface

#ifndef __MFD_SURFACE_H
#define __MFD_SURFACE_H

#include "Mfd.h"

class Instrument_Surface: public Instrument {
public:
	Instrument_Surface (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel);
	~Instrument_Surface();
	int Type () const { return MFD_SURFACE; }
	char ModeSelKey () const { return 'S'; }
	HELPCONTEXT *HelpTopic () const;
	bool KeyBuffered (DWORD key);
	bool ProcessButton (int bt, int event);
	const char *BtnLabel (int bt) const;
	int BtnMenu (const MFDBUTTONMENU **menu) const;
	void  UpdateDraw (oapi::Sketchpad *skp);
	void UpdateBlt ();
	void SetSize (const Spec &spec);

protected:
	void InitDeviceObjects ();
	void UpdateHorizon ();
	void UpdateTapes ();
	bool ReadParams (std::ifstream &ifs);
	void WriteParams (std::ostream &ofs) const;

private:
	//bool GRS (const CelestialBody *body, double &grs) const;
	bool IAS (const CelestialBody *body, double &ias) const;
	void CopyToHUD () const;
	const SurfParam *sp;
	const ATMCONST *atmc;
	double alt, spd, acc, vspd, vacc, dir, pitch, bank, aoa, psimt;
	double atm_p, atm_rho, atm_T, atm_M, as_plimit;
	double lng, lat, vlng, vlat;
	int spdmode; // speed indicator mode: 1=GRS (ground-relative speed), 2=TAS (true airspeed), 3=IAS (indicated airspeed), 4=OS (orbital speed)
	char title[40];
	DWORD tapecol;
	DWORD hrzx, hrzy, hrzr, hrzx0, hrzy0, hrzx1, hrzy1;
	DWORD spdx0, accx0, vacx0, vspx0, altx0, aoax0, diry0, accy0, accy, accr;
	int bnkmarkx[12], bnkmarky[12];
	int hdgtick, hdgbmpw, hdgbmph;
	SURFHANDLE horizon, heading;
	SURFHANDLE tapes1, ticks1, tapes2, ticks2;
	DWORD hrzcol[2];
	static COLORREF tapelabelcol[2];
	oapi::Pen *horizonpen;
	oapi::Brush *brush[3];
	oapi::IVECTOR2 spdpt[7], altpt[7], vsipt[7], accpt[7], vacpt[7], aoapt[7];

	static struct SavePrm {
		int spdmode;
	} saveprm;
};

#endif // !__MFD_SURFACE_H