// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// MomentInd.h
// User interface for moment indicators on overhead panel
// ==============================================================

#ifndef __MOMENTIND_H
#define __MOMENTIND_H

#include "..\Common\Instrument.h"

// ==============================================================

class AngRateIndicator: public PanelElement {
public:
	AngRateIndicator (VESSEL3 *v, SURFHANDLE blitsrc);
	~AngRateIndicator ();
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	void LoadVC (int vcid);
	void LoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

protected:
	void UncoverScale (int which, int axis, double phi, NTVERTEX *vtx);
	void ValStr (double v, char *str);
	void BlitReadout (int which, int axis, const char *str, SURFHANDLE tgt);

private:
	double upt;
	NTVERTEX **vtx0, *vtxbuf_2D, *vtxbuf_VC;
	int nvtx;
	double w0_2D, w0_VC, cost_2D, sint_2D, cost_VC, sint_VC;
	double *w0, *cost, *sint;
	double xcnt_2D[3], ycnt_2D[3], zcnt_2D[3];
	double xcnt_VC[3], ycnt_VC[3], zcnt_VC[3];
	double *xcnt, *ycnt, *zcnt;
	static int bmp_w[3];
	static int bmp_h[3];
	static int ofs_x[3];
	static int ofs_y[3];
	static int label_ofs_x[3];
	char label[3][3][8];
	SURFHANDLE bsrc;
};

#endif // !__MOMENTIND_H