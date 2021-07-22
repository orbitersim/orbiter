// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//
// Graph.h
// Generic data graph class interface.
// ==============================================================

#ifndef __GRAPH_H
#define __GRAPH_H

#include "windows.h"

const int MAXPLOT = 3;
const int NDATA = 200;

struct GDIres {
	HFONT font[2];
	HPEN  pen[2+MAXPLOT];
	int nref;
};

class Graph {
public:
	Graph (int _nplot = 1);
	~Graph();
	static void InitGDI ();
	static void FreeGDI ();
	void SetTitle (const char *_title);
	void SetXLabel (const char *_label);
	void SetYLabel (const char *_label);
	void SetLegend (const char *_legend);
	void ResetData();
	void AppendDataPoint (float val);
	void AppendDataPoints (float *val);
	void Refresh (HDC hDC, int w, int h);

protected:
	void SetAutoRange ();
	void SetAutoTicks ();

private:
	int nplot;
	float **data;
	float vmin, vmax;
	float data_tickscale;
	float data_dtick;
	float data_tickmin;
	int data_minortick;
	int ndata;
	int idx;
	char *title;
	char *xlabel, *ylabel;
	char *legend;
	int *legend_idx;
	static GDIres gdi;
};


#endif // !__GRAPH_H