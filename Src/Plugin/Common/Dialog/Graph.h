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

#include <windows.h>
#include <string>
#include <vector>

const int MAXPLOT = 3;
const int NDATA = 200;

struct GDIres {
	HFONT font[2];
	HPEN  pen[2+MAXPLOT];
	int nref;
};

class Graph {
public:
	Graph(int nplot = 1);
	Graph(const Graph& graph);
	~Graph();
	static void InitGDI ();
	static void FreeGDI ();

	/// \brief Returns graph title
	/// \return Graph title
	const std::string& Title() const { return m_title; }

	void SetTitle (const std::string &title);
	void SetXLabel (const std::string &label);
	void SetYLabel (const std::string &label);
	void SetLegend (const std::string &legend);
	void ResetData();
	void AppendDataPoint (float val);
	void AppendDataPoints (float *val);
	void Refresh (HDC hDC, int w, int h);

protected:
	void SetAutoRange ();
	void SetAutoTicks ();

private:
	std::vector<std::vector<float>> m_data;
	float m_vmin, m_vmax;
	float m_tickscale;
	float m_dtick;
	float m_tickmin;
	int m_minortick;
	int m_ndata;
	int m_idx;
	std::string m_title;
	std::string m_xlabel, m_ylabel;
	std::vector<std::string> m_legend;
	static GDIres gdi;
};


#endif // !__GRAPH_H