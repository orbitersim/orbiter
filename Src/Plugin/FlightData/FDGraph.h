// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//            Copyright (C) 2003 Martin Schweiger
//                   All rights reserved
//
// FDGraph.h
// Flight data graph class interface.
// ==============================================================

#ifndef __FDGRAPH_H
#define __FDGRAPH_H

#include "Dialog\Graph.h"
#include "stdio.h"

class FlightDataGraph: public Graph {
public:
	FlightDataGraph (int _dtype, int _nplot = 1): Graph (_nplot), dtype(_dtype) {}
	int DType() const { return dtype; }
	void AppendDataPoint (FILE *f = 0);
	void WriteHeader (FILE *f);

private:
	int dtype;
};

#endif // !__FDGRAPH_H