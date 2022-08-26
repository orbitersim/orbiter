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

#include <stdio.h>
#include "Dialog\Graph.h"

class VESSEL;

// ==============================================================

class FlightDataGraph: public Graph {
public:
	/// \brief Graph factory: return a new data graph, given its title 
	/// \param title Graph title, must be one of the recognised titles 
	/// \return pointer to new graph, or nullptr if title not recognised
	static FlightDataGraph* CreateGraph(const std::string& title);

	/// \brief Flight data graph constructor
	/// \param title Graph title 
	/// \param _nplot number of plots in graph
	FlightDataGraph(const std::string &title, int _nplot = 1);

	virtual ~FlightDataGraph();

	/// \brief Adds a new data point to the graph and log file
	/// \param v vessel pointer
	/// \param f log file handle, if applicable
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0) = 0;

	/// \brief Writes data type column header to log file
	/// \param f log file handle
	virtual void WriteHeader (FILE *f) = 0;
};

// ==============================================================

class FDGraph_Altitude : public FlightDataGraph {
public:
	static std::string Title() { return "Altitude"; }
	FDGraph_Altitude();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_Airspeed : public FlightDataGraph {
public:
	static std::string Title() { return "Airspeed"; }
	FDGraph_Airspeed();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_VSpeed : public FlightDataGraph {
public:
	static std::string Title() { return "Vertical speed"; }
	FDGraph_VSpeed();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_MachNumber : public FlightDataGraph {
public:
	static std::string Title() { return "Mach number"; }
	FDGraph_MachNumber();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_Temperature : public FlightDataGraph {
public:
	static std::string Title() { return "Freestream temperature"; }
	FDGraph_Temperature();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_Pressure : public FlightDataGraph {
public:
	static std::string Title() { return "Pressure"; }
	FDGraph_Pressure();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_AOA : public FlightDataGraph {
public:
	static std::string Title() { return "Angle of Attack"; }
	FDGraph_AOA();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_LiftDrag : public FlightDataGraph {
public:
	static std::string Title() { return "Lift and Drag"; }
	FDGraph_LiftDrag();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_LDRatio : public FlightDataGraph {
public:
	static std::string Title() { return "L/D"; }
	FDGraph_LDRatio();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

// ==============================================================

class FDGraph_Mass : public FlightDataGraph {
public:
	static std::string Title() { return "Mass"; }
	FDGraph_Mass();
	virtual void AppendDataPoint(VESSEL* v, FILE* f = 0);
	virtual void WriteHeader(FILE* f);
};

#endif // !__FDGRAPH_H