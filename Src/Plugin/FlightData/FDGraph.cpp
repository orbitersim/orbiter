// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//            Copyright (C) 2003 Martin Schweiger
//                   All rights reserved
//
// FDGraph.cpp
// Flight data graph class implementation.
// ==============================================================

#include "FDGraph.h"
#include "orbitersdk.h"
#include "resource.h"

extern VESSEL *g_VESSEL;

void FlightDataGraph::AppendDataPoint (FILE *f)
{
	double dp;
	float dp2[2];

	switch (dtype) {
	case 0: // altitude
		dp = g_VESSEL->GetAltitude() * 0.001;
		if (f) fprintf (f, " %10.4f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	case 1: // airspeed
		dp = g_VESSEL->GetAirspeed();
		if (f) fprintf (f, " %9.2f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	case 2: // Mach number
		dp = g_VESSEL->GetMachNumber();
		if (f) fprintf (f, " %6.2f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	case 3: // temperature
		dp = g_VESSEL->GetAtmTemperature();
		if (f) fprintf (f, " %7.1f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	case 4: // pressure
		dp2[0] = (float)(g_VESSEL->GetAtmPressure() * 0.001);
		dp2[1] = (float)(g_VESSEL->GetDynPressure() * 0.001);
		if (f) fprintf (f, " %10.4f %10.4f", dp2[0], dp2[1]);
		Graph::AppendDataPoints (dp2);
		return;
	case 5: // AOA
		dp2[0] = (float)(g_VESSEL->GetAOA()*DEG);
		dp2[1] = (float)(g_VESSEL->GetSlipAngle()*DEG);
		if (f) fprintf (f, " %7.1f %7.1f", dp2[0], dp2[1]);
		Graph::AppendDataPoints (dp2);
		return;
	case 6: // lift and drag
		dp2[0] = (float)(g_VESSEL->GetLift() * 0.001);
		dp2[1] = (float)(g_VESSEL->GetDrag() * 0.001);
		if (f) fprintf (f, " %9.2f %9.2f", dp2[0], dp2[1]);
		Graph::AppendDataPoints (dp2);
		return;
	case 7: // L/D
		dp = (g_VESSEL->GetDrag() ? g_VESSEL->GetLift()/g_VESSEL->GetDrag() : 0.0);
		if (f) fprintf (f, " %8.3f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	case 8: // Mass
		dp = g_VESSEL->GetMass();
		if (f) fprintf (f, " %8.0f", dp);
		Graph::AppendDataPoint ((float)dp);
		return;
	}
}

void FlightDataGraph::WriteHeader (FILE *f)
{
	switch (dtype) {
	case 0: // altitude
		fprintf (f, " _______ALT");
		return;
	case 1: // airspeed
		fprintf (f, " _AIRSPEED");
		return;
	case 2: // Mach number
		fprintf (f, " __MACH");
		return;
	case 3: // temperature
		fprintf (f, " ___TEMP");
		return;
	case 4: // pressure
		fprintf (f, " _______STP _______DNP");
		return;
	case 5: // AOA
		fprintf (f, " ____AOA ___SLIP");
		return;
	case 6: // lift and drag
		fprintf (f, " _____LIFT _____DRAG");
		return;
	case 7: // L/D
		fprintf (f, " _____L/D");
		return;
	case 8: // mass
		fprintf (f, " ____MASS");
		return;
	}
}
