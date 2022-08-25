// Copyright (c) Martin Schweiger
// Licensed under the MIT License

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

FlightDataGraph* FlightDataGraph::CreateGraph(const std::string& title)
{
	if (!title.compare(FDGraph_Altitude::Title()))
		return new FDGraph_Altitude();
	else if (!title.compare(FDGraph_Airspeed::Title()))
		return new FDGraph_Airspeed();
	else if (!title.compare(FDGraph_VSpeed::Title()))
		return new FDGraph_VSpeed();
	else if (!title.compare(FDGraph_MachNumber::Title()))
		return new FDGraph_MachNumber();
	else if (!title.compare(FDGraph_Temperature::Title()))
		return new FDGraph_Temperature();
	else if (!title.compare(FDGraph_Pressure::Title()))
		return new FDGraph_Pressure();
	else if (!title.compare(FDGraph_AOA::Title()))
		return new FDGraph_AOA();
	else if (!title.compare(FDGraph_LiftDrag::Title()))
		return new FDGraph_LiftDrag();
	else if (!title.compare(FDGraph_LDRatio::Title()))
		return new FDGraph_LDRatio();
	else if (!title.compare(FDGraph_Mass::Title()))
		return new FDGraph_Mass();
	else
		return nullptr;
}

FlightDataGraph::FlightDataGraph(const std::string &title, int _nplot)
	: Graph(_nplot)
{
	SetTitle(title);
}

FlightDataGraph::~FlightDataGraph()
{
}

// ==============================================================

FDGraph_Altitude::FDGraph_Altitude()
	: FlightDataGraph(FDGraph_Altitude::Title(), 1)
{
	SetYLabel("Altitude: km");
	SetLegend("Alt");
}

void FDGraph_Altitude::AppendDataPoint(VESSEL* v, FILE* f)
{
	double dp = v->GetAltitude() * 0.001; // unit: km
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %10.4f", dp);     // add to log file
}

void FDGraph_Altitude::WriteHeader(FILE* f)
{
	fprintf(f, " _______ALT");
}

// ==============================================================

FDGraph_Airspeed::FDGraph_Airspeed()
	: FlightDataGraph(FDGraph_Airspeed::Title(), 1)
{
	SetYLabel("Airspeed: m/s");
	SetLegend("Speed");
}

void FDGraph_Airspeed::AppendDataPoint(VESSEL* v, FILE* f)
{
	double dp = v->GetAirspeed();
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %9.2f", dp);      // add to log file
}

void FDGraph_Airspeed::WriteHeader(FILE* f)
{
	fprintf(f, " _AIRSPEED");
}

// ==============================================================

FDGraph_VSpeed::FDGraph_VSpeed()
	: FlightDataGraph(FDGraph_VSpeed::Title(), 1)
{
	SetYLabel("VSpeed: m/s");
	SetLegend("VSpeed");
}

void FDGraph_VSpeed::AppendDataPoint(VESSEL* v, FILE* f)
{
	VECTOR3 vel;
	double dp = 0.0;
	if (v->GetAirspeedVector(FRAME_HORIZON, vel))
		dp = vel.y;
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %9.2f", dp);      // add to log file
}

void FDGraph_VSpeed::WriteHeader(FILE* f)
{
	fprintf(f, " ___VSPEED");
}

// ==============================================================

FDGraph_MachNumber::FDGraph_MachNumber()
	: FlightDataGraph(FDGraph_MachNumber::Title(), 1)
{
	SetYLabel("Mach number");
	SetLegend("Mach");
}

void FDGraph_MachNumber::AppendDataPoint(VESSEL* v, FILE* f)
{
	double dp = v->GetMachNumber();
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %6.2f", dp);      // add to log file
}

void FDGraph_MachNumber::WriteHeader(FILE* f)
{
	fprintf(f, " __MACH");
}

// ==============================================================

FDGraph_Temperature::FDGraph_Temperature()
	: FlightDataGraph(FDGraph_Temperature::Title(), 1)
{
	SetYLabel("Temp: K");
	SetLegend("Temp");
}

void FDGraph_Temperature::AppendDataPoint(VESSEL* v, FILE* f)
{
	double dp = v->GetAtmTemperature();
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %7.1f", dp);      // add to log file
}

void FDGraph_Temperature::WriteHeader(FILE* f)
{
	fprintf(f, " ___TEMP");
}

// ==============================================================

FDGraph_Pressure::FDGraph_Pressure()
	: FlightDataGraph(FDGraph_Pressure::Title(), 2)
{
	SetYLabel("Pressure: kPa");
	SetLegend("Static&Dynamic");
}

void FDGraph_Pressure::AppendDataPoint(VESSEL* v, FILE* f)
{
	float dp[2];
	dp[0] = (float)(v->GetAtmPressure() * 0.001);      // ambient pressure [kPa]
	dp[1] = (float)(v->GetDynPressure() * 0.001);      // dynamic pressure [kPa]
	Graph::AppendDataPoints(dp);                       // add to plot
	if (f) fprintf(f, " %10.4f %10.4f", dp[0], dp[1]); // add to log file
}

void FDGraph_Pressure::WriteHeader(FILE* f)
{
	fprintf(f, " _______STP _______DNP");
}

// ==============================================================

FDGraph_AOA::FDGraph_AOA()
	: FlightDataGraph(FDGraph_AOA::Title(), 2)
{
	SetYLabel("AOA: deg");
	SetLegend("Vertical&Horizontal");
}

void FDGraph_AOA::AppendDataPoint(VESSEL* v, FILE* f)
{
	float dp[2];
	dp[0] = (float)(v->GetAOA() * DEG);              // AOA [deg]
	dp[1] = (float)(v->GetSlipAngle() * DEG);        // slip angle [deg]
	Graph::AppendDataPoints(dp);                     // add to plot
	if (f) fprintf(f, " %7.1f %7.1f", dp[0], dp[1]); // add to log file
}

void FDGraph_AOA::WriteHeader(FILE* f)
{
	fprintf(f, " ____AOA ___SLIP");
}

// ==============================================================

FDGraph_LiftDrag::FDGraph_LiftDrag()
	: FlightDataGraph(FDGraph_LiftDrag::Title(), 2)
{
	SetYLabel("Lift, Drag: kN");
	SetLegend("Lift&Drag");
}

void FDGraph_LiftDrag::AppendDataPoint(VESSEL* v, FILE* f)
{
	float dp[2];
	dp[0] = (float)(v->GetLift() * 0.001);           // lift [kN]
	dp[1] = (float)(v->GetDrag() * 0.001);           // drag [kN]
	Graph::AppendDataPoints(dp);                     // add to plot
	if (f) fprintf(f, " %9.2f %9.2f", dp[0], dp[1]); // add to log file
}

void FDGraph_LiftDrag::WriteHeader(FILE* f)
{
	fprintf(f, " _____LIFT _____DRAG");
}

// ==============================================================

FDGraph_LDRatio::FDGraph_LDRatio()
	: FlightDataGraph(FDGraph_LDRatio::Title(), 1)
{
	SetYLabel("L/D");
	SetLegend("L/D");
}

void FDGraph_LDRatio::AppendDataPoint(VESSEL* v, FILE* f)
{
	double dp = (v->GetDrag() ? v->GetLift() / v->GetDrag() : 0.0);
	Graph::AppendDataPoint((float)dp);    // add to plot
	if (f) fprintf(f, " %8.3f", dp);      // add to log file
}

void FDGraph_LDRatio::WriteHeader(FILE* f)
{
	fprintf(f, " _____L/D");
}

// ==============================================================

FDGraph_Mass::FDGraph_Mass()
	: FlightDataGraph(FDGraph_Mass::Title(), 2)
{
	SetYLabel("Mass: kg");
	SetLegend("Total&Propellant");
}

void FDGraph_Mass::AppendDataPoint(VESSEL* v, FILE* f)
{
	float dp[2];
	dp[0] = (float)v->GetMass();                     // total mass [kg]
	dp[1] = (float)v->GetTotalPropellantMass();      // propellant mass [kg]
	Graph::AppendDataPoints(dp);                     // add to plot
	if (f) fprintf(f, " %8.0f %8.0f", dp[0], dp[1]); // add to log file
}

void FDGraph_Mass::WriteHeader(FILE* f)
{
	fprintf(f, " _TOTMASS _PRPMASS");
}
