#include "TimeData.h"
#include "Astro.h"

//=============================================================================
// Implementation of class TimeData
//=============================================================================

TimeData::TimeData ()
{
	Reset();
}

void TimeData::Reset (double mjd_ref)
{
	TWarp = TWarpTarget = 1.0;
	TWarpDelay = 0.0;
	SysT0 = SysT1 = SysDT = 0.0;
	SimT0 = SimT1 = SimDT = SimDT0 = 0.0;
	SimT1_ofs = SimT1_inc = 0.0;
	MJD_ref = MJD0 = MJD1 = mjd_ref;
	fps = syst_acc = 0.0;
	framecount = frame_tick = sys_tick = 0;
	bWarpChanged = false;

	fixed_step = 0.0;
	bFixedStep = false;
}

void TimeData::SetFixedStep(double step)
{
	fixed_step = step;
	bFixedStep = (fixed_step > 0.0);
}

void TimeData::BeginStep (double deltat, bool running)
{
	bWarpChanged = false;
	SysT1 = SysT0 + (SysDT = deltat);
	iSysDT = 1.0/SysDT; // note that delta_ms==0 is trapped earlier

	framecount++;
	frame_tick++;
	syst_acc += SysDT;
	if ((size_t)SysT1 != sys_tick) {
		fps = frame_tick/syst_acc;
		frame_tick = 0;
		syst_acc = 0.0;
		sys_tick = (size_t)SysT1;
	}

	if (running) { // only advance simulation time if simulation is not paused

		if (TWarp != TWarpTarget) {
			if (TWarpDelay == 0.0)
				TWarp = TWarpTarget;
			else if (TWarpTarget > TWarp)
				TWarp = min (TWarpTarget, TWarp * pow (10, SysDT/TWarpDelay));
			else
				TWarp = max (TWarpTarget, TWarp * pow (10, -SysDT/TWarpDelay));
			bWarpChanged = true;
		}

		SimDT = (bFixedStep ? fixed_step : SysDT) * TWarp;
		iSimDT = 1.0/SimDT;
		if ((SimT1_inc += SimDT) > 1e6) {
			SimT1_ofs += 1e6;
			SimT1_inc -= 1e6;
		}
		SimT1 = SimT1_ofs + SimT1_inc;
		MJD1 = MJD_ref + Day (SimT1);
	}
}

void TimeData::EndStep (bool running)
{
	SysT0 = SysT1;

	if (running) {
		SimT0 = SimT1;
		SimDT0 = SimDT;
		iSimDT0 = iSimDT;
		MJD0 = MJD1;
	}
}

double TimeData::JumpTo (double mjd)
{
	double dt = (mjd-MJD0)*86400.0;
	MJD0 = MJD1 = mjd;
	SimT0 = SimT1 = SimT1_ofs = (mjd-MJD_ref)*86400.0;
	SimT1_inc = 0.0;
	return dt;
}

double TimeData::MJD(double simt) const
{
	return MJD_ref + Day(simt);
}

void TimeData::SetWarp (double warp, double delay) {
	TWarpTarget = warp;
	TWarpDelay  = delay;
	if (delay == 0.0) {
		TWarp = warp;
		bWarpChanged = true;
	}
}