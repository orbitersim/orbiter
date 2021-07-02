// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2003 Martin Schweiger
//                   All rights reserved
//
// Common.cpp
// Utility functions common to multiple Atlantis-related modules
// ==============================================================

#include "Atlantis.h"

#ifdef _DEBUG
// D. Beachy: GROW THE STACK HERE SO WE CAN USE BOUNDSCHECKER FOR DEBUGGING
// We need this is because BoundsChecker (for this object) grows the stack more than 1 full page (4K) at once 
// and then touches data beyond the initial 4K, skipping over the guard page that Windows places below the stack to grow it automatically.  
// Therefore we will grow the stack manually in one-page increments here.
// This is only necessary for BoundsChecker debugging.

int GrowStack()
{
    // NOTE: this requires that orbiter.exe has its 'Size of Stack Reserve' PE header parameter set to 4 MB
    int pageCount = 256;    // 256 4K pages = reserve 1 MB of stack
    DWORD dwStackDelta = 0; // total # of stack bytes used
    for (int i=0; i < pageCount; i++)
    {
        dwStackDelta += 4096;
        __asm
        {
            sub     esp, 4092;  // 1 page - 4 bytes
            push    0xFEEDFEED  // touch the page
        }
    }

    // now pop the stack we touched
    __asm
    {
        mov     eax, [dwStackDelta] // size in bytes
        add     esp, eax
    }

    return 0;
}

// invoke GrowStack early before the next lines are called (otherwise BoundsChecker will crash)
int growStack=GrowStack();
#endif

int SRB_nt = 6;
double SRB_Seq[6]    = {-SRB_STABILISATION_TIME, -1,     103,     115,       SRB_SEPARATION_TIME, SRB_CUTOUT_TIME};
double SRB_Thrust[6] = { 0,                       1,       1,       0.85,    0.05,                0              };
double SRB_Prop[6]   = { 1,                       0.98768, 0.13365, 0.04250, 0.001848,            0              };
double SRB_ThrSCL[5] = {(SRB_Thrust[1]-SRB_Thrust[0])/(SRB_Seq[1]-SRB_Seq[0]),
						(SRB_Thrust[2]-SRB_Thrust[1])/(SRB_Seq[2]-SRB_Seq[1]),
						(SRB_Thrust[3]-SRB_Thrust[2])/(SRB_Seq[3]-SRB_Seq[2]),
						(SRB_Thrust[4]-SRB_Thrust[3])/(SRB_Seq[4]-SRB_Seq[3]),
						(SRB_Thrust[5]-SRB_Thrust[4])/(SRB_Seq[5]-SRB_Seq[4])};
double SRB_PrpSCL[5] = {(SRB_Prop[1]-SRB_Prop[0])/(SRB_Seq[1]-SRB_Seq[0]),
						(SRB_Prop[2]-SRB_Prop[1])/(SRB_Seq[2]-SRB_Seq[1]),
						(SRB_Prop[3]-SRB_Prop[2])/(SRB_Seq[3]-SRB_Seq[2]),
						(SRB_Prop[4]-SRB_Prop[3])/(SRB_Seq[4]-SRB_Seq[3]),
						(SRB_Prop[5]-SRB_Prop[4])/(SRB_Seq[5]-SRB_Seq[4])};

//PARTICLESTREAMSPEC srb_contrail = {
//	0, 12.0, 3, 150.0, 0.4, 8.0, 4, 3.0, PARTICLESTREAMSPEC::DIFFUSE,
//	PARTICLESTREAMSPEC::LVL_PSQRT, 0, 0.5,
//	PARTICLESTREAMSPEC::ATM_PLOG, 1e-6, 0.1
//};
PARTICLESTREAMSPEC srb_contrail = {
	0, 12.0, 3, 200.0, 0.25, 12.0, 11, 10.0, PARTICLESTREAMSPEC::DIFFUSE,
	PARTICLESTREAMSPEC::LVL_PSQRT, 0, 0.7,
	PARTICLESTREAMSPEC::ATM_PLOG, 1e-6, 0.1
};
PARTICLESTREAMSPEC srb_exhaust = {
	0, 6.0, 40, 250.0, 0.04, 0.4, 20, 6.0, PARTICLESTREAMSPEC::EMISSIVE,
	PARTICLESTREAMSPEC::LVL_SQRT, 1, 1,
	PARTICLESTREAMSPEC::ATM_FLAT, 1, 1
};

// time-dependent calculation of SRB thrust and remaining propellant
void GetSRB_State (double met, double &thrust_level, double &prop_level)
{
	int i;
	for (i = SRB_nt-2; i >= 0; i--)
		if (met > SRB_Seq[i]) break;
	thrust_level = SRB_ThrSCL[i] * (met-SRB_Seq[i]) + SRB_Thrust[i];
	prop_level = SRB_PrpSCL[i] * (met-SRB_Seq[i]) + SRB_Prop[i];
}

