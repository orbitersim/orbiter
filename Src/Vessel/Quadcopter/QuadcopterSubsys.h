// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __QUADCOPTERSUBSYS_H
#define __QUADCOPTERSUBSYS_H

#include "Quadcopter.h"
#ifndef __linux__
#include "..\Common\Instrument.h"
#else // __linux__
#include "../Common/Instrument.h"
#endif // __linux__

// ==============================================================

class QuadcopterSubsystem : public Subsystem {
public:
	QuadcopterSubsystem(Quadcopter *v) : Subsystem(v) {}
	QuadcopterSubsystem(QuadcopterSubsystem *subsys) : Subsystem(subsys) {}
	inline Quadcopter *QC() { return (Quadcopter*)Vessel(); }
	inline const Quadcopter *QC() const { return (Quadcopter*)Vessel(); }
};

#endif // !__QUADCOPTERSUBSYS_H
