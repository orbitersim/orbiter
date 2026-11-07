// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __QUADCOPTERSUBSYS_H
#define __QUADCOPTERSUBSYS_H

#include "Quadcopter.h"
#include "../Common/Instrument.h"

// ==============================================================

class QuadcopterSubsystem : public Subsystem {
public:
	QuadcopterSubsystem(Quadcopter *v) : Subsystem(v) {}
	QuadcopterSubsystem(QuadcopterSubsystem *subsys) : Subsystem(subsys) {}
	inline Quadcopter *QC() { return (Quadcopter*)Vessel(); }
	inline const Quadcopter *QC() const { return (Quadcopter*)Vessel(); }
};

#endif // !__QUADCOPTERSUBSYS_H
