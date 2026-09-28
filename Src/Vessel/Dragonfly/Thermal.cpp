// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#include "thermal.h"
#else // __linux__
#include "Thermal.h"
#endif // __linux__

void therm_obj::thermic(double _en)
{ energy=0;Temp=_en;
};

void therm_obj::SetTemp(double _t)
{ Temp=_t;
};

double therm_obj::GetTemp()
{return Temp;
}
