
#include "thermal.h"

void therm_obj::thermic(double _en)
{ energy=0;Temp=_en;
};

void therm_obj::SetTemp(double _t)
{ Temp=_t;
};

double therm_obj::GetTemp()
{return Temp;
}