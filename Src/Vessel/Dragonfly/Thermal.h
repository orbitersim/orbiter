

#ifndef __THERMAL_H_
#define __THERMAL_H_
#include "matrix.h"

class therm_obj			//thermal object.an object that can receive thermal energy
{ public:

  double energy;		//Q ,or termic energy in Joules
  double c;				// c - material constant in J/gr*K
  vector3 pos;			//position in ship
  float mass;			//mass of the object (in grams)
  float Temp;			//duh!
  void thermic( double _en);
  void SetTemp(double _t);
  double GetTemp();
};
//class Thermal_engine  //main thermal parent. Handles most thermic problems
//{
	//therm_obj 

#endif 