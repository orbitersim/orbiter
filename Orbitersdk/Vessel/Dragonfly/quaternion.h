#ifndef _QUATERNION_
#define _QUATERNION_


#include "matrix.h"

class quaternion : public vector3
{
public:
	double w;
	quaternion();
	quaternion operator * (quaternion q2);
	matrix mat();
	quaternion rotaxis();
	void rotate(float theta,vector3 axis);
};

quaternion _quaternion(float theta,vector3 axis);
#endif