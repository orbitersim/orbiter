

#ifndef _VECTORS_
#define _VECTORS_


#include "matrix.h"
class matrix;
class vector3
{ public:
  double x;
  double y;
  double z;

  //constructors
      vector3(double _x,double _y,double _z) {set(_x,_y,_z);}
	  vector3()	{set(0.0,0.0,0.0);}
	  vector3(vector3 &v) {set(v.x,v.y,v.z);}
  //operators
	//vector to vector
  vector3 operator+ (vector3);
  void	  operator+= (vector3);
  vector3 operator- (vector3);
  void    operator-=(vector3);
  vector3 operator* (vector3);
  vector3 operator/ (vector3);
  double operator% (vector3);   //dot product
  vector3 operator! ();
	//vector to double
  vector3 operator+ (double);
  void    operator+=(double);
  vector3 operator- (double);
  void    operator-=(double);
  vector3 operator* (double);
  void    operator*=(double);
  vector3 operator/ (double);
  void    operator/=(double);
    //vector to matrix
  vector3 operator* (matrix &m);
  void operator*=(matrix &m);
  //functions
  void set(double _x,double _y,double _z);
  double selfdot();
  vector3 normalize();
  void selfnormalize();
  double length();
  double mod();
  double distance(vector3 &v);
  double angle(vector3 &v); 
  vector3 inplane(vector3 &v1,vector3 &v2); //return the projection of this onto plane v1,v2

};

vector3 _vector3(double _x, double _y, double _z);
#endif