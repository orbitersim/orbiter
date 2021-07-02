#ifndef _MATRIX_
#define _MATRIX_

#include "vectors.h"
#include <string.h>
enum {_XX=0,_XY,_XZ,_XF,_YX,_YY,_YZ,_YF,_ZX,_ZY,_ZZ,_ZF,_FX,_FY,_FZ,_FF};
class vector3;
class matrix
{ public:
	float p[16];
	//constructors
	matrix(){ memset(p,0,sizeof(float)*9);} //null constructor
	matrix(float xx,float xy,float xz,
		   float yx,float yy,float yz,
		   float zx,float zy,float zz)		//constructor from data
				{set(xx,xy,xz,
					 yx,yy,yz,
					 zx,zy,zz);}
   matrix( float ax,float ay,float az)		//rot angle matrix
				{setang(ax,ay,az);}
   matrix(matrix &m)	{memcpy(p,m.p,sizeof(float)*9);}	//copy matrix

   //operators
   matrix  operator* (matrix &m);
   vector3 operator* (vector3 &v);
   matrix  operator* (float);
   void    operator*= (matrix &m);
   void    operator*= (float);
   matrix operator! ();		//inverse matrix
   matrix operator/ (float);

   //functions
   void set (float xx,float xy,float xz,
			 float yx,float yy,float yz,
			 float zx,float zy,float zz);
   void setang (float ax,float ay,float az);
   void identity();
   matrix invert();

   void trans(float ax,float ay,float az);
   void rotx(float aa);
   void roty(float aa);
   void rotz(float aa);
   void rot(float aa,vector3 v1,vector3 v2);
   float det();
   void setrot(float theta, vector3 axis);
   void setrot(vector3 p1, vector3 p2);
};
#endif