
#include "quaternion.h"
#include <math.h>

quaternion::quaternion()
{ x=0.0f;y=0.0f;z=1.0f;w=0.0f;}



quaternion quaternion::operator * (quaternion q2)
{ quaternion q;

q.w=w*q2.w - x*q2.x - y*q2.y - z*q2.z;
q.x=w*q2.x + x*q2.w + y*q2.z - z*q2.y;
q.y=w*q2.y + y*q2.w + z*q2.x - x*q2.z;
q.z=w*q2.z + z*q2.w + x*q2.y - y*q2.x;

return q;

};

matrix quaternion::mat()
{
matrix m;
double xx=x*x;
double xy=x*y;
double xz=x*z;
double xw=x*w;

double yy=y*y;
double yz=y*z;
double yw=y*w;

double zz=z*z;
double zw=z*w;

m.p[0] = 1 - 2 * ( yy + zz );
m.p[1] =     2 * ( xy + zw );
m.p[2] =     2 * ( xz - yw );


m.p[4] =     2 * ( xy - zw );
m.p[5] = 1 - 2 * ( xx + zz );
m.p[6] =     2 * ( yz + xw );

m.p[8] =     2 * ( xz + yw );
m.p[9] =     2 * ( yz - xw );
m.p[10]= 1 - 2 * ( xx + yy );

m.p[3]=m.p[7]=m.p[11]=m.p[12]=m.p[13]=m.p[14]=0.0f;
m.p[15]=1.0f;
return m;
};

quaternion quaternion::rotaxis()
{
quaternion q;
q.w=2*acos(w);
double scale=this->mod();
q.x=x/scale;
q.y=y/scale;
q.z=z/scale;
return q;
}

void quaternion::rotate(float theta,vector3 axis)
{
float sin_t=sin(theta/2);
w=cos(theta/2);
x=axis.x*sin_t;
y=axis.y*sin_t;
z=axis.z*sin_t;
}

quaternion _quaternion(float theta,vector3 axis)
{quaternion q;
q.rotate(theta,axis);
return q;
};