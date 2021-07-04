
#include "matrix.h"
#include "vectors.h"
#include <math.h>

matrix matrix::operator * (matrix &m)
{ matrix temp;
  
temp.p[_XX]=p[_XX]*m.p[_XX]+p[_XY]*m.p[_YX]+p[_XZ]*m.p[_ZX]+p[_XF]*m.p[_FX];
temp.p[_YX]=p[_YX]*m.p[_XX]+p[_YY]*m.p[_YX]+p[_YZ]*m.p[_ZX]+p[_YF]*m.p[_FX];
temp.p[_ZX]=p[_ZX]*m.p[_XX]+p[_ZY]*m.p[_YX]+p[_ZZ]*m.p[_ZX]+p[_ZF]*m.p[_FX];
temp.p[_FX]=p[_FX]*m.p[_XX]+p[_FY]*m.p[_YX]+p[_FZ]*m.p[_ZX]+p[_FF]*m.p[_FX];

temp.p[_XY]=p[_XX]*m.p[_XY]+p[_XY]*m.p[_YY]+p[_XZ]*m.p[_ZY]+p[_XF]*m.p[_FY];
temp.p[_YY]=p[_YX]*m.p[_XY]+p[_YY]*m.p[_YY]+p[_YZ]*m.p[_ZY]+p[_YF]*m.p[_FY];
temp.p[_ZY]=p[_ZX]*m.p[_XY]+p[_ZY]*m.p[_YY]+p[_ZZ]*m.p[_ZY]+p[_ZF]*m.p[_FY];
temp.p[_FY]=p[_FX]*m.p[_XY]+p[_FY]*m.p[_YY]+p[_FZ]*m.p[_ZY]+p[_FF]*m.p[_FY];

temp.p[_XZ]=p[_XX]*m.p[_XZ]+p[_XY]*m.p[_YZ]+p[_XZ]*m.p[_ZZ]+p[_XF]*m.p[_FZ];
temp.p[_YZ]=p[_YX]*m.p[_XZ]+p[_YY]*m.p[_YZ]+p[_YZ]*m.p[_ZZ]+p[_YF]*m.p[_FZ];
temp.p[_ZZ]=p[_ZX]*m.p[_XZ]+p[_ZY]*m.p[_YZ]+p[_ZZ]*m.p[_ZZ]+p[_ZF]*m.p[_FZ];
temp.p[_FZ]=p[_FX]*m.p[_XZ]+p[_FY]*m.p[_YZ]+p[_FZ]*m.p[_ZZ]+p[_FF]*m.p[_FZ];

temp.p[_XF]=p[_XX]*m.p[_XF]+p[_XY]*m.p[_YF]+p[_XZ]*m.p[_ZF]+p[_XF]*m.p[_FF];
temp.p[_YF]=p[_YX]*m.p[_XF]+p[_YY]*m.p[_YF]+p[_YZ]*m.p[_ZF]+p[_YF]*m.p[_FF];
temp.p[_ZF]=p[_ZX]*m.p[_XF]+p[_ZY]*m.p[_YF]+p[_ZZ]*m.p[_ZF]+p[_ZF]*m.p[_FF];
temp.p[_FF]=p[_FX]*m.p[_XF]+p[_FY]*m.p[_YF]+p[_FZ]*m.p[_ZF]+p[_FF]*m.p[_FF];

 return temp;
}

vector3 matrix::operator * (vector3 &v)
{ vector3 temp;
  double t1,t2,t3,t4;
 temp.x=p[_XX]*v.x+p[_XY]*v.y+p[_XZ]*v.z+p[_XF];
 temp.y=p[_YX]*v.x+p[_YY]*v.y+p[_YZ]*v.z+p[_XF];
 temp.z=p[_ZX]*v.x+p[_ZY]*v.y+p[_ZZ]*v.z+p[_ZF];
 return temp;
}
matrix matrix::operator * (float f)
{matrix temp;
  for (int i=0;i<16;i++)
		temp.p[i]=p[i]*f;
  return temp;
}

void matrix::operator *= (matrix &m)
{ *this=*this*m;
}

void matrix::operator *= (float f)
{ *this=*this*f;
}

matrix matrix::operator !()
{matrix temp;
   for (int i=0;i<4;i++)
	    for (int j=0;j<4;j++)
			 temp.p[i+j*4]=p[j+i*4];
 return temp;
}
void matrix::set(float xx,float xy,float xz,float yx,float yy,float yz,float zx,float zy,float zz)
{p[_XX]=xx;p[_XY]=xy;p[_XZ]=xz;p[_XF]=0.0;
 p[_YX]=yx;p[_YY]=yy;p[_YZ]=yz;p[_YF]=0.0;
 p[_ZX]=zx;p[_ZY]=zy;p[_ZZ]=zz;p[_ZF]=0.0;
 p[_FX]=0.0;p[_FY]=0.0;p[_FZ]=0.0;p[_FF]=1.0;
}

void matrix::setang(float ax,float ay,float az)
{ float sinx,siny,sinz,cosx,cosy,cosz,syz,cxz,sxcz;
sinx=sinf(ax);cosx=cosf(ax);
siny=sinf(ay);cosy=cosf(ay);
sinz=sinf(az);cosz=cosf(az);

syz=siny*sinz;
cxz=cosx*cosz;
sxcz=sinx*cosz;
p[_XX]=sinx*syz+cxz;        p[_XY]=cosy*sinz; p[_XZ]=sxcz-cosx*syz;
p[_YX]=sxcz*siny-cosx*sinz; p[_YY]=cosy*cosz; p[_YZ]=-cxz*siny-sinx*sinz;
p[_ZX]=-sinx*cosy;			p[_ZY]=siny;	  p[_ZZ]=cosx*cosy;

}
void matrix::identity()
{ p[_XX]=1.f;p[_XY]=0;  p[_XZ]=0;  p[_XF]=0;
  p[_YX]=0;  p[_YY]=1.f;p[_YZ]=0;  p[_YF]=0;
  p[_ZX]=0;  p[_ZY]=0;  p[_ZZ]=1.f;p[_ZF]=0;
  p[_FX]=0;  p[_FY]=0;  p[_FZ]=0;  p[_FF]=1.f;
}

matrix matrix::invert()
{ 
matrix invert;
invert.p[_XX]=p[_YY]*p[_ZZ]-p[_YZ]*p[_ZY];
invert.p[_XY]=p[_XZ]*p[_ZY]-p[_XY]*p[_ZZ];
invert.p[_XZ]=p[_XY]*p[_YZ]-p[_XZ]*p[_YY];

invert.p[_YX]=p[_YZ]*p[_ZX]-p[_YX]*p[_ZZ];
invert.p[_YY]=p[_XX]*p[_ZZ]-p[_XZ]*p[_ZX];
invert.p[_YZ]=p[_XZ]*p[_YX]-p[_XX]*p[_YZ];

invert.p[_ZX]=p[_YX]*p[_ZY]-p[_YY]*p[_ZX];
invert.p[_ZY]=p[_XY]*p[_ZX]-p[_XX]*p[_ZY];
invert.p[_ZZ]=p[_XX]*p[_YY]-p[_XY]*p[_YX];
invert=invert/(this->det());
return invert;
}
matrix matrix::operator / (float f)
{
matrix temp;
  for (int i=0;i<16;i++)
		temp.p[i]=p[i]/f;
  return temp;
}
float matrix::det()
{
float det;
det=p[_XX]*p[_YY]*p[_ZZ];
det+=p[_YX]*p[_ZY]*p[_XZ];
det+=p[_ZX]*p[_XY]*p[_YZ];
det-=p[_XZ]*p[_YY]*p[_ZX];
det-=p[_YZ]*p[_ZY]*p[_XX];
det-=p[_ZZ]*p[_XY]*p[_YX];
return det;
}

void matrix::trans(float ax,float ay,float az)
{
identity();
p[14]=az;
p[15]=ay;
p[16]=ax;
}

void matrix::setrot(float theta,vector3 axis)
{
	
    float ct = cos(theta);
	float st = sin(theta);
	
	float xx=axis.x*axis.x;
	float yy=axis.y*axis.y;
	float zz=axis.z*axis.z;
	float xy=axis.x*axis.y;
	float xz=axis.x*axis.z;
	float yz=axis.y*axis.z;

    

    p[0] = xx + ct*(1-xx);
    p[1] = xy + ct*(-xy) + st*-axis.z;
    p[2] = xz + ct*(-xz) + st*axis.y;

    p[4] = xy + ct*(-xy) + st*axis.z;
    p[5] = yy + ct*(1-yy);
    p[6] = yz + ct*(-yz) + st*-axis.x;

    p[8] = xz + ct*(-xz) + st*-axis.y;
    p[9] = yz + ct*(-yz) + st*axis.x;
    p[10]= zz + ct*(1-zz);

	p[3]=p[7]=p[11]=p[12]=p[13]=p[14]=0.0f;
	p[15]=1.0f;

}

void matrix::setrot(vector3 p1,vector3 p2)
{
vector3 _ax=p1*p2;
float _th=p1.angle(p2);
setrot(_th,_ax);
}