/*
**-----------------------------------------------------------------------------
** Name:    D3DUtils.cpp
** Purpose: Various D3D utility functions
** Notes:
**
** Copyright (c) 1995-1999 by Microsoft, all rights reserved.
**-----------------------------------------------------------------------------
*/

// Note:  Must Define D3D_OVERLOADS to get C++ version of D3DMATRIX
#define D3D_OVERLOADS
#include <float.h>
#include <math.h>

#include "d3dutils.h"

const float pi = 3.141592654f;

/*
**-----------------------------------------------------------------------------
** Functions
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:       ZeroMatrix
**  Purpose:	sets D3D matrix to all 0's
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
ZeroMatrix(void)
{
    D3DMATRIX ret;
    for (int i=0; i<4; i++) {
        for (int j=0; j<4; j++) {
            ret(i, j) = 0.0f;
		}
	}
    return ret;
} // end ZeroMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       IdentityMatrix
**  Purpose:	sets D3D matrix to Identiy (1's on diagonal, zero's elsewhere)
**-----------------------------------------------------------------------------
*/

D3DMATRIX
IdentityMatrix(void)
{
    D3DMATRIX ret;
    for (int i=0; i<4; i++) {
        for (int j=0; j<4; j++)	{
            ret(i, j) = 0.0f;
		}
		ret(i,i) = 1.0f;
	}
    return ret;
} // end IdentityMatrix
  
/*
**-----------------------------------------------------------------------------
**  Name:       ProjectionMatrix
**  Purpose:	sets Projection matrix from fov, near and far planes
**  Notes:		
**		1. fov is in radians.
**		2. See Blinn, "A Trip Down the Graphics Pipeline" pg 188 for details.
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
ProjectionMatrix(const float near_plane, 
				 const float far_plane, 
				 const float fov)
{
	float	c, s, Q;

	c = (float) cos(fov*0.5);
	s = (float) sin(fov*0.5);
	Q = s/(1.0f - near_plane/far_plane);

    D3DMATRIX ret = ZeroMatrix();
    ret(0, 0) = c;
    ret(1, 1) = c;
	ret(2, 2) = Q;
	ret(3, 2) = -Q*near_plane;
    ret(2, 3) = s;
    return ret;
}	// end ProjectionMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       ViewMatrix
**  Purpose:	Controls where the camara is.
**  Notes:		
**		1. Note the roll parameter is in radians and rools the viewpoint
**			around the viewing direction
**-----------------------------------------------------------------------------
*/

D3DMATRIX
ViewMatrix(const D3DVECTOR& from, 
		   const D3DVECTOR& at, 
		   const D3DVECTOR& world_up, 
		   const float roll)
{
    D3DMATRIX view = IdentityMatrix();
    D3DVECTOR up, right, view_dir;

    view_dir = Normalize(at - from);
	right = CrossProduct(world_up, view_dir);
	up = CrossProduct(view_dir, right);

	right = Normalize(right);
	up = Normalize(up);
	
    view(0, 0) = right.x;
    view(1, 0) = right.y;
    view(2, 0) = right.z;
    view(0, 1) = up.x;
    view(1, 1) = up.y;
    view(2, 1) = up.z;
    view(0, 2) = view_dir.x;
    view(1, 2) = view_dir.y;
    view(2, 2) = view_dir.z;
	
    view(3, 0) = -DotProduct(right, from);
    view(3, 1) = -DotProduct(up, from);
    view(3, 2) = -DotProduct(view_dir, from);

	// Set roll
	if (roll != 0.0f) {
		view = MatrixMult(RotateZMatrix(-roll), view);
	}

    return view;
} // end ViewMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       RotateXMatrix
**  Purpose:	Rotate matrix about X axis
**-----------------------------------------------------------------------------
*/

D3DMATRIX
RotateXMatrix(const float rads)
{
	float	cosine, sine;

	cosine = (float) cos(rads);
	sine = (float) sin(rads);
    D3DMATRIX ret = IdentityMatrix();
    ret(1,1) = cosine;
	ret(2,2) = cosine;
	ret(1,2) = -sine;
	ret(2,1) = sine;
    return ret;
} // end RotateXMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       RotateYMatrix
**  Purpose:	Rotate matrix about Y axis
**-----------------------------------------------------------------------------
*/

D3DMATRIX
RotateYMatrix(const float rads)
{
	float	cosine, sine;

	cosine = (float) cos(rads);
	sine = (float) sin(rads);
    D3DMATRIX ret = IdentityMatrix();
    ret(0,0) = cosine;
	ret(2,2) = cosine;
	ret(0,2) = sine;
	ret(2,0) = -sine;
    return ret;
} // end RotateY
  
/*
**-----------------------------------------------------------------------------
**  Name:       RotateZMatrix
**  Purpose:	Rotate matrix about Z axis
**-----------------------------------------------------------------------------
*/

D3DMATRIX
RotateZMatrix(const float rads)
{
	float	cosine, sine;

	cosine = (float) cos(rads);
	sine = (float) sin(rads);
    D3DMATRIX ret = IdentityMatrix();
    ret(0,0) = cosine;
	ret(1,1) = cosine;
	ret(0,1) = -sine;
	ret(1,0) = sine;
    return ret;
} // end RotateZMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       TranslateMatrix
**  Purpose:    Returns matrix to translate by (dx, dy, dz)
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
TranslateMatrix(const float dx, const float dy, const float dz)
{
    D3DMATRIX ret = IdentityMatrix();
	ret(3, 0) = dx;
	ret(3, 1) = dy;
	ret(3, 2) = dz;
	return ret;
} // end TranslateMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       TranslateMatrix
**  Purpose:    Returns matrix to translate by v
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
TranslateMatrix(const D3DVECTOR& v)
{
    D3DMATRIX ret = IdentityMatrix();
	ret(3, 0) = v[0];
	ret(3, 1) = v[1];
	ret(3, 2) = v[2];
	return ret;
} // end TranslateMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       ScaleMatrix
**  Purpose:    scale matrix (uniform)
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
ScaleMatrix(const float size)
{
    D3DMATRIX ret = IdentityMatrix();
	ret(0, 0) = size;
	ret(1, 1) = size;
	ret(2, 2) = size;
	return ret;
} // end ScaleMatrix
  
/*
**-----------------------------------------------------------------------------
**  Name:       ScaleMatrix
**  Purpose:	scale matrix
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
ScaleMatrix(const float a, const float b, const float c)
{
    D3DMATRIX ret = IdentityMatrix();
	ret(0, 0) = a;
	ret(1, 1) = b;
	ret(2, 2) = c;
	return ret;
} // end ScaleMatrix
  
/*
**-----------------------------------------------------------------------------
**  Name:       ScaleMatrix
**  Purpose:	scale matrix
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
ScaleMatrix(const D3DVECTOR& v)
{
    D3DMATRIX ret = IdentityMatrix();
	ret(0, 0) = v.x;
	ret(1, 1) = v.y;
	ret(2, 2) = v.z;
	return ret;
} // end ScaleMatrix

/*
**-----------------------------------------------------------------------------
**  Name:       MatrixMult
**  Purpose:	[C] = [A] * [B]
**-----------------------------------------------------------------------------
*/

D3DMATRIX
MatrixMult(const D3DMATRIX & a, const D3DMATRIX & b)
{
	D3DMATRIX ret = ZeroMatrix();

	for (int i=0; i<4; i++) {
		for (int j=0; j<4; j++) {
			for (int k=0; k<4; k++) {
				ret(i, j) += a(k, j) * b(i, k);
			}
		}
	}
	return ret;
} // end MatrixMult

/*
**-----------------------------------------------------------------------------
**  Name:       TransformVector
**  Purpose:	V' = V * [M]
**-----------------------------------------------------------------------------
*/

D3DVECTOR 
TransformVector(const D3DVECTOR& v, const D3DMATRIX & m)
{
	float	hvec[4];

	for (int i=0; i<4; i++) {
		hvec[i] = 0.0f;
		for (int j=0; j<4; j++) {
			if (j==3) {
				hvec[i] += m(j, i);
			} else {
				hvec[i] += v[j] * m(j, i);
			}
		}
	}
	D3DVECTOR ret(hvec[0]/hvec[3], hvec[1]/hvec[3], hvec[2]/hvec[3]);

	return ret;
} // end TransformVector

/*
**-----------------------------------------------------------------------------
**  Name:       TransformNormal
**  Purpose:	N' = N * [M]
**-----------------------------------------------------------------------------
*/

D3DVECTOR
TransformNormal(const D3DVECTOR& v, const D3DMATRIX & mat)
{
	D3DMATRIX	m;

	m = MatrixInverse(mat);
	m = MatrixTranspose(m);
	return TransformVector(v, m);
}  // end TransformNormal

/*
**-----------------------------------------------------------------------------
**  Name:       TransformNormalPureRotation
**  Purpose:	N' = N * [M]
**
**	This assumes that the matrix is a pure rotation matrix so only the upper
**	3x3 part of the matrix matters.  In this case the inverse is the transpose
**	so we don't have to do anything but multiple the normal by the upper 3x3.
**-----------------------------------------------------------------------------
*/

D3DVECTOR
TransformNormalPureRotation(const D3DVECTOR& v, const D3DMATRIX & mat)
{
	D3DVECTOR	ret;

	ret[0] = v[0] * mat(0, 0) + v[1] * mat(1, 0) + v[2] * mat(2, 0);
	ret[1] = v[0] * mat(0, 1) + v[1] * mat(1, 1) + v[2] * mat(2, 1);
	ret[2] = v[0] * mat(0, 2) + v[1] * mat(1, 2) + v[2] * mat(2, 2);

	return ret;
}  // end TransformNormalPureRotation

/*
**-----------------------------------------------------------------------------
**  Name:       MatrixInverse
**  Purpose:	Creates the inverse of a 4x4 matrix
**-----------------------------------------------------------------------------
*/

static void	lubksb(D3DMATRIX & a, int *indx, float *b);
static void ludcmp(D3DMATRIX & a, int *indx, float *d);

D3DMATRIX   
MatrixInverse(const D3DMATRIX & m)
{
	D3DMATRIX	n, y;
	int			i, j, indx[4];
	float		d, col[4];

	n = m;
	ludcmp(n, indx, &d);

	for (j=0; j<4; j++) {
		for (i=0; i<4; i++) {
			col[i] = 0.0f;
		}
		col[j] = 1.0f;
		lubksb(n, indx, col);
		for (i=0; i<4; i++) {
			y(i, j) = col[i];
		}
	}
	return y;
} // end MatrixInverse

/*
**-----------------------------------------------------------------------------
**  Name:       lubksb
**  Purpose:	backward subsitution
**-----------------------------------------------------------------------------
*/

static void 
lubksb(D3DMATRIX & a, int *indx, float *b)
{
	int		i, j, ii=-1, ip;
	float	sum;

	for (i=0; i<4; i++) {
		ip = indx[i];
		sum = b[ip];
		b[ip] = b[i];
		if (ii>=0) {
			for (j=ii; j<=i-1; j++) {
				sum -= a(i, j) * b[j];
			}
		} else if (sum != 0.0) {
			ii = i;
		}
		b[i] = sum;
	}
	for (i=3; i>=0; i--) {
		sum = b[i];
		for (j=i+1; j<4; j++) {
			sum -= a(i, j) * b[j];
		}
		b[i] = sum/a(i, i);
	}
} // end lubksb

/*
**-----------------------------------------------------------------------------
**  Name:       ludcmp
**  Purpose:	LU decomposition
**-----------------------------------------------------------------------------
*/

static void 
ludcmp(D3DMATRIX & a, int *indx, float *d)
{
	float	vv[4];               /* implicit scale for each row */
	float	big, dum, sum, tmp;
	int		i, imax, j, k;

	*d = 1.0f;
	for (i=0; i<4; i++) {
		big = 0.0f;
		for (j=0; j<4; j++) {
			if ((tmp = (float) fabs(a(i, j))) > big) {
				big = tmp;
			}
		}
		/*
		if (big == 0.0f) {
			printf("ludcmp(): singular matrix found...\n");
			exit(1);
		}
		*/
		vv[i] = 1.0f/big;
	}
	for (j=0; j<4; j++) {
		for (i=0; i<j; i++) {
			sum = a(i, j);
			for (k=0; k<i; k++) {
				sum -= a(i, k) * a(k, j);
			}
			a(i, j) = sum;
		}
		big = 0.0f;
		for (i=j; i<4; i++) {
			sum = a(i, j);
			for (k=0; k<j; k++) {
				sum -= a(i, k)*a(k, j);
			}
			a(i, j) = sum;
			if ((dum = vv[i] * (float)fabs(sum)) >= big) {
				big = dum;
				imax = i;
			}
		}
		if (j != imax) {
			for (k=0; k<4; k++) {
				dum = a(imax, k);
				a(imax, k) = a(j, k);
				a(j, k) = dum;
			}
			*d = -(*d);
			vv[imax] = vv[j];
		}
		indx[j] = imax;
		if (a(j, j) == 0.0f) {
			a(j, j) = 1.0e-20f;      /* can be 0.0 also... */
		}
		if (j != 3) {
			dum = 1.0f/a(j, j);
			for (i=j+1; i<4; i++) {
				a(i, j) *= dum;
			}
		}
	}
} // end ludcmp
  
/*
**-----------------------------------------------------------------------------
**  Name:       Matrix Transpose
**  Purpose:	[M] = [M]'
**-----------------------------------------------------------------------------
*/

D3DMATRIX 
MatrixTranspose(const D3DMATRIX & m)
{
	D3DMATRIX	ret;
	int			i, j;

	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			ret(i, j) = m(j, i);
		}
	}

	return ret;
} // end MatrixTranspose

/*
	Class Methods
*/

/*
**-----------------------------------------------------------------------------
**  Name:       Light::Light
**  Purpose:	Constructor
**-----------------------------------------------------------------------------
*/

Light::Light(LPDIRECT3D2 lpD3D)
{
    memset(&light, 0, sizeof(D3DLIGHT2));
	light.dwSize = sizeof(D3DLIGHT2);
    lpD3D->CreateLight(&lpD3DLight, NULL);
	changed = 1;

	// default to no attenuation with distance
    light.dvAttenuation0 = 1.0f;
    light.dvAttenuation1 = 0.0f;
    light.dvAttenuation2 = 0.0f;

	// default to no max distance
	light.dvRange = D3DLIGHT_RANGE_MAX;
	// default to linear ramp from inner to outer cone
	light.dvFalloff = 1.0f;
	// default to on
	light.dwFlags = D3DLIGHT_ACTIVE;
} // end of Light::Light

/*
**-----------------------------------------------------------------------------
**  Name:       Light::~Light
**  Purpose:	Destructor
**-----------------------------------------------------------------------------
*/

Light::~Light()
{
	if (lpD3DLight) {
		lpD3DLight->Release();
		lpD3DLight = NULL;
	}
} // end Light::~Light
  
/*
**-----------------------------------------------------------------------------
**  Name:       PointLight::PointLight
**  Purpose:	Constructor
**-----------------------------------------------------------------------------
*/

PointLight::PointLight(LPDIRECT3D2		lpD3D, 
					   const D3DVECTOR&	color, 
					   const D3DVECTOR&	position) : Light(lpD3D)
{
    light.dltType = D3DLIGHT_POINT;
    light.dcvColor.r = color[0];
    light.dcvColor.g = color[1];
    light.dcvColor.b = color[2];
    light.dvPosition.x = position[0];
    light.dvPosition.y = position[1];
    light.dvPosition.z = position[2];

	changed = 1;
} // end PointLight::PointLight
  
/*
**-----------------------------------------------------------------------------
**  Name:       SpotLight::SpotLight
**  Purpose:	Constructor
**-----------------------------------------------------------------------------
*/

SpotLight::SpotLight(LPDIRECT3D2		lpD3D, 
					 const D3DVECTOR&	color, 
					 const D3DVECTOR&	position, 
					 const D3DVECTOR&	direction, 
					 const float		umbra_angle,
					 const float		penumbra_angle) : Light(lpD3D)
{
    light.dltType = D3DLIGHT_SPOT;
    light.dcvColor.r = color[0];
    light.dcvColor.g = color[1];
    light.dcvColor.b = color[2];
    light.dvPosition.x = position[0];
    light.dvPosition.y = position[1];
    light.dvPosition.z = position[2];
    light.dvDirection.x = direction[0];
    light.dvDirection.y = direction[1];
    light.dvDirection.z = direction[2];
	light.dvTheta = umbra_angle;
	light.dvPhi = penumbra_angle;

	changed = 1;
} // end of SpotLight::SpotLight
  
/*
**-----------------------------------------------------------------------------
**  Name:       DirectionalLight::DirectionalLight
**  Purpose:	Constructor
**-----------------------------------------------------------------------------
*/

DirectionalLight::DirectionalLight(LPDIRECT3D2		lpD3D, 
								   const D3DVECTOR&	color, 
								   const D3DVECTOR&	direction) : Light(lpD3D)
{
    light.dltType = D3DLIGHT_DIRECTIONAL;
    light.dcvColor.r = color[0];
    light.dcvColor.g = color[1];
    light.dcvColor.b = color[2];
    light.dvDirection.x = direction[0];
    light.dvDirection.y = direction[1];
    light.dvDirection.z = direction[2];

	changed = 1;
}	// end of DirectionalLight::DirectionalLight

/*
//-----------------------------------------------------------------------------
//  Name:       ParallelLight::ParallelLight
//  Purpose:	Constructor
//-----------------------------------------------------------------------------

ParallelPointLight::ParallelPointLight(LPDIRECT3D2			lpD3D, 
									   const D3DVECTOR&	color, 
									   const D3DVECTOR&	position) : Light(lpD3D)
{
    light.dltType = D3DLIGHT_PARALLELPOINT;
    light.dcvColor.r = color[0];
    light.dcvColor.g = color[1];
    light.dcvColor.b = color[2];
    light.dvPosition.x = position[0];
    light.dvPosition.y = position[1];
    light.dvPosition.z = position[2];

	changed = 1;
} // end of ParallelPointLight::ParallelPointLight constructor
*/

/*
**-----------------------------------------------------------------------------
**  Name:       Material::Material
**  Purpose:	Constructor
**-----------------------------------------------------------------------------
*/

Material::Material(LPDIRECT3D2 lpD3D, LPDIRECT3DDEVICE2 lpDev)
{
    memset(&Mat, 0, sizeof(D3DMATERIAL));
	Mat.dwSize = sizeof(D3DMATERIAL);

	// leave the default material black

    Mat.dwRampSize = 16;
	Mat.diffuse.a = 1.0f;

	lpD3D->CreateMaterial(&lpMat, NULL);
    lpMat->SetMaterial(&Mat);
    lpMat->GetHandle(lpDev, &hMat);

	changed = 1;
	Set();
} // end of Material::Material
  
/*
**-----------------------------------------------------------------------------
**  Name:       Material::~Material
**  Purpose:	Destructor
**-----------------------------------------------------------------------------
*/

Material::~Material()
{
	if (lpMat) {
		lpMat->Release();
		lpMat = NULL;
	}
} // end of Material::~Material
  
/*
**-----------------------------------------------------------------------------
**  Name:       rnd
**  Purpose:	Returns a random number (float) in the range [0.0 - 1.0]
**-----------------------------------------------------------------------------
*/

float 
rnd(void)
{
	return float(rand())/RAND_MAX;
}	// end of rnd()


/*
**-----------------------------------------------------------------------------
**  end of File
**-----------------------------------------------------------------------------
*/


