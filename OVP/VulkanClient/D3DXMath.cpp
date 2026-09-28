// not upstream: the D3DX math functions the client uses, with D3DX semantics

#include "D3DXMath.h"

D3DXMATRIX &D3DXMATRIX::operator*= (const D3DXMATRIX &mat)
{
	D3DXMatrixMultiply (this, this, &mat);
	return *this;
}

D3DXVECTOR3 *D3DXVec3Normalize (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV)
{
	float len = D3DXVec3Length (pV);
	if (!len) { pOut->x = pOut->y = pOut->z = 0.0f; return pOut; }
	float i = 1.0f/len;
	pOut->x = pV->x*i; pOut->y = pV->y*i; pOut->z = pV->z*i;
	return pOut;
}

D3DXVECTOR3 *D3DXVec3TransformCoord (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM)
{
	float w = pM->_14*pV->x + pM->_24*pV->y + pM->_34*pV->z + pM->_44;
	float x = pM->_11*pV->x + pM->_21*pV->y + pM->_31*pV->z + pM->_41;
	float y = pM->_12*pV->x + pM->_22*pV->y + pM->_32*pV->z + pM->_42;
	float z = pM->_13*pV->x + pM->_23*pV->y + pM->_33*pV->z + pM->_43;
	pOut->x = x/w; pOut->y = y/w; pOut->z = z/w;
	return pOut;
}

D3DXVECTOR3 *D3DXVec3TransformNormal (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM)
{
	float x = pM->_11*pV->x + pM->_21*pV->y + pM->_31*pV->z;
	float y = pM->_12*pV->x + pM->_22*pV->y + pM->_32*pV->z;
	float z = pM->_13*pV->x + pM->_23*pV->y + pM->_33*pV->z;
	pOut->x = x; pOut->y = y; pOut->z = z;
	return pOut;
}

D3DXVECTOR4 *D3DXVec3Transform (D3DXVECTOR4 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM)
{
	D3DXVECTOR4 r;
	r.x = pM->_11*pV->x + pM->_21*pV->y + pM->_31*pV->z + pM->_41;
	r.y = pM->_12*pV->x + pM->_22*pV->y + pM->_32*pV->z + pM->_42;
	r.z = pM->_13*pV->x + pM->_23*pV->y + pM->_33*pV->z + pM->_43;
	r.w = pM->_14*pV->x + pM->_24*pV->y + pM->_34*pV->z + pM->_44;
	*pOut = r;
	return pOut;
}

D3DXVECTOR4 *D3DXVec4Transform (D3DXVECTOR4 *pOut, const D3DXVECTOR4 *pV, const D3DXMATRIX *pM)
{
	D3DXVECTOR4 r;
	r.x = pM->_11*pV->x + pM->_21*pV->y + pM->_31*pV->z + pM->_41*pV->w;
	r.y = pM->_12*pV->x + pM->_22*pV->y + pM->_32*pV->z + pM->_42*pV->w;
	r.z = pM->_13*pV->x + pM->_23*pV->y + pM->_33*pV->z + pM->_43*pV->w;
	r.w = pM->_14*pV->x + pM->_24*pV->y + pM->_34*pV->z + pM->_44*pV->w;
	*pOut = r;
	return pOut;
}

D3DXMATRIX *D3DXMatrixIdentity (D3DXMATRIX *pOut)
{
	memset (pOut, 0, sizeof(D3DXMATRIX));
	pOut->_11 = pOut->_22 = pOut->_33 = pOut->_44 = 1.0f;
	return pOut;
}

D3DXMATRIX *D3DXMatrixMultiply (D3DXMATRIX *pOut, const D3DXMATRIX *pM1, const D3DXMATRIX *pM2)
{
	D3DXMATRIX r; // pOut may alias pM1 or pM2
	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++)
			r.m[i][j] = pM1->m[i][0]*pM2->m[0][j] + pM1->m[i][1]*pM2->m[1][j] + pM1->m[i][2]*pM2->m[2][j] + pM1->m[i][3]*pM2->m[3][j];
	*pOut = r;
	return pOut;
}

D3DXMATRIX *D3DXMatrixInverse (D3DXMATRIX *pOut, float *pDeterminant, const D3DXMATRIX *pM)
{
	const float *a = &pM->_11;
	float inv[16];
	inv[0]  =  a[5]*a[10]*a[15] - a[5]*a[11]*a[14] - a[9]*a[6]*a[15] + a[9]*a[7]*a[14] + a[13]*a[6]*a[11] - a[13]*a[7]*a[10];
	inv[4]  = -a[4]*a[10]*a[15] + a[4]*a[11]*a[14] + a[8]*a[6]*a[15] - a[8]*a[7]*a[14] - a[12]*a[6]*a[11] + a[12]*a[7]*a[10];
	inv[8]  =  a[4]*a[9]*a[15]  - a[4]*a[11]*a[13] - a[8]*a[5]*a[15] + a[8]*a[7]*a[13] + a[12]*a[5]*a[11] - a[12]*a[7]*a[9];
	inv[12] = -a[4]*a[9]*a[14]  + a[4]*a[10]*a[13] + a[8]*a[5]*a[14] - a[8]*a[6]*a[13] - a[12]*a[5]*a[10] + a[12]*a[6]*a[9];
	inv[1]  = -a[1]*a[10]*a[15] + a[1]*a[11]*a[14] + a[9]*a[2]*a[15] - a[9]*a[3]*a[14] - a[13]*a[2]*a[11] + a[13]*a[3]*a[10];
	inv[5]  =  a[0]*a[10]*a[15] - a[0]*a[11]*a[14] - a[8]*a[2]*a[15] + a[8]*a[3]*a[14] + a[12]*a[2]*a[11] - a[12]*a[3]*a[10];
	inv[9]  = -a[0]*a[9]*a[15]  + a[0]*a[11]*a[13] + a[8]*a[1]*a[15] - a[8]*a[3]*a[13] - a[12]*a[1]*a[11] + a[12]*a[3]*a[9];
	inv[13] =  a[0]*a[9]*a[14]  - a[0]*a[10]*a[13] - a[8]*a[1]*a[14] + a[8]*a[2]*a[13] + a[12]*a[1]*a[10] - a[12]*a[2]*a[9];
	inv[2]  =  a[1]*a[6]*a[15]  - a[1]*a[7]*a[14]  - a[5]*a[2]*a[15] + a[5]*a[3]*a[14] + a[13]*a[2]*a[7]  - a[13]*a[3]*a[6];
	inv[6]  = -a[0]*a[6]*a[15]  + a[0]*a[7]*a[14]  + a[4]*a[2]*a[15] - a[4]*a[3]*a[14] - a[12]*a[2]*a[7]  + a[12]*a[3]*a[6];
	inv[10] =  a[0]*a[5]*a[15]  - a[0]*a[7]*a[13]  - a[4]*a[1]*a[15] + a[4]*a[3]*a[13] + a[12]*a[1]*a[7]  - a[12]*a[3]*a[5];
	inv[14] = -a[0]*a[5]*a[14]  + a[0]*a[6]*a[13]  + a[4]*a[1]*a[14] - a[4]*a[2]*a[13] - a[12]*a[1]*a[6]  + a[12]*a[2]*a[5];
	inv[3]  = -a[1]*a[6]*a[11]  + a[1]*a[7]*a[10]  + a[5]*a[2]*a[11] - a[5]*a[3]*a[10] - a[9]*a[2]*a[7]   + a[9]*a[3]*a[6];
	inv[7]  =  a[0]*a[6]*a[11]  - a[0]*a[7]*a[10]  - a[4]*a[2]*a[11] + a[4]*a[3]*a[10] + a[8]*a[2]*a[7]   - a[8]*a[3]*a[6];
	inv[11] = -a[0]*a[5]*a[11]  + a[0]*a[7]*a[9]   + a[4]*a[1]*a[11] - a[4]*a[3]*a[9]  - a[8]*a[1]*a[7]   + a[8]*a[3]*a[5];
	inv[15] =  a[0]*a[5]*a[10]  - a[0]*a[6]*a[9]   - a[4]*a[1]*a[10] + a[4]*a[2]*a[9]  + a[8]*a[1]*a[6]   - a[8]*a[2]*a[5];
	float det = a[0]*inv[0] + a[1]*inv[4] + a[2]*inv[8] + a[3]*inv[12];
	if (pDeterminant) *pDeterminant = det;
	if (!det) return NULL;
	float idet = 1.0f/det;
	for (int i = 0; i < 16; i++) (&pOut->_11)[i] = inv[i]*idet;
	return pOut;
}

D3DXMATRIX *D3DXMatrixScaling (D3DXMATRIX *pOut, float sx, float sy, float sz)
{
	D3DXMatrixIdentity (pOut);
	pOut->_11 = sx; pOut->_22 = sy; pOut->_33 = sz;
	return pOut;
}

D3DXMATRIX *D3DXMatrixRotationAxis (D3DXMATRIX *pOut, const D3DXVECTOR3 *pV, float Angle)
{
	D3DXVECTOR3 v;
	D3DXVec3Normalize (&v, pV);
	float s = sinf (Angle), c = cosf (Angle), d = 1.0f - c;
	D3DXMatrixIdentity (pOut);
	pOut->_11 = d*v.x*v.x + c;     pOut->_12 = d*v.x*v.y + s*v.z; pOut->_13 = d*v.x*v.z - s*v.y;
	pOut->_21 = d*v.y*v.x - s*v.z; pOut->_22 = d*v.y*v.y + c;     pOut->_23 = d*v.y*v.z + s*v.x;
	pOut->_31 = d*v.z*v.x + s*v.y; pOut->_32 = d*v.z*v.y - s*v.x; pOut->_33 = d*v.z*v.z + c;
	return pOut;
}

D3DXMATRIX *D3DXMatrixLookAtRH (D3DXMATRIX *pOut, const D3DXVECTOR3 *pEye, const D3DXVECTOR3 *pAt, const D3DXVECTOR3 *pUp)
{
	D3DXVECTOR3 z = *pEye - *pAt, x, y;
	D3DXVec3Normalize (&z, &z);
	D3DXVec3Cross (&x, pUp, &z);
	D3DXVec3Normalize (&x, &x);
	D3DXVec3Cross (&y, &z, &x);
	pOut->_11 = x.x; pOut->_12 = y.x; pOut->_13 = z.x; pOut->_14 = 0.0f;
	pOut->_21 = x.y; pOut->_22 = y.y; pOut->_23 = z.y; pOut->_24 = 0.0f;
	pOut->_31 = x.z; pOut->_32 = y.z; pOut->_33 = z.z; pOut->_34 = 0.0f;
	pOut->_41 = -D3DXVec3Dot (&x, pEye); pOut->_42 = -D3DXVec3Dot (&y, pEye); pOut->_43 = -D3DXVec3Dot (&z, pEye); pOut->_44 = 1.0f;
	return pOut;
}

D3DXMATRIX *D3DXMatrixOrthoOffCenterLH (D3DXMATRIX *pOut, float l, float r, float b, float t, float zn, float zf)
{
	D3DXMatrixIdentity (pOut);
	pOut->_11 = 2.0f/(r-l);
	pOut->_22 = 2.0f/(t-b);
	pOut->_33 = 1.0f/(zf-zn);
	pOut->_41 = (l+r)/(l-r);
	pOut->_42 = (t+b)/(b-t);
	pOut->_43 = zn/(zn-zf);
	return pOut;
}

D3DXMATRIX *D3DXMatrixOrthoOffCenterRH (D3DXMATRIX *pOut, float l, float r, float b, float t, float zn, float zf)
{
	D3DXMatrixOrthoOffCenterLH (pOut, l, r, b, t, zn, zf);
	pOut->_33 = -pOut->_33;
	return pOut;
}

// rotation matrix of the quaternion (0, 0, sin(a/2), cos(a/2)): a rotation by a around z
static void RotationZ (D3DXMATRIX *pOut, float a)
{
	float s = sinf (a*0.5f), c = cosf (a*0.5f);
	D3DXMatrixIdentity (pOut);
	pOut->_11 = 1.0f - 2.0f*s*s; pOut->_12 = 2.0f*s*c;
	pOut->_21 = -2.0f*s*c;       pOut->_22 = 1.0f - 2.0f*s*s;
}

static void Translation (D3DXMATRIX *pOut, float x, float y)
{
	D3DXMatrixIdentity (pOut);
	pOut->_41 = x; pOut->_42 = y;
}

// M = Tsc^-1 * Rsr^-1 * S * Rsr * Tsc * Trc^-1 * R * Trc * T (D3DXMatrixTransformation with z = 0)
D3DXMATRIX *D3DXMatrixTransformation2D (D3DXMATRIX *pOut, const D3DXVECTOR2 *pScalingCenter, float ScalingRotation,
	const D3DXVECTOR2 *pScaling, const D3DXVECTOR2 *pRotationCenter, float Rotation, const D3DXVECTOR2 *pTranslation)
{
	D3DXVECTOR2 sc = pScalingCenter ? *pScalingCenter : D3DXVECTOR2(0.0f, 0.0f);
	D3DXVECTOR2 s  = pScaling ? *pScaling : D3DXVECTOR2(1.0f, 1.0f);
	D3DXVECTOR2 rc = pRotationCenter ? *pRotationCenter : D3DXVECTOR2(0.0f, 0.0f);
	D3DXVECTOR2 t  = pTranslation ? *pTranslation : D3DXVECTOR2(0.0f, 0.0f);
	D3DXMATRIX m, tmp;
	Translation (&m, -sc.x, -sc.y);
	RotationZ (&tmp, -ScalingRotation);  D3DXMatrixMultiply (&m, &m, &tmp);
	D3DXMatrixScaling (&tmp, s.x, s.y, 1.0f);  D3DXMatrixMultiply (&m, &m, &tmp);
	RotationZ (&tmp, ScalingRotation);   D3DXMatrixMultiply (&m, &m, &tmp);
	Translation (&tmp, sc.x-rc.x, sc.y-rc.y); D3DXMatrixMultiply (&m, &m, &tmp);
	RotationZ (&tmp, Rotation);          D3DXMatrixMultiply (&m, &m, &tmp);
	Translation (&tmp, rc.x+t.x, rc.y+t.y);   D3DXMatrixMultiply (&m, &m, &tmp);
	*pOut = m;
	return pOut;
}

D3DXMATRIX *D3DXMatrixAffineTransformation2D (D3DXMATRIX *pOut, float Scaling, const D3DXVECTOR2 *pRotationCenter,
	float Rotation, const D3DXVECTOR2 *pTranslation)
{
	float s = sinf (Rotation*0.5f);
	float tmp1 = 1.0f - 2.0f*s*s;
	float tmp2 = 2.0f*s*cosf (Rotation*0.5f);
	D3DXMatrixIdentity (pOut);
	pOut->_11 = Scaling*tmp1;  pOut->_12 = Scaling*tmp2;
	pOut->_21 = -Scaling*tmp2; pOut->_22 = Scaling*tmp1;
	if (pRotationCenter) {
		float x = pRotationCenter->x, y = pRotationCenter->y;
		pOut->_41 = y*tmp2 - x*tmp1 + x;
		pOut->_42 = -x*tmp2 - y*tmp1 + y;
	}
	if (pTranslation) {
		pOut->_41 += pTranslation->x;
		pOut->_42 += pTranslation->y;
	}
	return pOut;
}

BOOL D3DXIntersectTri (const D3DXVECTOR3 *p0, const D3DXVECTOR3 *p1, const D3DXVECTOR3 *p2,
	const D3DXVECTOR3 *pRayPos, const D3DXVECTOR3 *pRayDir, float *pU, float *pV, float *pDist)
{
	D3DXVECTOR3 e1 = *p1 - *p0, e2 = *p2 - *p0, p, q;
	D3DXVec3Cross (&p, pRayDir, &e2);
	float det = D3DXVec3Dot (&e1, &p);
	if (!det) return FALSE;
	float idet = 1.0f/det;
	D3DXVECTOR3 s = *pRayPos - *p0;
	float u = D3DXVec3Dot (&s, &p) * idet;
	D3DXVec3Cross (&q, &s, &e1);
	float v = D3DXVec3Dot (pRayDir, &q) * idet;
	float t = D3DXVec3Dot (&e2, &q) * idet;
	if (u >= 0.0f && v >= 0.0f && u + v <= 1.0f && t >= 0.0f) {
		if (pU) *pU = u;
		if (pV) *pV = v;
		if (pDist) *pDist = fabsf (t);
		return TRUE;
	}
	return FALSE;
}

int D3DXComputeBoundingSphere (const D3DXVECTOR3 *pFirstPosition, DWORD NumVertices, DWORD dwStride,
	D3DXVECTOR3 *pCenter, float *pRadius)
{
	if (!pFirstPosition || !pCenter || !pRadius) return -1; // D3DERR_INVALIDCALL
	const char *p = (const char*)pFirstPosition;
	D3DXVECTOR3 c (0.0f, 0.0f, 0.0f);
	for (DWORD i = 0; i < NumVertices; i++) c += *(const D3DXVECTOR3*)(p + i*dwStride);
	if (NumVertices) c /= (float)NumVertices;
	float r = 0.0f;
	for (DWORD i = 0; i < NumVertices; i++) {
		D3DXVECTOR3 d = *(const D3DXVECTOR3*)(p + i*dwStride) - c;
		float l = D3DXVec3Length (&d);
		if (l > r) r = l;
	}
	*pCenter = c;
	*pRadius = r;
	return 0; // D3D_OK
}
