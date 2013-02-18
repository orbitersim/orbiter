// ==============================================================
// Utilities
// Part of the ORBITER VISUALISATION PROJECT (OVP) D3D9 Client
// Released under GNU General Public License
// Copyright (C) 2006-2007 Martin Schweiger
//				 2012-2013 Jarmo Nikkanen
//				 2012 Émile "Bibi Uncle" Grégoire
// ==============================================================



#define STRICT
#include "D3D9util.h"
#include "D3D9Client.h"

float saturate(float x) { if (x>1) return 1; if (x<0) return 0; return x; }

void DX9Mat2MatExt(D3DMATERIAL9 *pIn, D3D9MatExt *pOut)
{
	memcpy(pOut, pIn, 64);
	pOut->Specular.a = pIn->Power;
	pOut->Reflect = pIn->Specular;
	pOut->Reflect.a = 0.0f;
	pOut->Fresnel = 0.0f;
	pOut->FOffset = 1.0f;
	pOut->DislMag = 0.0f;
	pOut->DislScale = 0.0f;
	pOut->pDissolve = NULL;
	pOut->ModFlags = 0;
}

void MatExt2DX9Mat(D3D9MatExt *pIn, D3DMATERIAL9 *pOut)
{
	memcpy(pOut, pIn, 64);
	pOut->Power = pIn->Specular.a;
	pOut->Specular.a = 1.0f;
}

void SurfaceLighting(D3D9Light *light, OBJHANDLE hP, OBJHANDLE hO, float ao)
{
	// hP=hPlanet, hS=hSun
	VECTOR3 GO, GS, GP;
	
	D3DXVECTOR3 _one(1,1,1);

	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	oapiGetGlobalPos (hO, &GO);				// object position
	oapiGetGlobalPos (hS, &GS);				// sun position
	oapiGetGlobalPos (hP, &GP);				// planet position

	VECTOR3 S = GS-GO;							// sun's position from base
	VECTOR3 P = unit(GO-GP);
	
	float s  = float(length(S));				// sun's distance
	float rs = float(oapiGetSize(hS)) / s;
	float h  = float(dotp(S,P)) / s;			// sun elevation
	float d  = 0.173f;							// sun elevation for dispersion
	float ae = 0.242f;							// sun elevation for ambient
	float aq = 0.342f;
	
	float amb0 = 0.0f;
	float disp = 0.0f;
	float amb  = 0.0f;

	const ATMCONST *atm = (oapiGetObjectType(hP)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hP) : NULL);

	if (atm) {
		amb0 = min (0.7f, log(float(atm->rho0)+1.0f)*0.4f);
		disp = max (0.02f, min(0.9f, log(float(atm->rho0)+1.0f)));
	}

	D3DXVECTOR3 lcol;
	D3DXVECTOR3 r0 = _one - D3DXVECTOR3(0.65f, 0.75f, 1.0f) * disp;

	if (atm) { // case 1: planet has atmosphere
		lcol = (r0 + (_one-r0) * saturate(h/d)) * saturate((h+rs)/(2.0f*rs)); 
		amb  = saturate((h+ae)/aq);	
		amb  = saturate(max(amb0*amb-0.05f,ao));
		lcol *= 1.0f-amb*0.5f; // reduce direct light component to avoid overexposure
	} 
	else {   // case 2: planet has no atmosphere
		lcol = r0 * saturate((h+rs)/(2.0f*rs)); 
		amb  = ao;
		lcol *= 1.0f-amb*0.5f; // reduce direct light component to avoid overexposure
	}

	light->Diffuse   = light->Specular = D3DXCOLOR(lcol.x, lcol.y, lcol.z, 1.0f); 
	light->Ambient   = D3DXCOLOR(amb, amb, amb, 1.0f);
	light->Direction = D3DXVEC(S) * (-1.0f/s);
}







void OrbitalLighting(D3D9Light *light, OBJHANDLE hP, VECTOR3 GO, float ao)
{
	VECTOR3 GS, GP;
	
	D3DXVECTOR3 _one(1,1,1);

	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	oapiGetGlobalPos(hS, &GS);				// sun position
	oapiGetGlobalPos(hP, &GP);				// planet position

	VECTOR3 S = GS-GO;						// sun's position from object
	VECTOR3 P = GO-GP;

	double s  = length(S);	

	if (hP==hS) {
		light->Diffuse   = light->Specular = D3DXCOLOR(1.0f, 1.0f, 1.0f, 1.0f); 
		light->Ambient   = D3DXCOLOR(ao, ao, ao, 1.0f);
		light->Direction = D3DXVEC(S) * (-1.0f/float(s));
		return;
	}

	double r   = length(P);
	double pres = 1.0;  
	double size = oapiGetSize(hP);
	double grav = oapiGetMass(hP) * 6.67259e-11 / (size*size);

	float aalt = 1.0f;
	float amb0 = 0.0f;
	float disp = 0.0f;
	float amb  = 0.0f;
	float aq   = 0.342f;
	float ae   = 0.242f;							
	float al   = 0.0f;		
	float k    = float(sqrt(r*r-size*size));		// HOrizon distance
	float alt  = float(r-size);
	float rs   = float(oapiGetSize(hS) / s);
	float ac   = float(-dotp(S,P)/(r*s));					// sun elevation

	if (ac>1.0f) ac=1.0f; if (ac<-1.0f) ac=-1.0f;

	ac = acos(ac) - asin(float(size/r));
	
	if (ac>1.39f)  ac = 1.39f;
	if (ac<-1.39f) ac = -1.39f;

	float h = tan(ac);

	const ATMCONST *atm = (oapiGetObjectType(hP)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hP) : NULL);

	if (atm) {
		aalt = float(atm->p0 * log(atm->p0/pres) / (atm->rho0*grav));
		amb0 = float(min (0.7, log(atm->rho0+1.0f)*0.4));
		disp = float(max (0.02, min(0.9, log(atm->rho0+1.0))));
	}

	if (alt>10e3f) al = aalt / k;
	else           al = 0.173f;
	
	D3DXVECTOR3 lcol(1,1,1);
	D3DXVECTOR3 r0 = _one - D3DXVECTOR3(0.65f, 0.75f, 1.0f) * disp;

	if (atm) { 
		lcol = (r0 + (_one-r0) * saturate((h/al))) * saturate((h+rs)/(2.0f*rs));
		amb = amb0 / (alt*0.5e-4f + 1.0f);
		amb  = saturate(max(amb*saturate(((h+ae)/aq)-0.05f), ao));
		lcol *= 1.0f-amb*0.5f; // reduce direct light component to avoid overexposure
	} 
	else {
		lcol = r0 * saturate((h+rs)/(2.0f*rs)); 
		amb  = ao;
		lcol *= 1.0f-amb*0.5f; // reduce direct light component to avoid overexposure
	}
	
	light->Diffuse   = light->Specular = D3DXCOLOR(lcol.x, lcol.y, lcol.z, 1.0f); 
	light->Ambient   = D3DXCOLOR(amb, amb, amb, 1.0f);
	light->Direction = D3DXVEC(S) * (-1.0f/float(s));
}



// ===========================================
// Remove unecessary spaces and tablations
// when reading a file.
// ===========================================
/*
char* _fgets(char* cbuf, int num, FILE* stream, bool keepOneSpace)
{
	
	cbuf[0] = '\0';

	char* temp = new char[num];

	if(fgets(temp, num, stream) == NULL) {
		delete []temp;
		return NULL;
	}

	bool equalSign = false;
	bool firstLetter = false;
	int cbufLine = 0;

	for(int i=0; i<num; i++)
	{
		if(temp[i] == ' ' || temp[i] == '\t')
		{
			if(keepOneSpace && temp[max(0, i-1)] != temp[i])
			{
				cbuf[cbufLine] = temp[i];
				cbufLine++;
				continue;
			}

			else if(equalSign == false)
				continue;

			else if(equalSign == true)
			{
				if(!firstLetter)
					continue;

				else
				{
					cbuf[cbufLine] = temp[i];
					cbufLine++;
					continue;
				}
			}
		}

		else if(temp[i] == '\0' || temp[i] == ';' || temp[i] == 0x0A || temp[i] == 0x0D)
			break;

		else
		{
			if(equalSign == true && firstLetter == false)
				firstLetter = true;

			if(temp[i] == '=')
				equalSign = true;

			cbuf[cbufLine] = temp[i];
			cbufLine++;
		}
	}

	cbuf[cbufLine] = '\0';

	while(cbufLine>0) {
		cbufLine--;
		if (cbuf[cbufLine]==' ' || cbuf[cbufLine]=='\t') cbuf[cbufLine] = '\0';
		else break;
	}

	delete[] temp;
	
	return cbuf;
}
*/





void strremchr(char *str, int idx)
{
	while (str[idx]!='\0') {
		str[idx] = str[idx+1];
		idx++;
	}
}

// --------------------------------------------------------------
// Improved version of fgets
// Copyright (C) 2012 Jarmo Nikkanen
// Return:
// -1 = eof
//  0 = invalid string
//  1 = success without '=' in string
//  2 = success with '=' in string 
//
// param:
//  0x01 = Don't Remove spaces from both sides of '='
//  0x02 = Don't convert '/' to '\'
//  0x04 = Convert to upper case
//  0x08 = Remove '=' if exists


int fgets2(char *buf, int cmax, FILE *file, DWORD param)  //bool bEquality, bool bSlash)
{
	bool bEql = false;
	bool bEquality = (param&0x01)==0;
	bool bSlash = (param&0x02)==0;
	bool bEqlRem = (param&0x08)!=0;
	bool bUpper = (param&0x04)!=0;

	if (fgets(buf, cmax, file)==NULL) return -1;

	int num = strlen(buf);

	if (num==(cmax-1)) LogErr("Insufficient buffer size in fgets2() size=%d, string=(%s)",cmax,buf);
		
	// Replace tabs with spaces and cut a comment parts and unwanted chars
	// Check the existance of equality sign '='
	for (int i=0;i<num;i++) {
		char c = buf[i];
		if (c=='=') bEql=true;
		if (c=='=' && bEqlRem) buf[i]=' ';
		if (c=='\t') buf[i]=' ';
		if (c=='/' && bSlash) buf[i]='\\';
		if (c==';' || c==0xA || c==0xD) {
			buf[i]='\0';
			break;
		}
	}

	num = strlen(buf);	
	if (num==0) return 0;

	// Remove spaces from the end of the line
	while (num>0) {
		num--;
		if (buf[num]==' ') buf[num]='\0';
		else break;
	}

	// Remove spaces from the front of the line
	while (buf[0]==' ') strremchr(buf,0);

	num = strlen(buf);	
	if (num==0) return 0;

	// Remove repeatitive spaces if exists. (double trible spaces and so on)
	// At this point a space can not be the last char, therefore [i+1] is not a problem
	for (int i=0;i<num;) {
		if (buf[i]==' ' && buf[i+1]==' ') { 
			strremchr(buf, i);
			num--; 
		}
		else i++;
	}

	num = strlen(buf);	
	if (num==0) return 0;

	// Remove spaces from both sides of '=' if exists
	if (bEql && bEquality) {
		if (buf[0]=='=' || buf[num-1]=='=') return 0;
		for (int i=0;i<num;i++) if (buf[i]=='=') {
			if (buf[i+1]==' ') strremchr(buf,i+1);
			if (buf[i-1]==' ') strremchr(buf,i-1);
			break;
		}
	}

	if (bUpper) _strupr_s(buf,strlen(buf));

	// Done
	if (bEql) return 2;
	return 1;
}




// =======================================================================
// Some utility methods for D3D vectors and matrices
// ============================================================================

D3DXVECTOR3 expv(D3DXVECTOR3 &v)
{
	return D3DXVECTOR3(exp(v.x), exp(v.y), exp(v.z));
}


float D3DXVec3Angle(D3DXVECTOR3 a, D3DXVECTOR3 b)
{
	D3DXVec3Normalize(&a,&a);
	D3DXVec3Normalize(&b,&b);
	float x = D3DXVec3Dot(&a,&b);
	if (x<-1.0f) x=-1.0f;
	if (x> 1.0f) x= 1.0f;
	return acos(x);
}


float DepthResolution(const LPD3DXVECTOR3 line, const LPD3DXVECTOR3 pos, float zfar, float znear)
{
	D3DXVECTOR3 q;
	float z = D3DXVec3Dot(D3DXVec3Normalize(&q, line), pos);
	return fabs( (znear-zfar)*z*z / (zfar*znear*24.0f) );
}


D3DXVECTOR3 Perpendicular(D3DXVECTOR3 *a)
{
	float x = fabs(a->x);
	float y = fabs(a->y);
	float z = fabs(a->z);
	float m = min(min(x, y), z);
	if (m==x) return D3DXVECTOR3(0, a->z,  a->y);
	if (m==y) return D3DXVECTOR3(a->z, 0, -a->x);	
	else      return D3DXVECTOR3(a->y, -a->x, 0);
}

// Cleate a billboarding matrix. X-axis of the vertex data will be pointing to the camera
//
void D3DMAT_CreateX_Billboard(const D3DXVECTOR3 *toCam, const D3DXVECTOR3 *pos, float size, D3DXMATRIX *pOut)
{
	float hz  = 1.0f/sqrt(toCam->x*toCam->x + toCam->z*toCam->z);

	pOut->_11 =  toCam->x;
	pOut->_12 =  toCam->y;
	pOut->_13 =  toCam->z;
	pOut->_31 = -toCam->z*hz;
	pOut->_32 =  0.0f;
	pOut->_33 =  toCam->x*hz;
	pOut->_21 = -pOut->_12*pOut->_33;
	pOut->_22 =  pOut->_33*pOut->_11 - pOut->_13*pOut->_31;
	pOut->_23 =  pOut->_31*pOut->_12;
	pOut->_41 =  pos->x;
	pOut->_42 =  pos->y;
	pOut->_43 =  pos->z;
	pOut->_14 = pOut->_24 = pOut->_34 = pOut->_44 = 0.0f;
	pOut->_11 *= size; pOut->_12 *= size; pOut->_13 *= size;
	pOut->_21 *= size; pOut->_22 *= size; pOut->_23 *= size;
	pOut->_31 *= size;					  pOut->_33 *= size;
}


// Cleate a billboarding matrix. X-axis of the vertex data will be pointing to the camera
//
void D3DMAT_CreateX_Billboard(const D3DXVECTOR3 *toCam, const D3DXVECTOR3 *pos, const D3DXVECTOR3 *dir, float size, float stretch, D3DXMATRIX *pOut)
{
	D3DXVECTOR3 q,w;
	D3DXVec3Normalize(&q, D3DXVec3Cross(&q, dir, toCam));
	D3DXVec3Normalize(&w, D3DXVec3Cross(&w, &q,  dir));

	pOut->_11 = w.x * size;
	pOut->_12 = w.y * size;
	pOut->_13 = w.z * size;

	pOut->_21 = q.x * size;
	pOut->_22 = q.y * size;
	pOut->_23 = q.z * size;

	pOut->_31 = dir->x * stretch;
	pOut->_32 = dir->y * stretch;
	pOut->_33 = dir->z * stretch;

	pOut->_41 = pos->x;
	pOut->_42 = pos->y;
	pOut->_43 = pos->z;

	pOut->_14 = pOut->_24 = pOut->_34 = pOut->_44 = 0.0f;
}


void D3DMAT_ZeroMatrix(D3DXMATRIX *mat)
{
	ZeroMemory(mat, sizeof (D3DXMATRIX));
}

// ============================================================================
// Matrix identity

void D3DMAT_Identity (D3DXMATRIX *mat)
{
	ZeroMemory(mat, sizeof (D3DXMATRIX));
	mat->_11 = mat->_22 = mat->_33 = mat->_44 = 1.0f;
}

// ============================================================================
// Copy a D3DXMATRIX

void D3DMAT_Copy (D3DXMATRIX *tgt, const D3DXMATRIX *src)
{
	 memcpy(tgt, src, sizeof (D3DXMATRIX)); 
}

void D3DMAT_FromAxis(D3DXMATRIX *mat, const D3DVECTOR *x, const D3DVECTOR *y, const D3DVECTOR *z)
{
	mat->_11 = x->x;
	mat->_21 = x->y;
	mat->_31 = x->z;
	
	mat->_12 = y->x;
	mat->_22 = y->y;
	mat->_32 = y->z;
	
	mat->_13 = z->x;
	mat->_23 = z->y;
	mat->_33 = z->z;
}

void D3DMAT_FromAxisT(D3DXMATRIX *mat, const D3DVECTOR *x, const D3DVECTOR *y, const D3DVECTOR *z)
{
	mat->_11 = x->x;
	mat->_12 = x->y;
	mat->_13 = x->z;
	
	mat->_21 = y->x;
	mat->_22 = y->y;
	mat->_23 = y->z;
	
	mat->_31 = z->x;
	mat->_32 = z->y;
	mat->_33 = z->z;
}

// ============================================================================
// Copy a rotation matrix into a D3DXMATRIX

void D3DMAT_SetRotation (D3DXMATRIX *mat, const MATRIX3 *rot)
{
	mat->_11 = (FLOAT)rot->m11;
	mat->_12 = (FLOAT)rot->m12;
	mat->_13 = (FLOAT)rot->m13;
	mat->_21 = (FLOAT)rot->m21;
	mat->_22 = (FLOAT)rot->m22;
	mat->_23 = (FLOAT)rot->m23;
	mat->_31 = (FLOAT)rot->m31;
	mat->_32 = (FLOAT)rot->m32;
	mat->_33 = (FLOAT)rot->m33;
}

// ============================================================================
// Copy the transpose of a matrix as rotation of a D3D transformation matrix

void D3DMAT_SetInvRotation (D3DXMATRIX *mat, const MATRIX3 *rot)
{
	mat->_11 = (FLOAT)rot->m11;
	mat->_12 = (FLOAT)rot->m21;
	mat->_13 = (FLOAT)rot->m31;
	mat->_21 = (FLOAT)rot->m12;
	mat->_22 = (FLOAT)rot->m22;
	mat->_23 = (FLOAT)rot->m32;
	mat->_31 = (FLOAT)rot->m13;
	mat->_32 = (FLOAT)rot->m23;
	mat->_33 = (FLOAT)rot->m33;
}

// ============================================================================
// Define a rotation matrix from a rotation axis & rotation angle

void D3DMAT_RotationFromAxis (const D3DXVECTOR3 &axis, float angle, D3DXMATRIX *rot)
{
	// Calculate quaternion
	angle *= 0.5f;
	float w = cosf(angle), sina = sinf(angle);
	float x = sina * axis.x;
	float y = sina * axis.y;
	float z = sina * axis.z;

	// Rotation matrix
	float xx = x*x, yy = y*y, zz = z*z;
	float xy = x*y, xz = x*z, yz = y*z;
	float wx = w*x, wy = w*y, wz = w*z;

	rot->_11 = 1 - 2 * (yy+zz);
	rot->_12 =     2 * (xy+wz);
	rot->_13 =     2 * (xz-wy);
	rot->_21 =     2 * (xy-wz);
	rot->_22 = 1 - 2 * (xx+zz);
	rot->_23 =     2 * (yz+wx);
	rot->_31 =     2 * (xz+wy);
	rot->_32 =     2 * (yz-wx);
	rot->_33 = 1 - 2 * (xx+yy);

	rot->_14 = rot->_24 = rot->_34 = rot->_41 = rot->_42 = rot->_43 = 0.0f;
	rot->_44 = 1.0f;
}

// ============================================================================
// Set up a as matrix for ANTICLOCKWISE rotation r around x/y/z-axis

void D3DMAT_RotX  (D3DXMATRIX *mat, double r)
{
	double sinr = sin(r), cosr = cos(r);
	ZeroMemory (mat, sizeof (D3DXMATRIX));
	mat->_22 = mat->_33 = (FLOAT)cosr;
	mat->_23 = -(mat->_32 = (FLOAT)sinr);
	mat->_11 = mat->_44 = 1.0f;
}

void D3DMAT_RotY (D3DXMATRIX *mat, double r)
{
	double sinr = sin(r), cosr = cos(r);
	ZeroMemory (mat, sizeof (D3DXMATRIX));
	mat->_11 = mat->_33 = (FLOAT)cosr;
	mat->_31 = -(mat->_13 = (FLOAT)sinr);
	mat->_22 = mat->_44 = 1.0f;
}

float D3DMAT_BSScaleFactor(const D3DXMATRIX *mat)
{
	float lx = mat->_11*mat->_11 + mat->_12*mat->_12 + mat->_13*mat->_13; 
    float ly = mat->_21*mat->_21 + mat->_22*mat->_22 + mat->_23*mat->_23; 
    float lz = mat->_31*mat->_31 + mat->_32*mat->_32 + mat->_33*mat->_33; 
	return sqrt(max(max(lx,ly),lz));
}


// ============================================================================
// Apply a translation vector toa D3D transformation matrix

void D3DMAT_SetTranslation (D3DXMATRIX *mat, const VECTOR3 *trans)
{
	mat->_41 = (FLOAT)trans->x;
	mat->_42 = (FLOAT)trans->y;
	mat->_43 = (FLOAT)trans->z;
}

// =======================================================================

bool D3DMAT_VectorMatrixMultiply (D3DXVECTOR3 *res, const D3DXVECTOR3 *v, const D3DXMATRIX *mat)
{
    float x = v->x*mat->_11 + v->y*mat->_21 + v->z* mat->_31 + mat->_41;
    float y = v->x*mat->_12 + v->y*mat->_22 + v->z* mat->_32 + mat->_42;
    float z = v->x*mat->_13 + v->y*mat->_23 + v->z* mat->_33 + mat->_43;
    float w = v->x*mat->_14 + v->y*mat->_24 + v->z* mat->_34 + mat->_44;
    
    if (fabs (w) < 1e-5f) return false;

    res->x = x/w;
    res->y = y/w;
    res->z = z/w;
    return true;
}

// =======================================================================
// Name: D3DMath_MatrixInvert()
// Desc: Does the matrix operation: [Q] = inv[A]. Note: this function only
//       works for matrices with [0 0 0 1] for the 4th column.
// =======================================================================

HRESULT D3DMAT_MatrixInvert (D3DXMATRIX *res, D3DXMATRIX *a)
{
    if( fabs(a->_44 - 1.0f) > .001f)
        return E_INVALIDARG;
    if( fabs(a->_14) > .001f || fabs(a->_24) > .001f || fabs(a->_34) > .001f )
        return E_INVALIDARG;

    FLOAT fDetInv = 1.0f / ( a->_11 * ( a->_22 * a->_33 - a->_23 * a->_32 ) -
                             a->_12 * ( a->_21 * a->_33 - a->_23 * a->_31 ) +
                             a->_13 * ( a->_21 * a->_32 - a->_22 * a->_31 ) );

    res->_11 =  fDetInv * ( a->_22 * a->_33 - a->_23 * a->_32 );
    res->_12 = -fDetInv * ( a->_12 * a->_33 - a->_13 * a->_32 );
    res->_13 =  fDetInv * ( a->_12 * a->_23 - a->_13 * a->_22 );
    res->_14 = 0.0f;

    res->_21 = -fDetInv * ( a->_21 * a->_33 - a->_23 * a->_31 );
    res->_22 =  fDetInv * ( a->_11 * a->_33 - a->_13 * a->_31 );
    res->_23 = -fDetInv * ( a->_11 * a->_23 - a->_13 * a->_21 );
    res->_24 = 0.0f;

    res->_31 =  fDetInv * ( a->_21 * a->_32 - a->_22 * a->_31 );
    res->_32 = -fDetInv * ( a->_11 * a->_32 - a->_12 * a->_31 );
    res->_33 =  fDetInv * ( a->_11 * a->_22 - a->_12 * a->_21 );
    res->_34 = 0.0f;

    res->_41 = -( a->_41 * res->_11 + a->_42 * res->_21 + a->_43 * res->_31 );
    res->_42 = -( a->_41 * res->_12 + a->_42 * res->_22 + a->_43 * res->_32 );
    res->_43 = -( a->_41 * res->_13 + a->_42 * res->_23 + a->_43 * res->_33 );
    res->_44 = 1.0f;

    return S_OK;
}