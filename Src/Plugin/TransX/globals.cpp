/* Copyright (c) 2007 Duncan Sharpe, Steve Arch
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
**
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.*/

#define STRICT

#include <windows.h>
#include <cstdio>
#include <cmath>
#include "orbitersdk.h"
#include "shiplist.h"
#include <list>

static int mode;

using namespace std;

// ==============================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
    static char name[] = "TransX";
	MFDMODESPECEX spec;
	spec.name    = name;
	spec.msgproc = TransxMFD::MsgProc;
	spec.context = NULL;
	//Code contributed by Dave Robotham
	ifstream kstream;
	kstream.open("Config\\MFD\\TransX.cfg",NULL);
	if( kstream )
	{
		try
		{
			char kbuf;
			kstream >> kbuf;
			kbuf = toupper(kbuf);
			kstream.close();
			switch( kbuf )
			{
				case 65 : spec.key = OAPI_KEY_A; break;
				case 66 : spec.key = OAPI_KEY_B; break;
				case 67 : spec.key = OAPI_KEY_C; break;
				case 68 : spec.key = OAPI_KEY_D; break;
				case 69 : spec.key = OAPI_KEY_E; break;
				case 70 : spec.key = OAPI_KEY_F; break;
				case 71 : spec.key = OAPI_KEY_G; break;
				case 72 : spec.key = OAPI_KEY_H; break;
				case 73 : spec.key = OAPI_KEY_I; break;
				case 74 : spec.key = OAPI_KEY_J; break;
				case 75 : spec.key = OAPI_KEY_K; break;
				case 76 : spec.key = OAPI_KEY_L; break;
				case 77 : spec.key = OAPI_KEY_M; break;
				case 78 : spec.key = OAPI_KEY_N; break;
				case 79 : spec.key = OAPI_KEY_O; break;
				case 80 : spec.key = OAPI_KEY_P; break;
				case 81 : spec.key = OAPI_KEY_Q; break;
				case 82 : spec.key = OAPI_KEY_R; break;
				case 83 : spec.key = OAPI_KEY_S; break;
				case 84 : spec.key = OAPI_KEY_T; break;
				case 85 : spec.key = OAPI_KEY_U; break;
				case 86 : spec.key = OAPI_KEY_V; break;
				case 87 : spec.key = OAPI_KEY_W; break;
				case 88 : spec.key = OAPI_KEY_X; break;
				case 89 : spec.key = OAPI_KEY_Y; break;
				case 90 : spec.key = OAPI_KEY_Z; break;

				default : spec.key = OAPI_KEY_J; break;
			}
		}
		catch( ... )
		{
			spec.key     = OAPI_KEY_J;
		}
	}

	else
		spec.key = OAPI_KEY_J;

	mode = oapiRegisterMFDMode (spec);

}//end code from Dave Robotham

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	oapiUnregisterMFDMode (mode);
}

DLLCLBK void opcCloseRenderViewport()
{//Clean up
	viewstate::preparetoclose();

	// MS-100706: The following block should be unnecessary,
	// since opcCloseRenderViewport() is called BEFORE the TransX destructors,
	// which means that either GetMfdCount()>0 or shipptrs::destroyshipptrs()
	// has already be called from TransX::~TransX(). Also see note in
	// viewstate::preparetoclose(). A developer should have a look at this.

	if (TransxMFD::GetMfdCount()==0)
	{// MFD's all closed - up to me to clean up!
		shipptrs::destroyshipptrs();
	}
}

static int choose = 0;

DLLCLBK void opcOpenRenderViewport(HWND renderWnd,DWORD width,DWORD height,BOOL fullscreen)
{
	mapfunction *temp=mapfunction::getthemap();//kicks off the process of map creation
	choose = 0;
}

DLLCLBK void opcFocusChanged(OBJHANDLE newfocus, OBJHANDLE oldfocus)
{

}

DLLCLBK void opcPostStep(double SimT, double SimDT, double mjd)
{
	if (choose!=0)
	{
		shipptrs::backgroundaction();
		choose=0;
	}
	else
	{
		MFDFunction::donextaction();
		choose=1;
	}
}

bool SelectVariableFloat(void *id, char *str, void *usrdata) {
	return ((MFDvarfloat*)usrdata)->SetVariableFloat(str);
}

bool SelectVariableAngle(void *id, char *str, void *usrdata) { // siiiiigh
	return ((MFDvarangle*)usrdata)->SetVariableAngle(str);
}

bool SelectVariableBody(void *id, char *str, void *usrdata)
{
	return ((MFDvarmoon*)usrdata)->SetVariableBody(str);
}

void TextShow(oapi::Sketchpad *sketchpad,const char *label,int wpos,int hpos,OBJHANDLE handle)
{
	char buffer[30],buffer2[20];
	oapiGetObjectName(handle,buffer2,20);
	strcpy(buffer,label);
	strcat(buffer,buffer2);
	sketchpad->Text(wpos,hpos,buffer,strlen(buffer));
}

void TextForm(char *buffer,const char *label,double value)
{
	char index[2]=" ";
	if (fabs(value)>1000)
	{
		value/=1000;
		index[0]='k';
	}
	if (fabs(value)>1000)
	{
		value/=1000;
		index[0]='M';
	}
	if (fabs(value)>1000)
	{
		value/=1000;
		index[0]='G';
	}
	if (fabs(value)>1000)
	{
		value/=1000;
		index[0]='T';
	}
	strcpy(buffer,label);
	char buffer2[20]="";
	sprintf(buffer2,"%.4g",value);
	strcat(buffer2,index);
	strcat(buffer,buffer2);
}


void TextShow(oapi::Sketchpad *sketchpad,const char *label, int wpos, int hpos, double value)
{
	char buffer[30]="";
	TextForm(buffer,label,value);

	int length=strlen(buffer);
	sketchpad->Text( wpos, hpos, buffer, length);
}

// Find length of vector
double length2my(const VECTOR3 &v)
{
	return v.x*v.x + v.y*v.y + v.z*v.z;
}

double cosangle(const VECTOR3 &veca,const VECTOR3 &vecb)
{
	return dotp(veca,vecb)/sqrt(length2my(veca)*length2my(vecb));
}

void getinvrotmatrix(VECTOR3 arot, MATRIX3 *invrotmatrix)//arot not really a vector - see arot defn from vessel struct
{
	double tcos=cos(arot.z);
	double tsin=sin(arot.z);
	MATRIX3 z={0,0,0,0,0,0,0,0,1};
	z.m11=z.m22=tcos;
	z.m12=-tsin;
	z.m21=tsin;
	tcos=cos(arot.y);
	tsin=sin(arot.y);
	MATRIX3 y={0,0,0,0,1,0,0,0,0};
	y.m11=y.m33=tcos;
	y.m13=tsin;
	y.m31=-tsin;
	tcos=cos(arot.x);
	tsin=sin(arot.x);
	MATRIX3 x={1,0,0,0,0,0,0,0,0};
	x.m22=x.m33=tcos;
	x.m32=tsin;
	x.m23=-tsin;
	MATRIX3 temp = mul(z, y);
	*invrotmatrix = mul(temp, x);
}

double getdeterminant(const MATRIX3 mat)
{
	return   mat.m11 * (mat.m22 * mat.m33 - mat.m23 * mat.m32)
		   - mat.m12 * (mat.m21 * mat.m33 - mat.m23 * mat.m31)
		   + mat.m13 * (mat.m21 * mat.m32 - mat.m22 * mat.m31);
}

MATRIX3 getinvmatrix(const MATRIX3 mat)
{
	MATRIX3 out = {0,0,0,0,0,0,0,0,0};
	double det = getdeterminant(mat);
	if(det == 0)
		return out; // prevent devide by zero

	out.m11 =  (mat.m22 * mat.m33 - mat.m23 * mat.m32) / det;
	out.m21 = -(mat.m21 * mat.m33 - mat.m23 * mat.m31) / det;
	out.m31 =  (mat.m21 * mat.m32 - mat.m22 * mat.m31) / det;
	out.m12 = -(mat.m12 * mat.m33 - mat.m13 * mat.m32) / det;
	out.m22 =  (mat.m11 * mat.m33 - mat.m13 * mat.m31) / det;
	out.m32 = -(mat.m11 * mat.m32 - mat.m12 * mat.m31) / det;
	out.m13 =  (mat.m12 * mat.m23 - mat.m13 * mat.m22) / det;
	out.m23 = -(mat.m11 * mat.m23 - mat.m13 * mat.m21) / det;
	out.m33 =  (mat.m11 * mat.m22 - mat.m12 * mat.m21) / det;

	return out;
}

VECTOR3 GetRotationToTarget(VESSEL * vessel, const VECTOR3 & target)
{
    VECTOR3 trtarget;
	VESSELSTATUS status;
    vessel->GetStatus(status);
	VECTOR3 arot=status.arot;
	MATRIX3 rotmatrix;
	getinvrotmatrix(arot,&rotmatrix);
	trtarget = mul(rotmatrix, target);

	return trtarget;
}

void AddVesselToStack(VESSEL *vessel, vector<VESSEL*> &stack)
{
	// Is the vessel in the stack
	for(vector<VESSEL*>::iterator it = stack.begin(); it != stack.end(); it++)
		if(*it == vessel)
			return;	// return early as the vessel is already in the stack

	if(vessel)
	{
		stack.push_back(vessel);

		// Add all the docked vessels to the stack
		for(unsigned int i = 0; i < vessel->DockCount(); i++)
			if(OBJHANDLE dockedVessel = vessel->GetDockStatus(vessel->GetDockHandle(i)))
				AddVesselToStack(oapiGetVesselInterface(dockedVessel), stack);
	}
}

double GetStackMass(VESSEL *vessel)
{
	// Create a list with all the vessels in the stack contained in the list
	vector<VESSEL*> stack;
	AddVesselToStack(vessel, stack);

	// Get the total mass of all the vessels in the list.
	double stackMass = 0.0;
	for(vector<VESSEL*>::iterator it = stack.begin(); it != stack.end(); it++)
		stackMass += (*it)->GetMass();
	return stackMass;
}

double GetBurnTime(VESSEL *vessel, double deltaV)
{
	// Returns the time to burn to the required deltaV. Calculates via rocket equation
	if(deltaV < 0)
		deltaV = -deltaV;
    double T = 0, isp = 0;
	const int numThrusters = vessel->GetGroupThrusterCount(THGROUP_MAIN);
	for(int i = 0; i < numThrusters; ++i)
	{
		THRUSTER_HANDLE thruster = vessel->GetGroupThruster(THGROUP_MAIN,i);
		T += vessel->GetThrusterMax0(thruster);
		isp += vessel->GetThrusterIsp0(thruster);
	}
	return - (isp * GetStackMass(vessel) / T * (exp(-deltaV / isp) - 1.0));
}

double GetBurnTimeVariadic(VESSEL* vessel, THGROUP_TYPE thGroupType, double deltaV)
{
	double thrust = 0, isp = 0;
	const int numThrusters = vessel->GetGroupThrusterCount(thGroupType);
	for (int i = 0; i < numThrusters; ++i)
	{
		THRUSTER_HANDLE thruster = vessel->GetGroupThruster(thGroupType, i);
		thrust += vessel->GetThrusterMax0(thruster);
		isp += vessel->GetThrusterIsp0(thruster);
	}

	double mass = GetStackMass(vessel);
	double startAccel = thrust / mass;
	double mdot = thrust / isp;	// mass flow rate (rate of change of mass)
	// jerk is rate of change of acceleration - differentiate F/(M - dm * t) using chain rule
	// jerk is not constant, but assume it is and take the starting value (at t=0)
	double jerk = thrust * mdot / (mass * mass);
	double totalBurnTime = GetBurnTime(vessel, deltaV);

	// magic formula calculated from equations of motion of an object under non-uniform acceleration but uniform jerk
	double startBurn = (startAccel * totalBurnTime) / (2 * startAccel + jerk * totalBurnTime) - totalBurnTime;
	return startBurn;
}

double GetBurnStart(VESSEL *vessel, THGROUP_TYPE thGroupType, double instantaneousBurnTime, double deltaV)
{
	double startBurn = instantaneousBurnTime + GetBurnTimeVariadic(vessel, thGroupType, deltaV);
	return startBurn;
}
