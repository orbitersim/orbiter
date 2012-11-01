// ==============================================================
// RunwayLights.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP) D3D9 Client
// Released under GNU General Public License
// Copyright (C) 2012 Émile "Bibi Uncle" Grégoire
//				 2012 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class RunwayLights
// Defines runway lights used in vBase.
// ==============================================================

#include "RunwayLights.h"
#include "Scene.h"
#include "BeaconArray.h"
#include "D3D9Config.h"
#include "D3D9Util.h"
#include <vector>

RunwayLights::RunwayLights(OBJHANDLE handle)
{
	end1 = _V(0, 0, 0);
	end2 = _V(0, 0, 0);
	width = 50.0;
	td_disp = 0.0;
	td_length = 600.0;
	apr_start = 900.0;
	apr_length = 257.0;
	nPAPI = 0;
	hObj  = handle;
	bVASI = false;
	bSingleEnded = false;

	for (int i=0;i<6;i++) papi_f[i]=papi_b[i]=NULL;

	vasi1 = vasi2 = NULL;
	beacons1 = beacons2 = NULL;
	currentTime = 0.0f;
}

RunwayLights::~RunwayLights()
{
	SAFE_DELETE(beacons1);
	SAFE_DELETE(beacons2);
	SAFE_DELETE(vasi1);
	SAFE_DELETE(vasi2);
	for (int i=0;i<6;i++) {
		SAFE_DELETE(papi_f[i]);
		SAFE_DELETE(papi_b[i]);
	}
}

void RunwayLights::SetSignleEnded(bool bEnd)
{
	bSingleEnded = bEnd;
}

void RunwayLights::SetEnd1(VECTOR3 pos)
{
	end1 = pos;
}

void RunwayLights::SetEnd2(VECTOR3 pos)
{
	end2 = pos;
}

void RunwayLights::SetWidth(double w)
{
	width = w;
}

void RunwayLights::SetTouchZoneDisplacement(double disp)
{
	td_disp = disp;
}

void RunwayLights::SetTouchZoneLength(double disp)
{
	td_length = disp;
}

void RunwayLights::SetDecisionDist(double dist)
{
	apr_length = dist;
}

void RunwayLights::SetApproachStart(double dist)
{
	apr_start = dist;
}

void RunwayLights::AddPAPI(VECTOR3 pos, float disp)
{
	if (nPAPI>5) return;
	PAPI_pos[nPAPI] = pos;
	PAPI_disp[nPAPI] = disp;
	nPAPI++;
}

void RunwayLights::AddVASI(VECTOR3 pos)
{
	bVASI = true;
	VASI = pos;
}


void RunwayLights::Init()
{
	_TRACE;
	
	beacons1 = BuildLights(end1, end2);
	if (!bSingleEnded) beacons2 = BuildLights(end2, end1);

	if (bVASI) {
		vasi1 = BuildVASI(end1, end2);
		if (!bSingleEnded) vasi2 = BuildVASI(end2, end1);
	}

	for (DWORD i=0;i<nPAPI;i++) {
		papi_f[i] = BuildPAPI(end1, end2, i);
		if (!bSingleEnded) papi_b[i] = BuildPAPI(end2, end1, i);
	}
}



BeaconArray *RunwayLights::BuildLights(VECTOR3 _start, VECTOR3 _end)
{
	_TRACE;
	const float lightSize = 4.0f;
	const float upAngle = 12.0f;

	// Helping vectors
	VECTOR3 _direction = _end - _start; // Vector of the runway
	VECTOR3 _dir = _direction; // Normalized direction
	normalise(_dir);
	VECTOR3 _td_disp = _dir * td_disp; // Touch zone displacement vector
	
	_start += _td_disp;
	_direction = _end - _start;

	double len = length(_direction); // Length of the runway
	double spacing = apr_length/10.0;

	// Number of lights
	int numLightsEdge = (int)(len/30.0)+2;
	int numLightsEnd = (int)(width/3.0)+1; // end lights (3m of spacing)
	int numLightsTouch = (int)(td_length/spacing); 
	int numLightsDecision = 10;
	int numLightsApproach = (int)((apr_start-apr_length)/spacing);
	int count = 0;

	if (numLightsApproach<0) numLightsApproach = 0;

	
	double limit = 59.0;

	// Not a critical. Must be higher than actual number of lights. Only used for memory allocation
	numLights = 2 * (numLightsEdge*4 + numLightsEnd*3 + numLightsTouch*3*2 + numLightsDecision*3*2 + numLightsApproach*5); // total lights
	
	float lightAngle = float(Config->RwyLightAngle);
	float brightness = float(Config->RwyBrightness);

	// Main lights vectors
	VECTOR3 _space; // Vector between each light
	VECTOR3 _current; // Incremented in the for loop
	VECTOR3 _shift;
	VECTOR3 _widthDir = crossp(_direction, _V(0, 1, 0)); // used to calculate the edge lights
	normalise(_widthDir);

	BeaconArrayEntry* beaconsEntry1 = new BeaconArrayEntry[numLights];
	
	int i=0, k=0;

	DWORD red    = 0xFFFF4444;
	DWORD green  = 0xFF00FF88;
	DWORD white  = 0xFFFFEECC; 
	DWORD yellow = 0xFFFFBB33; 

	BeaconArrayEntry edgeLight, centerLight, endLight, beaconLight, papiLight;

	centerLight.angle = lightAngle;
	centerLight.size = lightSize;
	centerLight.lon = 0.0f;
	centerLight.loff = 1.0f;
	centerLight.bright = 1.5f * brightness;
	centerLight.fall = 0.5;
	centerLight.dir = _dir*cos(upAngle*RAD) + _V(0, 1, 0)*sin(upAngle*RAD);

	endLight  = centerLight;
	edgeLight = centerLight;
	edgeLight.size = lightSize*1.0f;

	beaconLight = centerLight;
	beaconLight.size = lightSize*2.0f;
	beaconLight.angle = min(180.0f, lightAngle * 4.0f);
	beaconLight.bright = 4.0f * brightness;

	papiLight = centerLight;
	papiLight.size = lightSize*3.0f;
	papiLight.angle = min(180.0f, lightAngle * 4.0f);
	papiLight.bright = 4.0f * brightness;


	// end lights --------------------------------------

	_space = _widthDir * width/(numLightsEnd-1);
	_current = _end - _space*((numLightsEnd-1)/2);

	for(k=0;k<numLightsEnd;k++,i++)
	{
		beaconsEntry1[i] = endLight;
		beaconsEntry1[i].pos = _current;
		beaconsEntry1[i].color = red;
		_current += _space;
	}

	// main lights --------------------------------------

	_space = _direction * 1 / (numLightsEdge-1); // Vector between each light
	_current = _end; // Incremented in the for loop

	for(k=0; k<(numLightsEdge-1); k++,i+=4)
	{
		VECTOR3 _distanceCalc = _current - _start;
		double distanceFront = length(_distanceCalc);
		_distanceCalc = _current - _end;
		double distanceBack = length(_distanceCalc);

		beaconsEntry1[i+0] = edgeLight;
		beaconsEntry1[i+1] = edgeLight;
		beaconsEntry1[i+2] = centerLight;
		beaconsEntry1[i+3] = centerLight;

		beaconsEntry1[i+0].pos = _current+_widthDir*width/2; // right
		beaconsEntry1[i+1].pos = _current-_widthDir*width/2; // left
		beaconsEntry1[i+2].pos = _current; // center
		beaconsEntry1[i+3].pos = _current-_space/2; // center

		// Border color
		if(distanceBack>610) beaconsEntry1[i].color = beaconsEntry1[i+1].color = white;
		else				 beaconsEntry1[i].color = beaconsEntry1[i+1].color = yellow;

		// Center color
		if(distanceBack<305)
		{
			beaconsEntry1[i+2].color = red;
			beaconsEntry1[i+3].color = red;
		}

		else if (distanceBack>305 && distanceBack<914)
		{
				beaconsEntry1[i+2].color = white; 
				beaconsEntry1[i+3].color = red;
		}

		else
		{
			beaconsEntry1[i+2].color = white;
			beaconsEntry1[i+3].color = white;
		}

		_current -= _space;
	}

	// touch zone lights ---------------------------------

	_space = _dir * spacing;
	_current = _start + _space * double(numLightsTouch);

	if (width>limit) _shift = _widthDir * 17.0;
	else             _shift = _widthDir * floor(width/3.5);

	for(k=0; k<(numLightsTouch*6); k+=6,i+=6)
	{
		beaconsEntry1[i] = centerLight;
		beaconsEntry1[i].color = white;
		beaconsEntry1[i+5] = beaconsEntry1[i+4] = beaconsEntry1[i+3] = beaconsEntry1[i+2] = beaconsEntry1[i+1] = beaconsEntry1[i];

		beaconsEntry1[i+0].pos = _current + _shift;
		beaconsEntry1[i+1].pos = _current + _shift - (_widthDir*2.0);
		beaconsEntry1[i+2].pos = _current + _shift - (_widthDir*4.0);
		beaconsEntry1[i+3].pos = _current - _shift;
		beaconsEntry1[i+4].pos = _current - _shift + (_widthDir*2.0);
		beaconsEntry1[i+5].pos = _current - _shift + (_widthDir*4.0);
		_current -= _space;
	}

	// start lights --------------------------------------

	_space = _widthDir * width/float(numLightsEnd-1);

	if (width>limit) {
		_current = _start - _space*(float(numLightsEnd-1)/2.0) - _space * 3;
		count = numLightsEnd+6;
	}
	else {
		_current = _start - _space*(float(numLightsEnd-1)/2.0);
		count = numLightsEnd;
	}

	for(k=0; k<count; k++, i++)
	{
		beaconsEntry1[i] = endLight;
		beaconsEntry1[i].pos = _current;
		beaconsEntry1[i].color = green;
		_current += _space;
	}


	if (width>limit) {

		// pre-touch zone lights with red -------------------------------

		_space = _dir * spacing;
		_current = _start - _space;

		for (k=0; k<9; k++, i+=11)
		{
			beaconsEntry1[i] = centerLight;
			beaconsEntry1[i].color = red;
			
			for (int e=1;e<11;e++) beaconsEntry1[i+e] = beaconsEntry1[i];
			for (int e=3;e<8;e++)  beaconsEntry1[i+e].color = white;

			beaconsEntry1[i+0].pos = _current + _shift;
			beaconsEntry1[i+1].pos = _current + _shift - _widthDir*2.0;
			beaconsEntry1[i+2].pos = _current + _shift - _widthDir*4.0;

			beaconsEntry1[i+3].pos = _current;
			beaconsEntry1[i+4].pos = _current + _widthDir;
			beaconsEntry1[i+5].pos = _current + _widthDir*2.0;
			beaconsEntry1[i+6].pos = _current - _widthDir;
			beaconsEntry1[i+7].pos = _current - _widthDir*2.0;

			beaconsEntry1[i+8].pos = _current - _shift;
			beaconsEntry1[i+9].pos = _current - _shift + _widthDir*2.0;
			beaconsEntry1[i+10].pos = _current - _shift + _widthDir*4.0;
			_current -= _space;
		}

		// cross --------------------------------------------------------------

		for(k=0; k<8; k++, i+=2)
		{
			beaconsEntry1[i] = centerLight;
			beaconsEntry1[i+1] = centerLight;
			beaconsEntry1[i].color = white;
			beaconsEntry1[i+1].color = white;
			beaconsEntry1[i].pos = _current + _space * 5 + _widthDir * (3+k);
			beaconsEntry1[i+1].pos = _current + _space * 5 - _widthDir * (3+k);
		}
	}

	else {

		// pre-touch zone lights without red -------------------------------

		_space = _dir * spacing;
		_current = _start - _space;

		for (k=0; k<9; k++, i+=11)
		{
			beaconsEntry1[i] = centerLight;
			beaconsEntry1[i].color = white;
			
			for (int e=1;e<5;e++) beaconsEntry1[i+e] = beaconsEntry1[i];
			
			beaconsEntry1[i+0].pos = _current;
			beaconsEntry1[i+1].pos = _current + _widthDir;
			beaconsEntry1[i+2].pos = _current + _widthDir*2.0;
			beaconsEntry1[i+3].pos = _current - _widthDir;
			beaconsEntry1[i+4].pos = _current - _widthDir*2.0;

			_current -= _space;
		}
	}


	// white line ---------------------------------------------------------

	VECTOR3 _pos = _current - _shift;
	count = (int)(length(_shift)/2.0)*2 + 3;
	_space = (_shift*2)/count;

	for(k=0; k<count+1; k++, i++)
	{
		beaconsEntry1[i] = centerLight;
		beaconsEntry1[i].pos = _pos;
		beaconsEntry1[i].color = white;
		_pos += _space;
	}

	

	// approach lights -------------------------------------

	_space = _dir*spacing;
	_current -= _space;

	if (width>limit) count = numLightsApproach;
	else             count = 2;

	for(k=0; k<count; k++, i+=5)
	{
		beaconsEntry1[i] = centerLight;
		beaconsEntry1[i].color = white;
		beaconsEntry1[i+4] = beaconsEntry1[i+3] = beaconsEntry1[i+2] = beaconsEntry1[i+1] = beaconsEntry1[i];

		beaconsEntry1[i+0].pos = _current;
		beaconsEntry1[i+1].pos = _current + _widthDir;
		beaconsEntry1[i+2].pos = _current + _widthDir*2.0;
		beaconsEntry1[i+3].pos = _current - _widthDir;
		beaconsEntry1[i+4].pos = _current - _widthDir*2.0;

		_current -= _space;
	}

	// animated lights -------------------------------------------- 

	if (width<=limit) {

		_space = _dir*100.0;
		_current -= _space * 0.5;
		
		for(k=0; k<5; k++, i++)
		{
			beaconsEntry1[i] = beaconLight;

			if (Config->RwyLightAnimate) {
				beaconsEntry1[i].lon = (1.0f-float(k)*0.2f)-0.2f;
				beaconsEntry1[i].loff = (1.0f-float(k)*0.2f);
			}
			else {
				beaconsEntry1[i].lon = 0.0f;
				beaconsEntry1[i].loff = 0.0f;
			}

			beaconsEntry1[i].color = white;
			beaconsEntry1[i].pos = _current;
		
			_current -= _space;
		}
	}
	

	// Snap to ground

	for (int k=0;k<i;k++) beaconsEntry1[k].pos.y = 0;
	


	// VASI ---------------------------------------------------------
	
	if (bVASI) {
		BeaconArrayEntry vasiLight;

		vasiLight.angle = min(180.0f, float(Config->RwyLightAngle) * 2.0f);
		vasiLight.size = 1.5f * lightSize;
		vasiLight.lon = 0.0f;
		vasiLight.loff = 1.0f;
		vasiLight.bright = 3.0f * float(Config->RwyBrightness);
		vasiLight.fall = 0.1f;
		vasiLight.dir = _dir*cos(upAngle*RAD) + _V(0, 1, 0)*sin(upAngle*RAD);

		_current = _start + _dir * VASI.z + _widthDir * (width/2.0 + 30.0);
		_current.y = 0;

		for (k=0;k<20;k++, i++) {
			beaconsEntry1[i] = vasiLight;
			beaconsEntry1[i].color = red;
			beaconsEntry1[i].size = 1.0f * lightSize;
			beaconsEntry1[i].pos = _current + _widthDir * 2.0 * float(k) + _V(0,1,0);
		}

		_current -= _dir * VASI.y;

		for (k=0;k<5;k++, i++) {
			beaconsEntry1[i] = vasiLight;
			beaconsEntry1[i].color = white;
			beaconsEntry1[i].pos = _current + _widthDir * 2.0 * float(k) + _V(0,1,0) + _V(0,1,0)*(sin(VASI.x*RAD)*VASI.y);
		}
	}


	// Post process lights ------------------------------------------

	OBJHANDLE hPlanet = oapiGetBasePlanet(hObj);
	double size = oapiGetSize(hPlanet);

	for (int k=0;k<i;k++) {
		beaconsEntry1[k].dir = _V(-beaconsEntry1[k].dir.x, beaconsEntry1[k].dir.y, -beaconsEntry1[k].dir.z);
		double dst = length(beaconsEntry1[k].pos);
		double dif = sqrt(dst*dst + size*size) - size;
		beaconsEntry1[k].pos.y -= (dif-0.2);
	}

	BeaconArray *beacons = new BeaconArray(beaconsEntry1, i);
	delete[] beaconsEntry1;
	return beacons;
}


BeaconArray * RunwayLights::BuildPAPI(VECTOR3 start, VECTOR3 end, int i)
{

	// PAPI lights
	// Helping vectors
	VECTOR3 direction = end - start; // Vector of the runway
	VECTOR3 dir = unit(direction); // Normalized direction
	VECTOR3 widthDir = unit(crossp(direction, _V(0, 1, 0))); // used to calculate the edge lights
	
	BeaconArrayEntry papiLight;

	const float lightSize = 4.0f;
	const float upAngle = 7.5f;
	const float papi_separation = 6.0f;

	float disp = PAPI_disp[i] * float(width);
	
	papiLight.angle = min(180.0f, float(Config->RwyLightAngle) * 2.0f);
	papiLight.size = 3.0f * lightSize;
	papiLight.lon = 0.0f;
	papiLight.loff = 1.0f;
	papiLight.bright = 4.0f * float(Config->RwyBrightness);
	papiLight.fall = 0.5;
	papiLight.dir = dir*cos(upAngle*RAD) + _V(0, 1, 0)*sin(upAngle*RAD);

	BeaconArrayEntry entryPAPI[4];

	for(int j=0; j<4; j++)
	{
		entryPAPI[j] = papiLight;
		entryPAPI[j].dir = _V(-entryPAPI[j].dir.x, entryPAPI[j].dir.y, -entryPAPI[j].dir.z);
		entryPAPI[j].pos = start + dir*PAPI_pos[i].z + widthDir*disp + widthDir*j*papi_separation - widthDir*papi_separation*1.5;
	}

	BeaconArray *beacons = new BeaconArray(entryPAPI, 4);
	return beacons;
}


BeaconArray *RunwayLights::BuildVASI(VECTOR3 _start, VECTOR3 _end)
{
	_TRACE;
	const float lightSize = 4.0f;
	const float upAngle = 12.0f;

	// Helping vectors
	VECTOR3 _direction = _end - _start; // Vector of the runway
	VECTOR3 _dir = _direction; // Normalized direction
	normalise(_dir);
	VECTOR3 _td_disp = _dir * td_disp; // Touch zone displacement vector
	
	_start += _td_disp;
	_direction = _end - _start;

	double len = length(_direction); // Length of the runway
	double limit = 59.0;
	
	float lightAngle = float(Config->RwyLightAngle);
	float brightness = float(Config->RwyBrightness);

	// Main lights vectors
	VECTOR3 _current; // Incremented in the for loop
	VECTOR3 _widthDir = crossp(_direction, _V(0, 1, 0)); // used to calculate the edge lights
	normalise(_widthDir);

	BeaconArrayEntry* beaconsEntry1 = new BeaconArrayEntry[30];
	
	int i=0, k=0;

	DWORD red    = 0xFFFF4444;
	DWORD white  = 0xFFFFEECC; 
	
	BeaconArrayEntry vasiLight;

	vasiLight.angle = min(180.0f, float(Config->RwyLightAngle) * 2.0f);
	vasiLight.size = 1.5f * lightSize;
	vasiLight.lon = 0.0f;
	vasiLight.loff = 1.0f;
	vasiLight.bright = 3.0f * float(Config->RwyBrightness);
	vasiLight.fall = 0.1f;
	vasiLight.dir = _dir*cos(upAngle*RAD) + _V(0, 1, 0)*sin(upAngle*RAD);

	_current = _start + _dir * VASI.z + _widthDir * (width/2.0 + 30.0);
	_current.y = 0;

	for (k=0;k<20;k++, i++) {
		beaconsEntry1[i] = vasiLight;
		beaconsEntry1[i].color = red;
		beaconsEntry1[i].size = 1.0f * lightSize;
		beaconsEntry1[i].pos = _current + _widthDir * 2.0 * float(k) + _V(0,1,0);
	}

	_current -= _dir * VASI.y;

	for (k=0;k<5;k++, i++) {
		beaconsEntry1[i] = vasiLight;
		beaconsEntry1[i].color = white;
		beaconsEntry1[i].pos = _current + _widthDir * 2.0 * float(k) + _V(0,1,0) + _V(0,1,0)*(sin(VASI.x*RAD)*VASI.y);
	}
	
	// Post process lights ------------------------------------------

	OBJHANDLE hPlanet = oapiGetBasePlanet(hObj);
	double size = oapiGetSize(hPlanet);

	for (int k=0;k<i;k++) {
		beaconsEntry1[k].dir = _V(-beaconsEntry1[k].dir.x, beaconsEntry1[k].dir.y, -beaconsEntry1[k].dir.z);
		double dst = length(beaconsEntry1[k].pos);
		double dif = sqrt(dst*dst + size*size) - size;
		beaconsEntry1[k].pos.y -= (dif-0.2);
	}

	BeaconArray *beacons = new BeaconArray(beaconsEntry1, i);
	delete[] beaconsEntry1;
	return beacons;
}
	





void RunwayLights::SetPAPIColors(BeaconArray *pPAPI, LPD3DXMATRIX world, int i)
{

	BAVERTEX *pVrt = pPAPI->LockVertexBuffer();

	if (pVrt) {

		DWORD red    = 0xFFFF4444;
		DWORD white  = 0xFFFFEECC; 

		D3DXVECTOR3 vPos, vUp, vFront;
		D3DXVec3TransformNormal(&vUp, &D3DXVECTOR3(0,1,0), world);
		D3DXVECTOR3 vRef1(pVrt[0].x, pVrt[0].y, pVrt[0].z);
		D3DXVec3Normalize(&vFront, D3DXVec3TransformCoord(&vPos, &vRef1, world));
	
		float slope = float(-asin(D3DXVec3Dot(&vFront,&vUp))*DEG);

		VECTOR3 P = PAPI_pos[i];

		if (PAPI_disp[i]<0) {
			if (slope<float(P.x-P.y*1.0)) pVrt[0].color = red;
			else						  pVrt[0].color = white;
			if (slope<float(P.x-P.y*0.5)) pVrt[1].color = red;
			else						  pVrt[1].color = white;
			if (slope<float(P.x+P.y*0.0)) pVrt[2].color = red;
			else						  pVrt[2].color = white;
			if (slope<float(P.x+P.y*0.5)) pVrt[3].color = red;
			else						  pVrt[3].color = white;
		}
		else {
			if (slope<float(P.x-P.y*1.0)) pVrt[3].color = red;
			else						  pVrt[3].color = white;
			if (slope<float(P.x-P.y*0.5)) pVrt[2].color = red;
			else						  pVrt[2].color = white;
			if (slope<float(P.x+P.y*0.0)) pVrt[1].color = red;
			else						  pVrt[1].color = white;
			if (slope<float(P.x+P.y*0.5)) pVrt[0].color = red;
			else						  pVrt[0].color = white;
		}
		
		pPAPI->UnLockVertexBuffer();
	}
}



void RunwayLights::Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night)
{
	_TRACE;
	currentTime = float(fmod(1.7*oapiGetSimTime(), 1.0));
	if (currentTime<0) currentTime = 1.0f + currentTime;
	
	if (Config->RwyLightAnimate==0) currentTime = 0.5f;

	VECTOR3 dir = unit(end2 - end1); // Vector of the runway
	VECTOR3 camDir;
	D3DXVECTOR3 dirGlo;
	oapiCameraGlobalDir(&camDir);

	D3DXVec3TransformNormal(&dirGlo, &D3DXVEC(dir), world);
	
	if (D3DXVec3Dot(&D3DXVEC(camDir), &dirGlo) > 0)
	{
		if (night) beacons1->Render(dev, world, currentTime);
		if (bVASI) vasi1->Render(dev, world);

		for (DWORD i=0;i<nPAPI;i++) {
			SetPAPIColors(papi_f[i], world, i);
			papi_f[i]->Render(dev, world);
		}
	}
	else if (!bSingleEnded)
	{
		if (night) beacons2->Render(dev, world, currentTime);
		if (bVASI) vasi2->Render(dev, world);
		for (DWORD i=0;i<nPAPI;i++) {
			SetPAPIColors(papi_b[i], world, i);
			papi_b[i]->Render(dev, world);
		}
	}
}


int RunwayLights::CreateRunwayLights(OBJHANDLE base, const char *filename, RunwayLights**& out)
{
	int numRunwayLights = 0;
	std::vector<RunwayLights*> lights;
	char cbuf[256];
	
	FILE* file = fopen(filename, "r");

	if (file == NULL) {
		LogErr("Could not open %s file.", filename);
		return 0;
	}

	LogAlw("Creating Runway Lights from %s",filename);

	while(fgets2(cbuf, 256, file)>=0)
	{
		if(!strncmp(cbuf, "RUNWAYLIGHTS", 12))
		{
			numRunwayLights++;
			lights.push_back(new RunwayLights(base));
			
			for(;;)
			{
				if (fgets2(cbuf, 256, file)<0) break;

				if(!strncmp(cbuf, "END1", 4))
				{
					VECTOR3 vec;
					sscanf(cbuf, "END1 %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numRunwayLights-1]->SetEnd1(vec);
				}

				else if(!strncmp(cbuf, "END2", 4))
				{
					VECTOR3 vec;
					sscanf(cbuf, "END2 %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numRunwayLights-1]->SetEnd2(vec);
				}

				else if(!strncmp(cbuf, "WIDTH", 5))
				{
					double width;
					sscanf(cbuf, "WIDTH %lf", &width);
					lights[numRunwayLights-1]->SetWidth(width);
				}

				else if(!strncmp(cbuf, "TD_DISP", 7))
				{
					double disp;
					sscanf(cbuf, "TD_DISP %lf", &disp);
					lights[numRunwayLights-1]->SetTouchZoneDisplacement(disp);
				}

				else if(!strncmp(cbuf, "TD_LENGTH", 9))
				{
					double disp;
					sscanf(cbuf, "TD_LENGTH %lf", &disp);
					lights[numRunwayLights-1]->SetTouchZoneLength(disp);
				}

				else if(!strncmp(cbuf, "DECISION_DIST", 13))
				{
					double disp;
					sscanf(cbuf, "DECISION_DIST %lf", &disp);
					lights[numRunwayLights-1]->SetDecisionDist(disp);
				}

				else if(!strncmp(cbuf, "APPROACH_START", 14))
				{
					double disp;
					sscanf(cbuf, "DECISION_DIST %lf", &disp);
					lights[numRunwayLights-1]->SetApproachStart(disp);
				}

				else if(!strncmp(cbuf, "PAPI", 4))
				{
					VECTOR3 vec; DWORD u;
				
					int n = sscanf(cbuf, "PAPI %lf %lf %lf %u", &vec.x, &vec.y, &vec.z, &u);

					if (n==3) {
						lights[numRunwayLights-1]->AddPAPI(vec,  1);
						lights[numRunwayLights-1]->AddPAPI(vec, -1);
					}
					else if (n==4) {
						if (!u)  lights[numRunwayLights-1]->AddPAPI(vec,  0); 
						if (u&1) lights[numRunwayLights-1]->AddPAPI(vec,  1);
						if (u&2) lights[numRunwayLights-1]->AddPAPI(vec, -1);
					}
					else  LogErr("RUNWAYLIGHTS: Invalid parameter count in PAPI entry in (%s)",filename);
				}

				else if(!strncmp(cbuf, "VASI", 4))
				{
					VECTOR3 vec;
					sscanf(cbuf, "VASI %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numRunwayLights-1]->AddVASI(vec);
				}

				else if(!strncmp(cbuf, "SINGLEENDED", 11))
				{
					lights[numRunwayLights-1]->SetSignleEnded(true);
				}

				else if(!strncmp(cbuf, "END", 3))
				{
					break;
				}
			}
		}
	}

	fclose(file);

	out = new RunwayLights*[numRunwayLights];

	int i;
	for(i=0; i<numRunwayLights; i++)
	{
		out[i] = lights[i];
		out[i]->Init();
	}

	return numRunwayLights;
}



// ==============================================================
// class TaxiLights
// Defines runway lights used in vBase.
// ==============================================================


TaxiLights::TaxiLights(OBJHANDLE handle)
{
	end1  = _V(0, 0, 0);
	end2  = _V(0, 0, 0);
	color = _V(1, 1, 1);
	size  = 1.0;
	count = 10;
	hObj  = handle;
	beacons1 = NULL;
	currentTime = 0.0f;
}

TaxiLights::~TaxiLights()
{
	SAFE_DELETE(beacons1);

}

void TaxiLights::SetEnd1(VECTOR3 pos)
{
	end1 = pos;
}

void TaxiLights::SetEnd2(VECTOR3 pos)
{
	end2 = pos;
}

void TaxiLights::SetSize(double s)
{
	size = s;
}

void TaxiLights::SetCount(int c)
{
	count = c;
}

void TaxiLights::SetColor(VECTOR3 clr)
{
	color = clr;
}


void TaxiLights::Init()
{
	_TRACE;
	// Helping vectors
	VECTOR3 direction = end2 - end1; // Vector of the runway
	VECTOR3 dir = direction; // Normalized direction
	normalise(dir);
	double len = length(direction); // Length of the runway

	BeaconArrayEntry* beaconsEntry1 = new BeaconArrayEntry[count];
	BeaconArrayEntry taxiLight;

	taxiLight.angle = 360.0f;
	taxiLight.size = float(9.0*size);
	taxiLight.lon = 0.0f;
	taxiLight.loff = 1.0f;
	taxiLight.bright = 5.0f;
	taxiLight.fall = 0.5;
	taxiLight.dir = _V(0, 1, 0);
	taxiLight.color = D3DXCOLOR(float(color.x), float(color.y), float(color.z), 1.0f);

	VECTOR3 space = dir * len / (count-1);
	VECTOR3 current = end1;

	for (int i=0;i<count;i++)
	{
		beaconsEntry1[i] = taxiLight;
		beaconsEntry1[i].pos = current;
		current += space;
	} 

	beacons1 = new BeaconArray(beaconsEntry1, count);
	delete[] beaconsEntry1;
}


void TaxiLights::Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night)
{
	if (night) beacons1->Render(dev, world, 0.5f);
}



int TaxiLights::CreateTaxiLights(OBJHANDLE base, const char *filename, TaxiLights**& out)
{
	int numTaxiLights = 0;
	std::vector<TaxiLights*> lights;
	char cbuf[256];
	
	FILE* file = fopen(filename, "r");

	if (file == NULL) {
		LogErr("Could not open %s file.", filename);
		return 0;
	}

	while(fgets2(cbuf, 256, file)>=0)
	{
		if(!strncmp(cbuf, "BEACONARRAY", 11))
		{
			numTaxiLights++;
			lights.push_back(new TaxiLights(base));
			
			for(;;)
			{
				if (fgets2(cbuf, 256, file)<0) break;

				if(!strncmp(cbuf, "END1", 4))
				{
					VECTOR3 vec;
					sscanf(cbuf, "END1 %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numTaxiLights-1]->SetEnd1(vec);
				}

				else if(!strncmp(cbuf, "END2", 4))
				{
					VECTOR3 vec;
					sscanf(cbuf, "END2 %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numTaxiLights-1]->SetEnd2(vec);
				}

				else if(!strncmp(cbuf, "COL", 3))
				{
					VECTOR3 vec;
					sscanf(cbuf, "COL %lf %lf %lf", &vec.x, &vec.y, &vec.z);
					lights[numTaxiLights-1]->SetColor(vec);
				}

				else if(!strncmp(cbuf, "SIZE", 4))
				{
					double size;
					sscanf(cbuf, "SIZE %lf", &size);
					lights[numTaxiLights-1]->SetSize(size);
				}

				else if(!strncmp(cbuf, "COUNT", 5))
				{
					int count;
					sscanf(cbuf, "COUNT %d", &count);
					lights[numTaxiLights-1]->SetCount(count);
				}

				else if(!strncmp(cbuf, "END", 3))
				{
					break;
				}
			}
		}
	}

	fclose(file);

	out = new TaxiLights*[numTaxiLights];

	int i;
	for(i=0; i<numTaxiLights; i++)
	{
		out[i] = lights[i];
		out[i]->Init();
	}

	return numTaxiLights;
}