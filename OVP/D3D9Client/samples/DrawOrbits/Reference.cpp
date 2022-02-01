
// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


#define WIN32_LEAN_AND_MEAN
#define NOSERVICE
#define NOMCX
#define NOIME
#define NOSOUND
#define NOKANJI
#define NOIMAGE
#define NOTAPE

#include <windows.h>
#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "Reference.h"
#include "Tools.h"
#include "VesselAPI.h"


class ReferenceClass *Refer;


ReferenceClass::ReferenceClass()
{
	total_count = oapiGetVesselCount() + oapiGetGbodyCount();

	References = (ReferenceClass_info *)malloc((total_count+16)*sizeof(ReferenceClass_info));

	memset((void *)References,0,(total_count+1)*sizeof(ReferenceClass_info));

	if (References) {
		StarHandle = FindStar();
		CreateDatabase();
	}
}



ReferenceClass::~ReferenceClass()
{

}



OBJHANDLE ReferenceClass::FindStar()
{

	double mas,mass=0;
	OBJHANDLE obj = NULL, star = NULL;
	int i,count=oapiGetGbodyCount();

	for (i=0;i<count;i++) {
		obj = oapiGetGbodyByIndex(i);
		mas = oapiGetMass(obj);
		if (mas>mass) mass=mas,star=obj;		
	}

	return(star);	
}



OBJHANDLE ReferenceClass::FindGravityReference(OBJHANDLE body)
{
	
	VECTOR3 pos,vel;
    OBJHANDLE obj, gr_ref=StarHandle;
	
	
	if (body==StarHandle) return(StarHandle);
	if (body==NULL) return(NULL);

	double mas=oapiGetMass(body);
    double distance,mass,force,gr_dist=10e20,gf=0;

	

	int i,count=oapiGetGbodyCount();

	for (i=0;i<count;i++) {

		obj = oapiGetGbodyByIndex(i);
		
		if (obj!=body && obj!=StarHandle) {
                
			mass = oapiGetMass(obj);

			if (mass>mas) {

				oapiGetRelativePos(obj,body,&pos);
				oapiGetRelativeVel(obj,body,&vel);

				// Compute Eccentricity
                double myy = mass * GC;
				double v = length(vel);
				double r = length(pos);
				double e = length( (  (pos * ((v*v)-(myy/r)))  -  (vel * dotp(pos,vel))  ) * (1/myy) );
			
                if (e<1) {
					distance=length(pos);				
					force=(GC*mass)/(distance*distance);				
					if (force>gf) gf=force, gr_ref=obj, gr_dist=distance;
				}
			}
		}
	}

	return(gr_ref);
}



void ReferenceClass::CreateDatabase()
{
	
	VECTOR3 pos;
    OBJHANDLE ref,obj;
    double distance;

	int i,j,gcount=oapiGetGbodyCount();
    int id=0;

	

	for (i=0;i<gcount;i++) {
		obj = oapiGetGbodyByIndex(i);
		ref = FindGravityReference(obj);
		
		References[id].handle = obj;
		References[id].grf_handle = ref;
        References[id].dist=0;	
        References[id].sys_index=0;	
		id++;
	}
   

    // NEW
	for (i=0;i<id;i++) {
		
		ref = References[i].handle; // Handle of the first object
	
		for (j=0;j<id;j++) {

			if (References[j].grf_handle==ref && References[j].handle!=ref) { // Anything orbiting this object

				oapiGetRelativePos(References[j].handle,ref,&pos); // Compute it's distance
				distance = length(pos) * 1.5;

				if (References[i].dist<distance) References[i].dist=distance;

				References[i].system[References[i].sys_index] = References[j].handle;
                References[i].sys_index++;

				if (References[i].sys_index>255) {
					return;
				}
			}
		}
	}  	


	int count=oapiGetVesselCount();

	for (i=0;i<count;i++) {

		obj = oapiGetVesselByIndex(i);
		ref = FindGravityReference(obj);
		
		References[id].handle = obj;
		References[id].grf_handle = ref;
		References[id].soi = 0;
        References[id].dist = 0;
		References[id].sys_index=0;
		id++;

		for (j=0;j<gcount;j++) {
			if (References[j].handle==ref) { 
				References[j].system[References[j].sys_index] = obj;
                References[j].sys_index++;

				if (References[j].sys_index>255) {
					return;
				}
			}
		}	
	}
}



bool ReferenceClass::IsGbody(OBJHANDLE x)
{
	int i;
	int count = oapiGetGbodyCount();
	for (i=0;i<count;i++) if (References[i].handle==x) return true;
	return false;
}



OBJHANDLE ReferenceClass::Get2ndReferenceForShip()
{
	VECTOR3 pos;
	VESSEL *ship=oapiGetFocusInterface();
	OBJHANDLE ref=ship->GetGravityRef();
	OBJHANDLE sec=ref;
	double min=0;
	int i,c=GetSystemCount(ref);
	
	if (c>0) {
		OBJHANDLE *list=GetSystemList(ref);
		for (i=0;i<c;i++) {
			OBJHANDLE bod=list[i];
			oapiGetRelativePos(ref, bod, &pos);
			double l=length(pos);
			double m=oapiGetMass(bod);

			double a=GC*m/(l*l);

			if (a>min) sec=bod, min=a;
		}
	}
	return sec;
}
	

// This function should find correct reference for
// Planets, Moons, Ships and Stations.

OBJHANDLE ReferenceClass::GetReference(OBJHANDLE x)
{
	int i;
	VECTOR3 distance;
	OBJHANDLE nref,ref=NULL;
   
	if (x==StarHandle)	return(StarHandle);
	if (x==NULL)		return(StarHandle);

	VESSEL *ship=oapiGetVesselInterface(x);


	if (ship==NULL) 
	for (i=0;i<total_count;i++) if (References[i].handle==x) {
		return(References[i].grf_handle);
	}
    

    // ReferenceClass For Vessel
	
	if (ship) ref=ship->GetGravityRef();

    if (ref!=StarHandle) return(ref);

	for (i=0;i<total_count;i++) {
		if (References[i].dist>0 && References[i].handle!=StarHandle) { // does this ReferenceClass have an distance setting
			nref=References[i].handle;
			oapiGetRelativePos(x,nref,&distance);
			if (length(distance)<References[i].dist) return(nref);
		}
	}
		
    return(StarHandle);	
}



// This function should find correct reference for
// Planets, Moons, Ships and Stations.

OBJHANDLE *ReferenceClass::GetSystemList(OBJHANDLE x)
{
	int i,gcount;

	gcount = oapiGetGbodyCount();

	for (i=0;i<gcount;i++) if (References[i].handle==x) {
		return(References[i].system);
	}
		
    return(NULL);	
}



int  ReferenceClass::GetSystemCount(OBJHANDLE x)
{
	int i,gcount;

	gcount = oapiGetGbodyCount();

	for (i=0;i<gcount;i++) if (References[i].handle==x) {
		return(References[i].sys_index);
	}
		
    return(0);	
}




OBJHANDLE ReferenceClass::GetReferenceByName(char *name)
{
	OBJHANDLE obj=oapiGetObjectByName(name);
	return(GetReference(obj));
}




double ReferenceClass::GetSOI(OBJHANDLE obj)
{
    if (obj==NULL) return(0);

    OBJHANDLE r=Refer->GetReference(obj);

	if (r!=obj) {
		return CalculateSOI(obj,r);
	}

	return(oapiGetSize(obj));

}
