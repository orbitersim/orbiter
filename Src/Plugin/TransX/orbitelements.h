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


#pragma once

#include <OrbiterSDK.h>


class OrbitElements{
private:
	VECTOR3 planevector; // Vector perpendicular to plane - defines orbital plane
	double angularmomentum2; // Angular momentum squared
	double semimajor; // Length of semi-major axis - NEGATIVE if elliptical, POSITIVE if hyperbolic
	VECTOR3 eccvector; // Eccentricity vector
	double eccentricity; // eccentricity of orbit
	double currcosthi; // cosine of angle of craft from periapsis
	double currsinthi; //sin of angle of craft from periapsis
	double timestamp; //Simtime when orbit snapshot taken
	VECTOR3 majoraxis; // Unit vector along major axis towards periapsis
	VECTOR3 minoraxis; // Unit vector along minor axis towards outward side of orbit
	VECTOR3 currposition; // Current position on orbit
	VECTOR3 currvelocity; // Current velocity on orbit
	double planet; // GM for planet
	double deltatime;  // Time from current position to Periapsis
	double orbitconstant; //Time for one radian of mean anomaly
	bool valid; //Whether the orbit class is valid or not
	double simpletimetoradius(double radius) const;
	double simpletimetothi(double costhi, double sinthi) const;//private time calculation function
	OrbitElements *minoraboutbarycentre;
public:
	void improvebyradius(double timetarget,double topthi,double timeattopthi,class OrbitTime *posvel) const;
	void improvebysubdivision(double timetarget,double topthi,double timeattopthi,class OrbitTime *posvel) const;
	bool improve(double timetarget,class OrbitTime *posvel) const;
	void release();
public:
	virtual ~OrbitElements();
	OrbitElements(); // Default constructor
	void gettimeorbit(int *orbitnumber,double *orbittime, double timefromnow) const;
	OrbitElements(OBJHANDLE hmajor, OBJHANDLE hminor); //Constructor
	OrbitElements(VECTOR3 rposition, VECTOR3 rvelocity, double gmplanet); //Constructor using pos and vel vectors
	double getvelocityatdist(double radius) const;//Calculates velocity using energy calculation - gives results even outside area of current orbit
	void init(OBJHANDLE hmajor, OBJHANDLE hminor);//Initialiser
	void init(const VECTOR3 &rposition, const VECTOR3 &rvelocity, double gmplanet); //Initialiser using pos and vel vectors
	void init(const VECTOR3 &rposition, const VECTOR3 &rvelocity, double ttimestamp, double gmplanet); //Initialiser that pushes out a timestamp as well
	void minortomajorinit(const OrbitElements &craftinrmin, const OrbitElements &rmininrmaj, double soisize);
	void majortominorinit(OBJHANDLE target, OBJHANDLE object, const class Intercept &closestapproach, double soisize);//Going from major to minor
	void draworbit(oapi::Sketchpad *sketchpad, const class Graph *graph, bool drawradius) const; //Draws an orbit in window and projection described in graph
	//Can be called even if orbit.isvalid() is false
	void setinvalid(){valid=false;};//Forces an existing orbit structure into invalidity
	//The routines below generally ASSUME that orbit has been initialised
	double GetTimeToThi(double costhi, double sinthi,int fullorbits = 0,int halforbits = 0) const; //Gets time from present position to the angle thi.
	double GetTimeToRadius(double radius, bool outward) const; //Gets time from present position to the radius given. Better for hyperbolics.
	void vectortothi(const VECTOR3 &vector,double *costhi,double *sinthi) const; //Gets closest costhi and sinthi from vector angle (NOT from length)
	double thitoradius(double costhi) const; //cos thi to radius
	double radiustothi(double radius) const; //radius to cos thi
	void GetTimesToThi(double costhi, double *time1, double *time2,int fullorbits = 0,int halforbits = 0) const; //Gets time from present position to a given costhi (both + and - solutions)
	void thitovectors(double costhi, double sinthi, VECTOR3 *position, VECTOR3 *velocity) const; //Angle thi from periapsis to pos and vel
	void radiustovectors(double radius, bool outward, VECTOR3 *position, VECTOR3 *velocity) const; // Now works from radius length too - should always go from known information
	void timetovectors(double timefromnow, VECTOR3 *pos, VECTOR3 *vel) const;//Time from present to pos and vel vectors
	void timetovectors(double timefromnow,class OrbitTime *posvel) const;
	void minortomajor(const OrbitElements &rmininrmaj, OrbitElements *rmajorbit, double soisize) const; //Finds path from hyperbolic orbit around present body to orbit around rmaj
	double geteccentricity() const; //Parameter retrieving routines
	double getangmomentum2() const;
	double gettimeorbit() const {return orbitconstant*2*PI;};//Time to go around a complete orbit
	double getcurrcosthi() const;
	double getcurrsinthi() const;
	void getaxes(VECTOR3 *tmajoraxis, VECTOR3 *tminoraxis) const; //Major and minor axes
	void getcurrentvectors(VECTOR3 *tpos, VECTOR3 *tvel) const; //vectors when orbit defined
	double getsemimajor() const;
	VECTOR3 geteccentricityvector() const;
	VECTOR3 getplanevector() const;
	double gettimestamp() const;
	double getpedeltatime() const;
	double getinfinityvelocity() const;
	void getinfinityvelvector(bool outward,VECTOR3 *velocity) const;
	bool isvalid() const;//Says whether this structure has been initialised properly
	double getgmplanet() const;//mass of central body * GRAVITY
	double getcurrradius() const;
	double getpedistance() const;
	double getapodistance() const;
	VECTOR3 getintersectvector(const OrbitElements &torbit) const;//vector along line of intersection of two orbits (like align MFD)
	OrbitElements *getminorbarycentricorbit() const {return minoraboutbarycentre;};
};

class OrbitTime{
public:
	void getposvel(VECTOR3 *tpos,VECTOR3 *tvel);
	OrbitTime();
private:
	VECTOR3 pos,vel;
	double currangle,icosthi;
	bool processed;
	friend void OrbitElements::timetovectors(double timefromnow,OrbitTime *posvel) const;
	friend bool OrbitElements::improve(double timetarget,OrbitTime *posvel) const;
	friend void OrbitElements::improvebysubdivision(double timetarget,double topthi,double timeattopthi,OrbitTime *posvel) const;
	friend void OrbitElements::improvebyradius(double timetarget,double topthi,double timeattopthi,OrbitTime *posvel) const;
};
