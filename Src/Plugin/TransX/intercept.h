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

#ifndef __INTERCEPT_H
#define __INTERCEPT_H

#include "orbitelements.h"

class Intercept
{
	private:
		//Variables
		double iceptradius;
		OrbitTime alphatime,betatime;
		double icepttimeoffset;
		int iceptmethod;
		VECTOR3 iceptalpha;
		VECTOR3 iceptbeta;
		bool newintercept;
		VECTOR3 icraftpos;
		VECTOR3 icraftvel;
		VECTOR3 itargetpos;
		VECTOR3 itargetvel;
		VECTOR3 iplanecept;
		double itimeintercept;
		double gain;//oscillation controller
		double lasttimecorrection;//used in oscillation control
		int fullorbits,halforbits;//used for finding location of targets
        bool shouldUpdateBarycenter;
		//Private functions
		void improveinterceptstraightline(const OrbitElements &craft, const OrbitElements &target);//Straight line improvement on previous intercept
		void adjustorbitsdown();//Change the orbital offset
	public:
		Intercept(); //Default constructor
		void updateintercept(const OrbitElements &craft, const OrbitElements &target,double craftorbitsahead = 0);
		bool getvalid(){return !newintercept;};
		void ShouldUpdateBarycenter( bool bset ) { shouldUpdateBarycenter = bset; }
		void resetintercept();
		void getpositions(VECTOR3 *craftpos, VECTOR3 *targetpos) const;
		void getvelocities(VECTOR3 *craftvel, VECTOR3 *targetvel) const;
		void getrelpos(VECTOR3 *relpos) const;
		void getrelvel(VECTOR3 *relvel) const;
		void getplanecept(VECTOR3 *planecept) const;
		void getorbitsoffset(int *ifullorbits,int *ihalforbits) const;
		double gettimeintercept() const;
};

#endif
