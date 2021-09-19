// =================================================================================================================================
//
// Copyright (C) 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense
// copies of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) You do not remove or alter any copyright notices contained within the Software.
// d) This copyright notice must be included in all copies or substantial portions of the Software.
//
// If the Software is distributed in an object code form then in addition to conditions above:
// e) It must inform that the source code is available and how to obtain it.
// f) It must display "NO WARRANTY" and "DISCLAIMER OF LIABILITY" statements on behalf of all contributors like the one below.
//
// The accompanying materials such as artwork, if any, are provided under the terms of this license unless otherwise noted. 
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#ifndef __ORBIT_ReferenceClass_H
#define __ORBIT_ReferenceClass_H

#include "OrbiterAPI.h"

class ReferenceClass {
	
public:

	ReferenceClass();
    ~ReferenceClass();

	OBJHANDLE   FindStar();
	OBJHANDLE   Get2ndReferenceForShip();
	OBJHANDLE	GetReference(OBJHANDLE);
	OBJHANDLE	GetReferenceByName(char *);
	double		GetSOI(OBJHANDLE obj);
	OBJHANDLE * GetSystemList(OBJHANDLE x);
	int			GetSystemCount(OBJHANDLE x);
    bool		IsGbody(OBJHANDLE x);

	OBJHANDLE	StarHandle;

private:

	OBJHANDLE	FindGravityReference(OBJHANDLE);
	void		CreateDatabase();

	struct ReferenceClass_info {	
		OBJHANDLE	handle;
		OBJHANDLE	grf_handle;
		double		soi;
		double		dist;  // Distance where this ReferenceClass is active
		OBJHANDLE   system[256];
		int         sys_index;
	};

	int total_count;
	struct ReferenceClass_info *References;
	
};

extern class ReferenceClass *Refer;

#endif

