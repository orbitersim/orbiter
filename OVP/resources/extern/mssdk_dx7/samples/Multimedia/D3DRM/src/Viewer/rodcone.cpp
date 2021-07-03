/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: rodcone.cpp
 *
 ***************************************************************************/

/*
 * Sample code for building objects out of rods and cones.
 */

#include <d3drmwin.h>
#include "viewer.h"
#include "rodcone.h"
#include <math.h>

static unsigned long rod_faces[] =
{   8, 7, 7, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0, /* end 1 */
    4, 0, 0, 1, 1, 9, 1, 8, 0,	/* side 0 */
    4, 1, 1, 2, 2, 10, 2, 9, 1,	/* side 1 */
    4, 2, 2, 3, 3, 11, 3, 10, 2, /* side 2 */
    4, 3, 3, 4, 4, 12, 4, 11, 3, /* side 3 */
    4, 4, 4, 5, 5, 13, 5, 12, 4, /* side 4 */
    4, 5, 5, 6, 6, 14, 6, 13, 5, /* side 5 */
    4, 6, 6, 7, 7, 15, 7, 14, 6, /* side 6 */
    4, 7, 7, 0, 0, 8, 0, 15, 7,		/* side 7 */
    8, 8, 0, 9, 1, 10, 2, 11, 3, 12, 4, 13, 5, 14, 6, 15, 7, /* end 2 */
    0,
};

void AddRod(LPDIRECT3DRMMESHBUILDER3 mesh, D3DVALUE radius, D3DVECTOR a, D3DVECTOR b)
{
    D3DVECTOR d, u, r;
    D3DVECTOR v[16];
    D3DVECTOR n[8];
    D3DVALUE f;
    int i;

    /*
     * Find the unit vector along the rod.
     */
    d.x = b.x - a.x;
    d.y = b.y - a.y;
    d.z = b.z - a.z;
    D3DRMVectorNormalise(&d);

    /*
     * Pick a vector normal to d
     */
    if (d.y != D3DVAL(0.0) || d.z != D3DVAL(0.0))
    {	u.x = D3DVAL(0.0);
	if (d.y == D3DVAL(0.0))
	{   u.y = D3DVAL(1.0);
	    u.z = D3DVAL(0.0);
	} else
	{   D3DVALUE n_fix =
		D3DVAL(1.0)
	    +	D3DDivide(D3DMultiply(d.z, d.z), D3DMultiply(d.y, d.y));
#ifdef FIXED_POINT_API
	    double un_val = (double)n_fix / (double)(1<<16);
	    u.z = D3DVAL(sqrt(1/un_val));
#else
	    u.z = D3DVAL(sqrt(D3DDivide(D3DVAL(1.0), D3DVAL(n_fix))));
#endif
	    u.y = -D3DMultiply(u.z, D3DDivide(d.z, d.y));
	}
    } else
    {	u.x = D3DVAL(0.0);
	u.y = D3DVAL(0.0);
	u.z = D3DVAL(1.0);
    }

    /*
     * Now find a vector normal to them both, to give us a coordinate
     * system in the plane normal to the rod.
     */
    D3DRMVectorCrossProduct(&r, &d, &u);

    /*
     * Scale down the coordinates to the radius of the rod.
     */
    u.x = D3DMultiply(u.x, radius);
    u.y = D3DMultiply(u.y, radius);
    u.z = D3DMultiply(u.z, radius);
    r.x = D3DMultiply(r.x, radius);
    r.y = D3DMultiply(r.y, radius);
    r.z = D3DMultiply(r.z, radius);

    /*
     * Calculate the corners of an octagon.
     */
    f = D3DVAL((float)sqrt(2.0) / (2 * (1 + (float)sqrt(2.0) / 2)));
    v[0].x = u.x + D3DMultiply(r.x, f);
    v[0].y = u.y + D3DMultiply(r.y, f);
    v[0].z = u.z + D3DMultiply(r.z, f);

    v[1].x = D3DMultiply(u.x, f) + r.x;
    v[1].y = D3DMultiply(u.y, f) + r.y;
    v[1].z = D3DMultiply(u.z, f) + r.z;

    v[2].x = D3DMultiply(-u.x, f) + r.x;
    v[2].y = D3DMultiply(-u.y, f) + r.y;
    v[2].z = D3DMultiply(-u.z, f) + r.z;

    v[3].x = -u.x + D3DMultiply(r.x, f);
    v[3].y = -u.y + D3DMultiply(r.y, f);
    v[3].z = -u.z + D3DMultiply(r.z, f);

    v[4].x = -u.x - D3DMultiply(r.x, f);
    v[4].y = -u.y - D3DMultiply(r.y, f);
    v[4].z = -u.z - D3DMultiply(r.z, f);

    v[5].x = D3DMultiply(-u.x, f) - r.x;
    v[5].y = D3DMultiply(-u.y, f) - r.y;
    v[5].z = D3DMultiply(-u.z, f) - r.z;

    v[6].x = D3DMultiply(u.x, f) - r.x;
    v[6].y = D3DMultiply(u.y, f) - r.y;
    v[6].z = D3DMultiply(u.z, f) - r.z;

    v[7].x = u.x - D3DMultiply(r.x, f);
    v[7].y = u.y - D3DMultiply(r.y, f);
    v[7].z = u.z - D3DMultiply(r.z, f);

    /*
     * Add the rod endpoints and calculate the vertex normals.
     */
    for (i = 0; i < 8; i++)
    {	n[i] = v[i];
	D3DRMVectorNormalise(&n[i]);
	v[i + 8].x = v[i].x + b.x;
	v[i + 8].y = v[i].y + b.y;
	v[i + 8].z = v[i].z + b.z;
	v[i].x += a.x;
	v[i].y += a.y;
	v[i].z += a.z;
    }

    /*
     * Now add the faces.
     */
    mesh->AddFaces(16, v, 8, n, rod_faces, NULL);
}

static unsigned long cone_faces[] =
{   8, 7, 7, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0, /* end 1 */
    3, 0, 0, 1, 1, 8, 1,	/* side 0 */
    3, 1, 1, 2, 2, 8, 1,	/* side 1 */
    3, 2, 2, 3, 3, 8, 1, /* side 2 */
    3, 3, 3, 4, 4, 8, 1, /* side 3 */
    3, 4, 4, 5, 5, 8, 1, /* side 4 */
    3, 5, 5, 6, 6, 8, 1, /* side 5 */
    3, 6, 6, 7, 7, 8, 1, /* side 6 */
    3, 7, 7, 0, 0, 8, 1,		/* side 7 */
    0,
};

void AddCone(LPDIRECT3DRMMESHBUILDER3 mesh, D3DVALUE radius, D3DVECTOR a, D3DVECTOR b)
{
    D3DVECTOR d, u, r;
    D3DVECTOR v[16];
    D3DVECTOR n[8];
    D3DVALUE f;
    int i;

    /*
     * Find the unit vector along the rod.
     */
    d.x = b.x - a.x;
    d.y = b.y - a.y;
    d.z = b.z - a.z;
    D3DRMVectorNormalise(&d);

    /*
     * Pick a vector normal to d
     */
    if (d.y != D3DVAL(0.0) || d.z != D3DVAL(0.0))
    {	u.x = D3DVAL(0.0);
	if (d.y == D3DVAL(0.0))
	{   u.y = D3DVAL(1.0);
	    u.z = D3DVAL(0.0);
	} else
	{   D3DVALUE n_fix =
		D3DVAL(1.0)
	    +	D3DDivide(D3DMultiply(d.z, d.z), D3DMultiply(d.y, d.y));
#ifdef FIXED_POINT_API
	    double un_val = (double)n_fix / (double)(1<<16);
	    u.z = D3DVAL(sqrt(1 / un_val));
#else
	    u.z = D3DVAL(sqrt(D3DVAL(1.0) / D3DVAL(n_fix)));
#endif
	    u.y = - D3DDivide(D3DMultiply(u.z, d.z), d.y);
	}
    } else
    {	u.x = D3DVAL(0.0);
	u.y = D3DVAL(0.0);
	u.z = D3DVAL(1.0);
    }

    /*
     * Now find a vector normal to them both, to give us a coordinate
     * system in the plane normal to the rod.
     */
    D3DRMVectorCrossProduct(&r, &d, &u);

    /*
     * Scale down the coordinates to the radius of the rod.
     */
    u.x = D3DMultiply(u.x, radius);
    u.y = D3DMultiply(u.y, radius);
    u.z = D3DMultiply(u.z, radius);
    r.x = D3DMultiply(r.x, radius);
    r.y = D3DMultiply(r.y, radius);
    r.z = D3DMultiply(r.z, radius);

    /*
     * Calculate the corners of an octagon.
     */
    f = D3DVAL((float)sqrt(2.0) / (2 * (1 + (float)sqrt(2.0) / 2)));
    v[0].x = u.x + D3DMultiply(r.x, f);
    v[0].y = u.y + D3DMultiply(r.y, f);
    v[0].z = u.z + D3DMultiply(r.z, f);

    v[1].x = D3DMultiply(u.x, f) + r.x;
    v[1].y = D3DMultiply(u.y, f) + r.y;
    v[1].z = D3DMultiply(u.z, f) + r.z;

    v[2].x = D3DMultiply(-u.x, f) + r.x;
    v[2].y = D3DMultiply(-u.y, f) + r.y;
    v[2].z = D3DMultiply(-u.z, f) + r.z;

    v[3].x = -u.x + D3DMultiply(r.x, f);
    v[3].y = -u.y + D3DMultiply(r.y, f);
    v[3].z = -u.z + D3DMultiply(r.z, f);

    v[4].x = -u.x - D3DMultiply(r.x, f);
    v[4].y = -u.y - D3DMultiply(r.y, f);
    v[4].z = -u.z - D3DMultiply(r.z, f);

    v[5].x = D3DMultiply(-u.x, f) - r.x;
    v[5].y = D3DMultiply(-u.y, f) - r.y;
    v[5].z = D3DMultiply(-u.z, f) - r.z;

    v[6].x = D3DMultiply(u.x, f) - r.x;
    v[6].y = D3DMultiply(u.y, f) - r.y;
    v[6].z = D3DMultiply(u.z, f) - r.z;

    v[7].x = u.x - D3DMultiply(r.x, f);
    v[7].y = u.y - D3DMultiply(r.y, f);
    v[7].z = u.z - D3DMultiply(r.z, f);

    v[8] = b;

    /*
     * Calculate the vertex normals.
     */
    for (i = 0; i < 8; i++)
    {	n[i] = v[i];
	D3DRMVectorNormalise(&n[0]);
	v[i].x += a.x;
	v[i].y += a.y;
	v[i].z += a.z;
    }

    /*
     * Now add the faces.
     */
    mesh->AddFaces(9, v, 8, n, cone_faces, NULL);
}
