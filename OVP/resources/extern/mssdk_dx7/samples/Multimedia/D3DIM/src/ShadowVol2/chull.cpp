// File: chull.cpp
// 
// Desc: Convex-hull code for the D3D shadow demo. 
//       This has been modified from Clarkson's original.

/*
 * Ken Clarkson wrote this.  Copyright (c) 1996 by AT&T..
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */


/*
 * two-dimensional convex hull
 * read points from stdin,
 *      one point per line, as two numbers separated by whitespace
 * on stdout, points on convex hull in order around hull, given
 *      by their numbers in input order
 * the results should be "robust", and not return a wildly wrong hull,
 *	despite using floating point
 * works in O(n log n); I think a bit faster than Graham scan;
 * 	somewhat like Procedure 8.2 in Edelsbrunner's "Algorithms in Combinatorial
 *	Geometry".
 */

#include <stdlib.h>
#include "shadow.h"





BOOL ccw( COLORVERTEX* P[], int i, int j, int k )
{
	FLOAT a = P[i]->p.x - P[j]->p.x;
	FLOAT b = P[i]->p.y - P[j]->p.y;
	FLOAT c = P[k]->p.x - P[j]->p.x;
	FLOAT d = P[k]->p.y - P[j]->p.y;

	return ( a*d - b*c <= 0 ); // TRUE if points i, j, k are counterclockwise
}




int cmpl( const VOID* a, const VOID* b )
{
    COLORVERTEX** pVertexPtrA = (COLORVERTEX**)a;
    COLORVERTEX** pVertexPtrB = (COLORVERTEX**)b;

	FLOAT vx = (*pVertexPtrA)->p.x - (*pVertexPtrB)->p.x;
	if( vx > 0.0f ) return 1;
	if( vx < 0.0f ) return -1;

	FLOAT vy = (*pVertexPtrB)->p.y - (*pVertexPtrA)->p.y;
	if( vy > 0.0f ) return 1;
	if( vy < 0.0f ) return -1;

	return 0;
}




int cmph( const VOID* a, const VOID* b )
{
	return cmpl( b, a );
}




DWORD MakeChain( COLORVERTEX** pVertexPtrs, DWORD dwNumVertexPtrs,
				 int (*cmp)(const void*, const void*) ) 
{
	DWORD s = 1;

	qsort( pVertexPtrs, dwNumVertexPtrs, sizeof(COLORVERTEX*), cmp );
	
	for( DWORD i=2; i<dwNumVertexPtrs; i++ ) 
	{
		DWORD j = s;
		while( j>=1 && ccw( pVertexPtrs, i, j, j-1 ) )
			j--;

		s = j+1;

		// Swap the vertices s and i
		COLORVERTEX* tmp = pVertexPtrs[s];
		pVertexPtrs[s] = pVertexPtrs[i];
		pVertexPtrs[i] = tmp;
	}

	return s;
}




int ConvexHull2D( COLORVERTEX** pVertexPtrs, DWORD dwNumVertexPtrs )
{
	if( !dwNumVertexPtrs )
		return 0;

	DWORD cnt = 0;

	// Make lower hull
	cnt += MakeChain( &pVertexPtrs[0], dwNumVertexPtrs, cmpl );
	
	pVertexPtrs[dwNumVertexPtrs] = pVertexPtrs[0];
	
	// Make upper hull
	cnt += MakeChain( &pVertexPtrs[cnt], dwNumVertexPtrs-cnt+1, cmph );

	return cnt;
}




VOID Find2DConvexHull( DWORD dwNumInVertices, COLORVERTEX* pInVertices,
					   DWORD* pdwNumOutIndices, WORD** ppOutIndices )
{
	// Allocate memory for the hull (max size is dwNumInVertices+1 times the
	// the storage space for a DWORD and a ptr to a vertex
	DWORD dwElementSize = sizeof(DWORD) + sizeof(COLORVERTEX*);
    WORD* pIndices      = (WORD*)(new BYTE[(dwNumInVertices+1)*dwElementSize]);
    DWORD dwNumIndices  = 0;

	COLORVERTEX** ppVertices = (COLORVERTEX**)&pIndices[dwNumInVertices+1];

	// The algorithm requires array of ptrs to vertices (for qsort) instead of
	// array of vertices, so do the conversion
    for( DWORD i=0; i<dwNumInVertices; i++ )
		ppVertices[i] = &pInVertices[i];

	dwNumIndices = ConvexHull2D( ppVertices, dwNumInVertices );

    // Convert back to array of indices
    for( i=0; i<dwNumIndices;i++ )
		pIndices[i] = (WORD)( ppVertices[i] - &pInVertices[0] );

	(*pdwNumOutIndices) = dwNumIndices;
	(*ppOutIndices)     = pIndices;      // Caller will free returned array
}

