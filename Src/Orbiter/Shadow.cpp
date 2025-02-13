// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// shadow calculations


#include "Shadow.h"
#include "Log.h"

BOOL ccw( VECTOR2D* P[], int i, int j, int k )
{
	FLOAT a = P[i]->x - P[j]->x;
	FLOAT b = P[i]->y - P[j]->y;
	FLOAT c = P[k]->x - P[j]->x;
	FLOAT d = P[k]->y - P[j]->y;

	return ( a*d - b*c <= 0 ); // TRUE if points i, j, k are counterclockwise
}




int cmpl( const VOID* a, const VOID* b )
{
    VECTOR2D** pVertexPtrA = (VECTOR2D**)a;
    VECTOR2D** pVertexPtrB = (VECTOR2D**)b;

	FLOAT vx = (*pVertexPtrA)->x - (*pVertexPtrB)->x;
	if( vx > 0.0f ) return 1;
	if( vx < 0.0f ) return -1;

	FLOAT vy = (*pVertexPtrB)->y - (*pVertexPtrA)->y;
	if( vy > 0.0f ) return 1;
	if( vy < 0.0f ) return -1;

	return 0;
}




int cmph( const VOID* a, const VOID* b )
{
	return cmpl( b, a );
}




uint32_t MakeChain(VECTOR2D** pVertexPtrs, uint32_t dwNumVertexPtrs,
				 int (*cmp)(const void*, const void*) ) 
{
	uint32_t s = 1;

	qsort( pVertexPtrs, dwNumVertexPtrs, sizeof(VECTOR2D*), cmp );
	
	for( uint32_t i=2; i<dwNumVertexPtrs; i++ ) 
	{
		uint32_t j = s;
		while( j>=1 && ccw( pVertexPtrs, i, j, j-1 ) )
			j--;

		s = j+1;

		// Swap the vertices s and i
		VECTOR2D* tmp = pVertexPtrs[s];
		pVertexPtrs[s] = pVertexPtrs[i];
		pVertexPtrs[i] = tmp;
	}

	return s;
}




int ConvexHull2D( VECTOR2D** pVertexPtrs, uint32_t dwNumVertexPtrs )
{
	if( !dwNumVertexPtrs )
		return 0;

	uint32_t cnt = 0;

	// Make lower hull
	cnt += MakeChain( &pVertexPtrs[0], dwNumVertexPtrs, cmpl );
	
	pVertexPtrs[dwNumVertexPtrs] = pVertexPtrs[0];
	
	// Make upper hull
	cnt += MakeChain( &pVertexPtrs[cnt], dwNumVertexPtrs-cnt+1, cmph );

	return cnt;
}



VOID Find2DConvexHull( uint32_t dwNumInVertices, VECTOR2D* pInVertices,
					   uint32_t* pdwNumOutIndices, WORD** ppOutIndices )
{
	uint32_t i;
	static uint32_t dwVtxBuf = 0;
	static WORD *pIndices;
	// Allocate memory for the hull (max size is dwNumInVertices+1 times the
	// the storage space for a uint32_t and a ptr to a vertex
	static uint32_t dwElementSize = sizeof(uint32_t) + sizeof(VECTOR2D*);
	if (dwNumInVertices > dwVtxBuf) {
		if (dwVtxBuf) delete []pIndices;
		pIndices = (WORD*)(new BYTE[(dwNumInVertices+1)*dwElementSize]); TRACENEW
		dwVtxBuf = dwNumInVertices;
	}
    uint32_t dwNumIndices  = 0;

	VECTOR2D** ppVertices = (VECTOR2D**)&pIndices[dwNumInVertices+1];

	// The algorithm requires array of ptrs to vertices (for qsort) instead of
	// array of vertices, so do the conversion
    for (i = 0; i < dwNumInVertices; i++)
		ppVertices[i] = &pInVertices[i];

	dwNumIndices = ConvexHull2D( ppVertices, dwNumInVertices );

    // Convert back to array of indices
    for( i=0; i<dwNumIndices;i++ )
		pIndices[i] = (WORD)( ppVertices[i] - &pInVertices[0] );

	(*pdwNumOutIndices) = dwNumIndices;
	(*ppOutIndices)     = pIndices;
}

