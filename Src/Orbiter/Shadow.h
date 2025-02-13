// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SHADOW_H
#define __SHADOW_H

#include "D3d7util.h"

VOID Find2DConvexHull( DWORD dwNumInVertices, VECTOR2D* pInVertices,
					   DWORD* pdwNumOutIndices, WORD** ppOutIndices );

#endif // !__SHADOW_H