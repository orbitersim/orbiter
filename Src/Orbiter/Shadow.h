// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SHADOW_H
#define __SHADOW_H

#include "D3d7util.h"

VOID Find2DConvexHull( uint32_t dwNumInVertices, VECTOR2D* pInVertices,
					   uint32_t* pdwNumOutIndices, WORD** ppOutIndices );

#endif // !__SHADOW_H