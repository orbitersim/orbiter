//-----------------------------------------------------------------------------
// File: xofsave.h
//
// Desc: Header file for D3DRM XofSave sample
//
// Copyright (C) 1998 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------

#ifndef _XOFSAVE_H_
#define _XOFSAVE_H_

class Vector
{
public:
    float x;
    float y;
    float z;
};

class MeshVertices
{
public:
    DWORD nVertices;
    Vector vertices[1];
};

class MeshFace
{
public:
    DWORD nFaceVertexIndices;
    DWORD faceVertexIndices[1];
};

class MeshFaces
{
public:
    DWORD nFaces;
    MeshFace faces[1];
};

#endif
