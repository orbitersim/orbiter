//-----------------------------------------------------------------------------
// File: geom.cpp
//
// Desc: Example code showing how to use stencil buffers to implement shadows
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Mirosoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include "shadow.h"




//-----------------------------------------------------------------------------
// Name: RotateVertexInX()
// Desc: Rotates an array of vertices by an amount theta about the x-axis.
//-----------------------------------------------------------------------------
VOID RotateVertexInX( FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {
                FLOAT y = pvInVertices[i].y;
                FLOAT z = pvInVertices[i].z;
                FLOAT ny = pvInVertices[i].ny;
                FLOAT nz = pvInVertices[i].nz;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else

                pvOutVertices[i].y = fCos*y + fSin*z;
                pvOutVertices[i].z = -fSin*y + fCos*z;

                pvOutVertices[i].ny = fCos*ny + fSin*nz;
                pvOutVertices[i].nz = -fSin*ny + fCos*nz;
        }
}

VOID TransRotateVertexInX(D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {
                FLOAT y = pvInVertices[i].y+transvec.y;
                FLOAT z = pvInVertices[i].z+transvec.z;
                FLOAT ny = pvInVertices[i].ny;
                FLOAT nz = pvInVertices[i].nz;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else

                pvOutVertices[i].y = fCos*y + fSin*z - transvec.y;
                pvOutVertices[i].z = -fSin*y + fCos*z - transvec.z;

                pvOutVertices[i].ny = fCos*ny + fSin*nz;
                pvOutVertices[i].nz = -fSin*ny + fCos*nz;
        }
}

VOID RotateVertexInZ( FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {

                FLOAT x = pvInVertices[i].x;
                FLOAT y = pvInVertices[i].y;
                FLOAT ny = pvInVertices[i].ny;
                FLOAT nx = pvInVertices[i].nx;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else

                pvOutVertices[i].x = fCos*x + fSin*y;
                pvOutVertices[i].y = -fSin*x + fCos*y;


                pvOutVertices[i].nx = fCos*nx + fSin*ny;
                pvOutVertices[i].ny = -fSin*nx + fCos*ny;
        }
}

VOID TransRotateVertexInZ(D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {

                FLOAT x = pvInVertices[i].x+transvec.x;
                FLOAT y = pvInVertices[i].y+transvec.y;
                FLOAT ny = pvInVertices[i].ny;
                FLOAT nx = pvInVertices[i].nx;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else

                pvOutVertices[i].x = fCos*x + fSin*y - transvec.x;
                pvOutVertices[i].y = -fSin*x + fCos*y - transvec.y;


                pvOutVertices[i].nx = fCos*nx + fSin*ny;
                pvOutVertices[i].ny = -fSin*nx + fCos*ny;
        }
}


VOID TransRotateVertexInY(D3DVECTOR &transvec, FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {

                FLOAT x = pvInVertices[i].x+transvec.x;
                FLOAT z = pvInVertices[i].z+transvec.z;
                FLOAT nz = pvInVertices[i].nz;
                FLOAT nx = pvInVertices[i].nx;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else first

                pvOutVertices[i].x = fCos*x + fSin*z - transvec.x;
                pvOutVertices[i].z = -fSin*x + fCos*z - transvec.z;


                pvOutVertices[i].nx = fCos*nx + fSin*nz;
                pvOutVertices[i].nz = -fSin*nx + fCos*nz;
        }
}

VOID RotateVertexInY( FLOAT fTheta, DWORD dwCount,
                                          D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
        FLOAT fCos = (FLOAT)cos(fTheta);
    
        for( DWORD i=0; i<dwCount; i++ )
        {

                FLOAT x = pvInVertices[i].x;
                FLOAT z = pvInVertices[i].z;
                FLOAT nz = pvInVertices[i].nz;
                FLOAT nx = pvInVertices[i].nx;

                pvOutVertices[i]=pvInVertices[i];  // copy everything else first

                pvOutVertices[i].x = fCos*x + fSin*z;
                pvOutVertices[i].z = -fSin*x + fCos*z;

                pvOutVertices[i].nx = fCos*nx + fSin*nz;
                pvOutVertices[i].nz = -fSin*nx + fCos*nz;
        }
}




//-----------------------------------------------------------------------------
// Name: GenerateSphere()
// Desc: Makes vertex and index data for a sphere.
//-----------------------------------------------------------------------------
BOOL GenerateSphere(SHADOWCASTER *obj,D3DVECTOR& vCenter, FLOAT fRadius, WORD wNumRings, WORD wNumSections,
                                         FLOAT sx, FLOAT sy, FLOAT sz)
{
    FLOAT x, y, z, v, rsintheta; // Temporary variables
    WORD  i, j, n, m;            // counters
    D3DVECTOR vPoint;

    //Generate space for the required triangles and vertices.
    WORD       wNumTriangles = (wNumRings + 1) * wNumSections * 2;
    DWORD      dwNumIndices   = wNumTriangles*3;
    DWORD      dwNumVertices  = (wNumRings + 1) * wNumSections + 2;
    D3DVERTEX* pvVertices     = new D3DVERTEX[dwNumVertices];
    WORD*      pwIndices      = new WORD[3*wNumTriangles];

    // Generate vertices at the top and bottom points.
    D3DVECTOR vTopPoint  = vCenter + D3DVECTOR( 0.0f, +sy*fRadius, 0.0f);
    D3DVECTOR vBotPoint  = vCenter + D3DVECTOR( 0.0f, -sy*fRadius, 0.0f);
    D3DVECTOR vNormal = D3DVECTOR( 0.0f, 0.0f, 1.0f );
    pvVertices[0]               = D3DVERTEX( vTopPoint,  vNormal, 0.0f, 0.0f );
    pvVertices[dwNumVertices-1] = D3DVERTEX( vBotPoint, -vNormal, 0.0f, 0.0f );

    // Generate vertex points for rings
    FLOAT dtheta = (float)(g_PI / (wNumRings + 2));     //Angle between each ring
    FLOAT dphi   = (float)(2*g_PI / wNumSections); //Angle between each section
    FLOAT theta  = dtheta;
    n = 1; //vertex being generated, begins at 1 to skip top point

    for( i = 0; i < (wNumRings+1); i++ )
    {
        y = fRadius * (float)cos(theta); // y is the same for each ring
        v = theta / g_PI;     // v is the same for each ring
        rsintheta = fRadius * (float)sin(theta);
        FLOAT phi = 0.0f;

        for( j = 0; j < wNumSections; j++ )
        {
            x = rsintheta * (float)sin(phi);
            z = rsintheta * (float)cos(phi);
        
            FLOAT u = (FLOAT)(1.0 - phi / (2*g_PI) );
            
            vPoint        = vCenter + D3DVECTOR( sx*x, sy*y, sz*z );
            vNormal       = D3DVECTOR( x/fRadius, y/fRadius, z/fRadius );
            pvVertices[n] = D3DVERTEX( vPoint, vNormal, u, v );

            phi += dphi;
            ++n;
        }
        theta += dtheta;
    }

    // Generate triangles for top and bottom caps.
    for( i = 0; i < wNumSections; i++ )
    {
        pwIndices[3*i+0] = 0;
        pwIndices[3*i+1] = i + 1;
        pwIndices[3*i+2] = 1 + ((i + 1) % wNumSections);

        pwIndices[3*(wNumTriangles - wNumSections + i)+0] = (WORD)( dwNumVertices - 1 );
        pwIndices[3*(wNumTriangles - wNumSections + i)+1] = (WORD)( dwNumVertices - 2 - i );
        pwIndices[3*(wNumTriangles - wNumSections + i)+2] = (WORD)( dwNumVertices - 2 - 
                ((1 + i) % wNumSections) );
    }

    // Generate triangles for the rings
    m = 1;            // first vertex in current ring,begins at 1 to skip top point
    n = wNumSections; // triangle being generated, skip the top cap 
        
    for( i = 0; i < wNumRings; i++ )
    {
        for( j = 0; j < wNumSections; j++ )
        {
            pwIndices[3*n+0] = m + j;
            pwIndices[3*n+1] = m + wNumSections + j;
            pwIndices[3*n+2] = m + wNumSections + ((j + 1) % wNumSections);
            
            pwIndices[3*(n+1)+0] = pwIndices[3*n+0];
            pwIndices[3*(n+1)+1] = pwIndices[3*n+2];
            pwIndices[3*(n+1)+2] = m + ((j + 1) % wNumSections);
            
            n += 2;
        }
        m += wNumSections;
    }

    obj->dwNumVertices = dwNumVertices;
    obj->dwNumIndices  = dwNumIndices;
    obj->pIndices      = pwIndices;
    obj->pVertices     = pvVertices;
    obj->vCenter       = vCenter;

    return TRUE;
}


