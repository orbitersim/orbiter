//-----------------------------------------------------------------------------
// File: xofsave.cpp
//
// Desc: Save a .X File using the DirectXFile API
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID

#include <windows.h>
#include <iostream.h>
#include <objbase.h>
#include "dxfile.h"
#include "rmxfguid.h"
#include "rmxftmpl.h"
#include "xofsave.h"

// Define the Vertex information for our object
Vector CUBE_VERTICES[] = {
     1,  1, -1,
    -1,  1, -1,
    -1,  1,  1,
     1,  1,  1,
     1, -1, -1,
    -1, -1, -1,
    -1, -1,  1,
     1, -1,  1
};

// Define the Face information for our object
MeshFace CUBE_FACES[] = {
    3,0,1,2,
    3,0,2,3,
    3,0,4,5,
    3,0,5,1,
    3,1,5,6,
    3,1,6,2,
    3,2,6,7,
    3,2,7,3,
    3,3,7,4,
    3,3,4,0,
    3,4,7,6,
    3,4,6,5
};

// Main program
int main(int argc, char *argv[])
{
    LPDIRECTXFILE pxofapi = NULL;
    LPDIRECTXFILESAVEOBJECT pxofsave = NULL;
    DXFILEFORMAT xFormat = DXFILEFORMAT_TEXT;
    HRESULT hr;

    // Get command line arguments.
    // -b -B /b /B for saving as binary format
    // otherwise text format.
    if (argc > 1 &&
        strlen(argv[1]) > 1 &&
        (argv[2][1] == 'b' || argv[2][1] == 'B'))
        xFormat = DXFILEFORMAT_BINARY;

    // Using a do/while(FALSE) loop instead of goto for errors
    do {

        // Create xofapi object.
        hr = DirectXFileCreate(&pxofapi);
        if (FAILED(hr))
            break;

        // Registe templates for d3drm.
        hr = pxofapi->RegisterTemplates((LPVOID)D3DRM_XTEMPLATES,
                                        D3DRM_XTEMPLATE_BYTES);
        if (FAILED(hr))
            break;

        // Create save object.
        hr = pxofapi->CreateSaveObject("test.x",    // filename
                                       xFormat,     // binary or text
                                       &pxofsave);
        if (FAILED(hr))
            break;

        // Create data object.
        // First create data chunk.

        LPVOID         pvData;
        DWORD          cbSize;

        cbSize = sizeof(DWORD) + 8 * sizeof(Vector) +
                 sizeof(DWORD) + 12 * 4 * sizeof(DWORD);

        pvData = new char[cbSize];
        if (!pvData)
            break;

        MeshVertices *pVerts = (MeshVertices *)pvData;

        pVerts->nVertices = 8;
        memcpy (pVerts->vertices, CUBE_VERTICES, sizeof(CUBE_VERTICES));

        MeshFaces *pFaces = (MeshFaces *)&pVerts->vertices[8];

        pFaces->nFaces = 12;

        memcpy (pFaces->faces, CUBE_FACES, sizeof(CUBE_FACES));

        // Then call api to create data object.

        LPDIRECTXFILEDATA pxofobj = NULL;
        hr = pxofsave->CreateDataObject(TID_D3DRMMesh,
                                        "cube1",    // meshname
                                        NULL,       // uuid *
                                        cbSize,     // data chunk size.
                                        pvData,     // pointer to data chunk.
                                        &pxofobj);
        delete pvData;
        if (FAILED(hr)) {
            break;
        }

        // If you have optional data, save it by
        // 1. create a child object pxofChild.
        // 2. add child object with pxofobj->AddDataObject(pxofChild).
        //    or AddDataReference() or AddBinaryObject()
        // 3. release child object pxofChild->Release().

        // Save data object.
        hr = pxofsave->SaveData(pxofobj);
        pxofobj->Release();

    } while (FALSE);

    // Free up outstanding interfaces
    if (pxofsave)
        pxofsave->Release();

    if (pxofapi)
        pxofapi->Release();

    cout << "Done.\n";

    return 0;
}
