//-----------------------------------------------------------------------------
// File: D3DFile.cpp
//
// Desc: Support code for loading DirectX .X files.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <stdio.h>
#include <tchar.h>
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DTextr.h"
#include "dxfile.h"
#include "rmxfguid.h"
#include "rmxftmpl.h"
#include "D3DFile.h"




//-----------------------------------------------------------------------------
// Name: GetFace
// Desc: Get the nth face
//-----------------------------------------------------------------------------
DWORD* GetFace( DWORD* pFaceData, DWORD dwFace )
{
    for( DWORD i=0; i<dwFace; i++ )
        pFaceData += (*pFaceData) + 1;

    return pFaceData;
}




//-----------------------------------------------------------------------------
// Name: GetNumIndices
// Desc: Get number of indices from face data
//-----------------------------------------------------------------------------
DWORD GetNumIndices( DWORD* pFaceData, DWORD dwNumFaces )
{
    DWORD dwNumIndices = 0;
    while( dwNumFaces-- > 0 )
    {
        dwNumIndices += (*pFaceData-2)*3;
        pFaceData += *pFaceData + 1;
    }

    return dwNumIndices;
}




//-----------------------------------------------------------------------------
// Name: CD3DFileBaseObject()
// Desc: 
//-----------------------------------------------------------------------------
CD3DFileObject::CD3DFileObject( TCHAR* strName )
{
    m_pNext        = NULL;
    m_pChild       = NULL;
    m_strName[0]   = 0;
    m_bHasMeshData = FALSE;

    if( strName )
        lstrcpy( m_strName, strName );

    // Set a default matrix
    D3DUtil_SetIdentityMatrix( m_mat );

    // Set a default material
    D3DUtil_InitMaterial( m_Material[0].m_mtrl, 1.0f, 1.0f, 1.0f );
    ZeroMemory( m_Material, sizeof(m_Material) );
    m_dwNumMaterials = 0;
    m_bHasAlpha      = FALSE;

    // Clear out vertex data
    m_dwNumVertices = 0L;
    m_pVertices     = NULL;
    m_dwNumIndices  = 0L;
    m_pIndices      = NULL;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
CD3DFileObject::~CD3DFileObject()
{
    SAFE_DELETE( m_pChild );
    SAFE_DELETE( m_pNext );

    for( DWORD i=0; i<m_dwNumMaterials; i++ )
        D3DTextr_DestroyTexture( m_Material[i].m_strTexture );

    SAFE_DELETE( m_pVertices );
    SAFE_DELETE( m_pIndices );
}




//-----------------------------------------------------------------------------
// Name: SetMeshGeometry()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CD3DFileObject::SetMeshGeometry( D3DVECTOR* pVertexData,
                                     DWORD dwNumVertices, DWORD* pFaceData,
                                     DWORD dwNumFaces )
{
    // Set up vertices
    m_dwNumVertices = dwNumVertices;
    m_pVertices     = new D3DVERTEX[m_dwNumVertices];
    if( NULL == m_pVertices )
        return E_FAIL;

    for( DWORD i=0; i< m_dwNumVertices; i++ )
    {
        ZeroMemory( &m_pVertices[i], sizeof(D3DVERTEX) );
        m_pVertices[i].x = pVertexData[i].x;
        m_pVertices[i].y = pVertexData[i].y;
        m_pVertices[i].z = pVertexData[i].z;
    }

    // Count the number of indices (converting n-sided faces to triangles)
    m_dwNumIndices = GetNumIndices( pFaceData, dwNumFaces );

    // Allocate memory for the indices, you must call AddFace() to set the vertices
    m_pIndices = new WORD[m_dwNumIndices];

    if( NULL == m_pIndices )
        return E_FAIL;

    m_bHasMeshData  = TRUE;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CD3DFileObject::AddChild( CD3DFileObject* pChild )
{
    if( m_pChild )
        m_pChild->AddNext( pChild );
    else
        m_pChild = pChild;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CD3DFileObject::AddNext( CD3DFileObject* pNext )
{
    if( m_pNext )
        m_pNext->AddNext( pNext );
    else
        m_pNext = pNext;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CD3DFileObject::Render( LPDIRECT3DDEVICE7 pd3dDevice, BOOL bAlpha )
{
    if( m_bHasMeshData )
    {
        // Render the mesh
        WORD* pIndices = m_pIndices;
        for( DWORD i=0; i <= m_dwNumMaterials; i++ )
        {
            // Skip materials with no references
            if( 0L == m_Material[i].m_dwNumIndices )
                continue;

            // Render opaque and transparent meshes during separate passes
            if( bAlpha == m_bHasAlpha )
            {
                TCHAR* strTexture   = m_Material[i].m_strTexture;
                DWORD  dwNumIndices = m_Material[i].m_dwNumIndices;

                if( strTexture[0] )
                    pd3dDevice->SetTexture( 0, D3DTextr_GetSurface( strTexture ) );
                pd3dDevice->SetMaterial( &m_Material[i].m_mtrl );
                pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                                  m_pVertices, m_dwNumVertices,
                                                  pIndices, dwNumIndices, NULL );
            }

            pIndices += m_Material[i].m_dwNumIndices;
        }
    }
    else
    {
        if( m_pChild )
        {
            // Save the old matrix sate
            D3DMATRIX matWorldOld, matWorldNew;
            pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldOld );

            // Concat the frame matrix with the current world matrix
            matWorldNew = m_mat * matWorldOld;
            pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldNew );

            // Render the child nodes
            m_pChild->Render( pd3dDevice, bAlpha );

            // Restore the old matrix state
            pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldOld );
        }
    }

    // Render the remaining sibling nodes
    if( m_pNext )
        m_pNext->Render( pd3dDevice, bAlpha );
}




//-----------------------------------------------------------------------------
// Name: SetMaterialData()
// Desc: Sets the material structure for the mesh
//-----------------------------------------------------------------------------
VOID CD3DFileObject::SetMaterialData( DWORD dwMaterial, D3DMATERIAL7* pmtrl,
                                      TCHAR* strName )
{
    if( dwMaterial < MAX_MATERIAL )
    {
        m_Material[dwMaterial].m_mtrl = *pmtrl;
        lstrcpyn( m_Material[dwMaterial].m_strTexture, strName, MAX_TEXTURE_NAME );

        if( pmtrl->diffuse.a < 1.0f )
            m_bHasAlpha = TRUE;
    }
}




//-----------------------------------------------------------------------------
// Name: AddFace()
// Desc: Adds one or more faces to a material slot in a Mesh. Note: this must
//       be called in order (material 0 first, then 1, ...)
//-----------------------------------------------------------------------------
VOID CD3DFileObject::AddFace( DWORD dwMaterial, DWORD* pFaceData,
                              DWORD dwNumFaces )
{
    // Make sure dwMaterial is in range
    if( dwMaterial >= MAX_MATERIAL)
        return;

    // Update the material count
    if( m_dwNumMaterials < dwMaterial+1 )
        m_dwNumMaterials = dwMaterial+1;

    // add indices to the end
    WORD* pIndices = m_pIndices;
    for( DWORD i=0; i<=dwMaterial; i++ )
        pIndices += m_Material[i].m_dwNumIndices;

    // Assign the indices (build a triangle fan for high-order polygons)
    while( dwNumFaces-- )
    {
        DWORD dwNumVerticesPerFace = *pFaceData++;

        for( DWORD i=2; i<dwNumVerticesPerFace; i++ )
        {
            m_Material[dwMaterial].m_dwNumIndices += 3;
            *pIndices++ = (WORD)pFaceData[0];
            *pIndices++ = (WORD)pFaceData[i-1];
            *pIndices++ = (WORD)pFaceData[i];
        }

        pFaceData += dwNumVerticesPerFace;
    }
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CD3DFileObject::GetMeshGeometry( D3DVERTEX** ppVertices, DWORD* pdwNumVertices,
                                         WORD** ppIndices, DWORD* pdwNumIndices )
{
    if( ppVertices )     *ppVertices     = m_pVertices;
    if( pdwNumVertices ) *pdwNumVertices = m_dwNumVertices;
    if( ppIndices )      *ppIndices      = m_pIndices;
    if( pdwNumIndices )  *pdwNumIndices  = m_dwNumIndices;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CD3DFileObject::ComputeNormals()
{
    D3DVECTOR* pNormals = new D3DVECTOR[m_dwNumVertices];
    ZeroMemory( pNormals, sizeof(D3DVECTOR)*m_dwNumVertices );

    for( DWORD i=0; i<m_dwNumIndices; i+=3 )
    {
        WORD a = m_pIndices[i+0];
        WORD b = m_pIndices[i+1];
        WORD c = m_pIndices[i+2];

        D3DVECTOR* v1 = (D3DVECTOR*)&m_pVertices[a];
        D3DVECTOR* v2 = (D3DVECTOR*)&m_pVertices[b];
        D3DVECTOR* v3 = (D3DVECTOR*)&m_pVertices[c];

        D3DVECTOR n = Normalize( CrossProduct( *v2-*v1, *v3-*v2 ) );

        pNormals[a] += n;
        pNormals[b] += n;
        pNormals[c] += n;
    }

    // Assign the newly computed normals back to the vertices
    for( i=0; i<m_dwNumVertices; i++ )
    {
        // Provide some relief to bogus normals
        if( Magnitude( pNormals[i] ) < 0.1f ) 
            pNormals[i] = D3DVECTOR( 0.0f, 0.0f, 1.0f );
        
        pNormals[i] = Normalize( pNormals[i] );
        m_pVertices[i].nx = pNormals[i].x;
        m_pVertices[i].ny = pNormals[i].y;
        m_pVertices[i].nz = pNormals[i].z;
    }

    delete pNormals;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CD3DFileObject::SetNormals( D3DVECTOR* pNormals )
{
    for( DWORD i=0; i<m_dwNumVertices; i++ )
    {
        m_pVertices[i].nx = pNormals[i].x;
        m_pVertices[i].ny = pNormals[i].y;
        m_pVertices[i].nz = pNormals[i].z;
    }
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CD3DFileObject::SetTextureCoords( FLOAT* pTexCoords )
{
    for( DWORD i=0; i<m_dwNumVertices; i++ )
    {
        m_pVertices[i].tu = pTexCoords[2*i+0];
        m_pVertices[i].tv = pTexCoords[2*i+1];
    }
}




//-----------------------------------------------------------------------------
// Name: ParseXXXX()
// Desc: The following routines implement the DirectX .X file loader.
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT ParseMaterial( LPDIRECTXFILEDATA pFileData, CD3DFileObject* pMesh,
                       DWORD dwMaterial )
{
    // Read data from the file
    LONG  pData;
    DWORD dwSize;
    TCHAR strTexture[128];

    if( FAILED( pFileData->GetData( NULL, &dwSize, (VOID**)&pData ) ) )
        return NULL;

    // Set the material properties for the mesh
    D3DMATERIAL7 mtrl;
    ZeroMemory( &mtrl, sizeof(mtrl) );
    memcpy( &mtrl.diffuse,  (VOID*)(pData+0),  sizeof(FLOAT)*4 );
    memcpy( &mtrl.ambient,  (VOID*)(pData+0),  sizeof(FLOAT)*4 );
    memcpy( &mtrl.power,    (VOID*)(pData+16), sizeof(FLOAT)*1 );
    memcpy( &mtrl.specular, (VOID*)(pData+20), sizeof(FLOAT)*3 );
    memcpy( &mtrl.emissive, (VOID*)(pData+32), sizeof(FLOAT)*3 );
    strTexture[0] = 0;

    LPDIRECTXFILEOBJECT pChildObj;
    if( SUCCEEDED( pFileData->GetNextObject(&pChildObj) ) )
    {
        LPDIRECTXFILEDATA pChildData;

        if( SUCCEEDED( pChildObj->QueryInterface( IID_IDirectXFileData,
                                                    (VOID**)&pChildData) ) )
        {
            const GUID* pguid;
            pChildData->GetType( &pguid );

            if( TID_D3DRMTextureFilename == *pguid )
            {
                TCHAR** string;

                if( FAILED( pChildData->GetData( NULL, &dwSize, (VOID**)&string ) ) )
                    return NULL;

                D3DTextr_CreateTextureFromFile( *string );
                lstrcpyn( strTexture, *string, 128 );
            }

            pChildData->Release();
        }

        pChildObj->Release();
    }

    pMesh->SetMaterialData( dwMaterial, &mtrl, strTexture );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT ParseMeshMaterialList( LPDIRECTXFILEDATA pFileData,
                               CD3DFileObject* pMesh )
{
    LPDIRECTXFILEOBJECT        pChildObj;
    LPDIRECTXFILEDATA          pChildData;
    LPDIRECTXFILEDATAREFERENCE pChildDataRef;
    DWORD                      dwMaterial = 0;

    while( SUCCEEDED( pFileData->GetNextObject( &pChildObj ) ) )
    {
        if( SUCCEEDED( pChildObj->QueryInterface( IID_IDirectXFileData,
                                                    (VOID**)&pChildData) ) )
        {
            const GUID* pguid;
            pChildData->GetType( &pguid );

            if( TID_D3DRMMaterial == *pguid )
            {
                ParseMaterial(pChildData, pMesh, dwMaterial++);
            }

            pChildData->Release();
        }

        if( SUCCEEDED( pChildObj->QueryInterface( IID_IDirectXFileDataReference,
                                                    (VOID**)&pChildDataRef) ) )
        {
            if( SUCCEEDED( pChildDataRef->Resolve( &pChildData ) ) )
            {
                const GUID* pguid;
                pChildData->GetType( &pguid );

                if( TID_D3DRMMaterial == *pguid )
                {
                    ParseMaterial( pChildData, pMesh, dwMaterial++ );
                }

                pChildData->Release();
            }
            pChildDataRef->Release();
        }

        pChildObj->Release();
    }
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT ParseMesh( LPDIRECTXFILEDATA pFileData, CD3DFileObject* pParentFrame )
{
    DWORD dwNameLen=80;
    TCHAR strName[80];
    if( FAILED( pFileData->GetName( strName, &dwNameLen ) ) )
        return E_FAIL;

    // Read the Mesh data from the file
    LONG  pData;
    DWORD dwSize;
    if( FAILED( pFileData->GetData( NULL, &dwSize, (VOID**)&pData ) ) )
        return E_FAIL;

    DWORD      dwNumVertices =    *((DWORD*)pData); pData += 4;
    D3DVECTOR* pVertices     = ((D3DVECTOR*)pData); pData += 12*dwNumVertices;
    DWORD      dwNumFaces    =    *((DWORD*)pData); pData += 4;
    DWORD*     pFaceData     =      (DWORD*)pData;

    // Create the Mesh object
    CD3DFileObject* pMesh = new CD3DFileObject( strName );
    pMesh->SetMeshGeometry( pVertices, dwNumVertices, pFaceData, dwNumFaces );

    BOOL bHasNormals = FALSE;
    BOOL bHasMaterials = FALSE;

    // Enumerate child objects.
    LPDIRECTXFILEOBJECT pChildObj;
    while( SUCCEEDED( pFileData->GetNextObject( &pChildObj ) ) )
    {
        LPDIRECTXFILEDATA pChildData;

        if( SUCCEEDED( pChildObj->QueryInterface( IID_IDirectXFileData,
                                                  (VOID**)&pChildData ) ) )
        {
            const GUID* pGUID;
            LONG        pData;
            DWORD       dwSize;

            pChildData->GetType( &pGUID );
            if( FAILED( pChildData->GetData( NULL, &dwSize, (VOID**)&pData ) ) )
            {
                delete pMesh;
                return NULL;
            }

            if( TID_D3DRMMeshMaterialList == *pGUID )
            {
                DWORD  dwNumMaterials = *((DWORD*)pData);   pData += 4;
                DWORD  dwNumMatFaces  = *((DWORD*)pData);   pData += 4;
                DWORD* pMatFace       =   (DWORD*)pData;

                if( dwNumMaterials == 1 || dwNumMatFaces != dwNumFaces )
                {
                    // Only one material add all faces at once
                    pMesh->AddFace( 0, pFaceData, dwNumFaces );
                }
                else
                {
                    // Multiple materials, add in sorted order
                    for( DWORD mat=0; mat<dwNumMaterials; mat++ )
                    {
                        for( DWORD face=0; face<dwNumMatFaces; face++ )
                        {
                            if( pMatFace[face] == mat )
                                pMesh->AddFace( mat, GetFace( pFaceData, face ), 1 );
                        }
                    }
                }

                ParseMeshMaterialList( pChildData, pMesh );
                bHasMaterials = TRUE;
            }

            if( TID_D3DRMMeshNormals == *pGUID )
            {
                DWORD      dwNumNormals = *((DWORD*)pData);
                D3DVECTOR* pNormals     = (D3DVECTOR*)(pData+4);

                if( dwNumNormals == dwNumVertices )
                {
                    pMesh->SetNormals( pNormals );
                    bHasNormals = TRUE;
                }
            }

            if( TID_D3DRMMeshTextureCoords == *pGUID )
            {
                // Copy the texture coords into the mesh's vertices
                DWORD  dwNumTexCoords = *((DWORD*)pData);
                FLOAT* pTexCoords     = (FLOAT*)(pData+4);

                if( dwNumTexCoords == dwNumVertices )
                    pMesh->SetTextureCoords( pTexCoords );
            }

            pChildData->Release();
        }

        pChildObj->Release();
    }

    if( FALSE == bHasMaterials )
        pMesh->AddFace( 0, pFaceData, dwNumFaces );

    if( FALSE == bHasNormals )
        pMesh->ComputeNormals();

    pParentFrame->AddChild( pMesh );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT ParseFrame( LPDIRECTXFILEDATA pFileData, CD3DFileObject* pParentFrame )
{
    DWORD dwNameLen=80;
    TCHAR strName[80];
    if( FAILED( pFileData->GetName( strName, &dwNameLen ) ) )
        return E_FAIL;

    CD3DFileObject* pFrame = new CD3DFileObject( strName );

    // Enumerate child objects.
    LPDIRECTXFILEOBJECT pChildObj;
    while( SUCCEEDED( pFileData->GetNextObject( &pChildObj ) ) )
    {
        LPDIRECTXFILEDATA pChildData;
        if( SUCCEEDED( pChildObj->QueryInterface( IID_IDirectXFileData,
                            (VOID**)&pChildData ) ) )
        {
            const GUID* pGUID;
            pChildData->GetType( &pGUID );

            if( TID_D3DRMFrame == *pGUID )
                ParseFrame( pChildData, pFrame );

            if( TID_D3DRMMesh == *pGUID )
                ParseMesh( pChildData, pFrame );

            if( TID_D3DRMFrameTransformMatrix == *pGUID )
            {
                DWORD dwSize;
                VOID* pData;
                if( FAILED( pChildData->GetData( NULL, &dwSize, &pData ) ) )
                {
                    delete pFrame;
                    return NULL;
                }

                if( dwSize == sizeof(D3DMATRIX) )
                {
                    // Convert from a left- to a right-handed cordinate system
                    D3DMATRIX* pmatFrame = (D3DMATRIX*)pData;
                    pmatFrame->_13 *= -1.0f;
                    pmatFrame->_31 *= -1.0f;
                    pmatFrame->_23 *= -1.0f;
                    pmatFrame->_32 *= -1.0f;
                    pmatFrame->_43 *= -1.0f;
                    pFrame->SetMatrix( pmatFrame );
                }
            }

            pChildData->Release();
        }

        pChildObj->Release();
    }

    pParentFrame->AddChild( pFrame );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CD3DFile()
// Desc: Class constructor
//-----------------------------------------------------------------------------
CD3DFile::CD3DFile()
{
    m_pRoot = NULL;
}




//-----------------------------------------------------------------------------
// Name: ~CD3DFile()
// Desc: Class destructor
//-----------------------------------------------------------------------------
CD3DFile::~CD3DFile()
{
    SAFE_DELETE( m_pRoot );
}




//-----------------------------------------------------------------------------
// Name: Load()
// Desc: Loads a .X geometry file, and creates a hierarchy of frames and meshes
//       to represent the geometry in that file.
//-----------------------------------------------------------------------------
HRESULT CD3DFile::Load( TCHAR* strFilename )
{
    HRESULT                 hr;
    LPDIRECTXFILE           pDXFile;
    LPDIRECTXFILEENUMOBJECT pEnumObj;
    LPDIRECTXFILEDATA       pFileData;
    const GUID*             pGUID;
    CD3DFileObject*         pFrame = NULL;

    // Cleanup any existing object
    SAFE_DELETE( m_pRoot );

    // Create the file object, and register the D3DRM templates for .X files
    if( FAILED( DirectXFileCreate( &pDXFile ) ) )
        return E_FAIL;
    if( FAILED( pDXFile->RegisterTemplates( (VOID*)D3DRM_XTEMPLATES,
                                            D3DRM_XTEMPLATE_BYTES ) ) )
    {
        pDXFile->Release();
        return E_FAIL;
    }

    // Create an enumerator object, to enumerate through the .X file objects.
    // This will open the file in the current directory.
    hr = pDXFile->CreateEnumObject( strFilename, DXFILELOAD_FROMFILE, &pEnumObj );

    if( FAILED(hr) )
    {
        TCHAR strPath[512] = _T("");
        lstrcat( strPath, D3DUtil_GetDXSDKMediaPath() );
        lstrcat( strPath, strFilename );
        
        hr = pDXFile->CreateEnumObject( strPath, DXFILELOAD_FROMFILE,
                                        &pEnumObj );
        if( FAILED(hr) )
        {
            pDXFile->Release();
            return hr;
        }
    }

    // Create a root object for the X file object
    m_pRoot = new CD3DFileObject( _T("D3DFile_Root") );

    // Cycle through each object. Parse meshes and frames as appropriate
    while( SUCCEEDED( hr = pEnumObj->GetNextDataObject( &pFileData ) ) )
    {
        pFileData->GetType( &pGUID );

        if( *pGUID == TID_D3DRMFrame )
            ParseFrame( pFileData, m_pRoot );

        if( *pGUID == TID_D3DRMMesh )
            ParseMesh( pFileData, m_pRoot );

        pFileData->Release();
    }

    // Success will result in hr == DXFILEERR_NOMOREOBJECTS
    if( DXFILEERR_NOMOREOBJECTS == hr ) 
        hr = S_OK;
    else
        SAFE_DELETE( m_pRoot );

    pEnumObj->Release();
    pDXFile->Release();

    return hr;
}




//-----------------------------------------------------------------------------
// Name: GetMeshVertices()
// Desc: Traverse the hierarchy of frames and meshes that make up the file
//       object, and retrieves the vertices for the specified mesh.
//-----------------------------------------------------------------------------
HRESULT CD3DFile::GetMeshVertices( TCHAR* strName, D3DVERTEX** ppVertices,
                                   DWORD* pdwNumVertices )
{
    CD3DFileObject* pObject = FindObject( strName );
    if( pObject )
        return pObject->GetMeshGeometry( ppVertices, pdwNumVertices, NULL, NULL );

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: GetMeshVertices()
// Desc: Traverse the hierarchy of frames and meshes that make up the file
//       object, and retrieves the vertices for the specified mesh.
//-----------------------------------------------------------------------------
HRESULT CD3DFile::GetMeshIndices( TCHAR* strName, WORD** ppIndices,
                                  DWORD* pdwNumIndices )
{
    CD3DFileObject* pObject = FindObject( strName );
    if( pObject )
        return pObject->GetMeshGeometry( NULL, NULL, ppIndices, pdwNumIndices );

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: EnumObjects()
// Desc: Enumerates all objects in the file.
//-----------------------------------------------------------------------------
BOOL CD3DFileObject::EnumObjects( BOOL (*fnCallback)(CD3DFileObject*,D3DMATRIX*,VOID*),
                                  D3DMATRIX* pmat, VOID* pContext )
{
    if( fnCallback( this, pmat, pContext ) == TRUE )
        return TRUE;

    if( m_pChild )
    {
        // Concat matrix set
        D3DMATRIX matSave = (*pmat);
        (*pmat) = (*pmat) * m_mat;
    
        if( m_pChild->EnumObjects( fnCallback, pmat, pContext ) == TRUE )
            return TRUE;

        // Restore matrix set
        (*pmat) = matSave;
    }

    if( m_pNext )
        if( m_pNext->EnumObjects( fnCallback, pmat, pContext ) == TRUE )
            return TRUE;

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: EnumObjects()
// Desc: Enumerates all objects in the file.
//-----------------------------------------------------------------------------
VOID CD3DFile::EnumObjects( BOOL (*fnCallback)(CD3DFileObject*,D3DMATRIX*,VOID*),
                            D3DMATRIX* pmat, VOID* pContext )
{
    if( m_pRoot )
    {
        D3DMATRIX mat;

        if( pmat )
            mat = *pmat;
        else
            D3DUtil_SetIdentityMatrix( mat );

        m_pRoot->EnumObjects( fnCallback, &mat, pContext );
    }
}




//-----------------------------------------------------------------------------
// Name: ScaleMeshCB()
// Desc: Callback to scale a mesh
//-----------------------------------------------------------------------------
BOOL ScaleMeshCB( CD3DFileObject* pFileObject, D3DMATRIX*, VOID* pContext )
{
    D3DVERTEX* pVertices;
    DWORD      dwNumVertices;

    if( SUCCEEDED( pFileObject->GetMeshGeometry( &pVertices, &dwNumVertices,
                                                 NULL, NULL ) ) )
    {
        for( DWORD i=0; i<dwNumVertices; i++ )
        {
            pVertices[i].x *= (*((FLOAT*)pContext));
            pVertices[i].y *= (*((FLOAT*)pContext));
            pVertices[i].z *= (*((FLOAT*)pContext));
        }
    }

    // Keep enumerating
    return FALSE;
}



//-----------------------------------------------------------------------------
// Name: FindMeshCB()
// Desc: Callback to scale a mesh
//-----------------------------------------------------------------------------
BOOL FindMeshCB( CD3DFileObject* pFileObject, D3DMATRIX*, VOID* pContext )
{
    struct FINDMESHRECORD
    {
        TCHAR*          strName;
        CD3DFileObject* pObject;
    };

    FINDMESHRECORD* data = (FINDMESHRECORD*)pContext;
    
    if( 0 == lstrcmpi( data->strName, pFileObject->GetName() ) )
    {
        data->pObject = pFileObject;
        return TRUE;
    }

    // Keep enumerating
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: Scale()
// Desc: Scales all meshes in the file
//-----------------------------------------------------------------------------
VOID CD3DFile::Scale( FLOAT fScale )
{
    EnumObjects( ScaleMeshCB, NULL, (VOID*)&fScale );
}




//-----------------------------------------------------------------------------
// Name: FindObject()
// Desc: Searches all meshes in file object and returns named mesh
//-----------------------------------------------------------------------------
CD3DFileObject* CD3DFile::FindObject( TCHAR* strName )
{
    if( NULL == strName )
        return m_pRoot;

    struct FINDMESHRECORD
    {
        TCHAR*          strName;
        CD3DFileObject* pObject;
    };

    FINDMESHRECORD data = { strName, NULL };

    EnumObjects( FindMeshCB, NULL, (VOID*)&data );
    return data.pObject;
}




//-----------------------------------------------------------------------------
// Name: Render()
// Desc: Renders the hierarchy of frames and meshes that make up the file
//       object
//-----------------------------------------------------------------------------
HRESULT CD3DFile::Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    LPDIRECTDRAWSURFACE7 pddsSavedTexture;
    D3DMATRIX    matSaved;
    D3DMATERIAL7 mtrlSaved;
    DWORD        dwAlphaState, dwSrcBlendState, dwDestBlendState;

    if( m_pRoot )
    {
        // State render states that will be overwritten
        pd3dDevice->GetMaterial( &mtrlSaved );
        pd3dDevice->GetTexture( 0, &pddsSavedTexture );
        pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matSaved );
        pd3dDevice->GetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, &dwAlphaState );
        pd3dDevice->GetRenderState( D3DRENDERSTATE_SRCBLEND,  &dwSrcBlendState );
        pd3dDevice->GetRenderState( D3DRENDERSTATE_DESTBLEND, &dwDestBlendState );

        // Render the opaque file object's hierarchy of frames and meshes
        m_pRoot->Render( pd3dDevice, FALSE );

        // Render the transparent file object's hierarchy of frames and meshes
        pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );
        m_pRoot->Render( pd3dDevice, TRUE );

        // Restore the render states
        pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, dwAlphaState );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  dwSrcBlendState );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, dwDestBlendState );
        pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matSaved );
        pd3dDevice->SetTexture( 0, pddsSavedTexture );
        pd3dDevice->SetMaterial( &mtrlSaved );

        // Keep the ref count of the texture consistent
        if( pddsSavedTexture )
            pddsSavedTexture->Release();
    }

    return S_OK;
}




