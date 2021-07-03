//-----------------------------------------------------------------------------
// File: Boids.cpp
//
// Desc: Example code showing how to do flocking. 
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1996-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <time.h>
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DApp.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define MIN(a,b) ((a<b)?a:b)
#define MAX(a,b) ((a>b)?a:b)
#define rnd()  (((FLOAT)rand() ) / RAND_MAX)

#define NUM_BOIDS        13
#define NUM_OBSTACLES     8
#define OBSTACLE_RADIUS   3.0f
#define NUM_GRID         20
#define GRID_WIDTH      190.0f

const FLOAT InfluenceRadius        = 20.0f;
const FLOAT InfluenceRadiusSquared = InfluenceRadius * InfluenceRadius;
const FLOAT CollisionFraction      = 0.8f;
const FLOAT InvCollisionFraction   = 1.0f/(1.0f-CollisionFraction);
const FLOAT NormalSpeed            = 0.1f;
const FLOAT AngleTweak             = 0.02f;
const FLOAT PitchToSpeedRatio      = 0.002f;




//-----------------------------------------------------------------------------
// Name: struct Boid
// Desc: Structure for holding data for each boid
//-----------------------------------------------------------------------------
struct Boid
{
    D3DVECTOR   vLoc;
    D3DVECTOR   vDir;       // Current direction
    D3DVECTOR   vDeltaPos;  // Change in position from flock centering
    D3DVECTOR   vDeltaDir;  // Change in direction
    WORD        wDeltaCnt;  // Number of boids that influence this vDeltaDir
    FLOAT       fSpeed;
    FLOAT       fYaw, fPitch, fRoll, fDYaw;
    FLOAT       r, g, b;    // Color of the boid
    FLOAT       afDist[NUM_BOIDS];      // Array of boid distances, yuk what a waste
    
    D3DMATRIX   matLocal;
};




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    D3DVERTEX* m_pObstacleVertices;
    WORD*      m_pObstacleIndices;
    DWORD      m_dwNumObstacleVertices;
    DWORD      m_dwNumObstacleIndices;

    D3DVERTEX  m_pvGridVertices[NUM_GRID*NUM_GRID];
    D3DVERTEX  m_pvBoidVertices[16];
    WORD       m_pwBoidIndices[30];

    Boid       m_pBoids[NUM_BOIDS];
    D3DVECTOR  m_vObstacleLocations[NUM_OBSTACLES];
    D3DVECTOR  m_vGoal;
    D3DMATRIX  m_matGrid;

    VOID    UpdateFlock();
    HRESULT CreateSphere( D3DVERTEX** ppVertices, DWORD* pdwNumVertices,
                          WORD** ppIndices, DWORD* pdwNumIndices,
                          FLOAT fRadius, DWORD dwNumRings );

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
    CMyD3DApplication();
};




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point to the program. Initializes everything, and goes into a
//       message-processing loop. Idle time is used to render the scene.
//-----------------------------------------------------------------------------
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
{
    CMyD3DApplication d3dApp;

    if( FAILED( d3dApp.Create( hInst, strCmdLine ) ) )
        return 0;

    return d3dApp.Run();
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication()
// Desc: Application constructor. Sets attributes for the app.
//-----------------------------------------------------------------------------
CMyD3DApplication::CMyD3DApplication()
                  :CD3DApplication()
{
    m_strWindowTitle  = TEXT("Boids: Flocking objects");
    m_bAppUseZBuffer  = TRUE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pObstacleVertices = NULL;
    m_pObstacleIndices  = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Points and normals which make up a boid geometry
    D3DVECTOR p1 = D3DVECTOR( 0.00f, 0.00f, 0.50f );
    D3DVECTOR p2 = D3DVECTOR( 0.50f, 0.00f,-0.50f );
    D3DVECTOR p3 = D3DVECTOR( 0.15f, 0.15f,-0.35f );
    D3DVECTOR p4 = D3DVECTOR(-0.15f, 0.15f,-0.35f );
    D3DVECTOR p5 = D3DVECTOR( 0.15f,-0.15f,-0.35f );
    D3DVECTOR p6 = D3DVECTOR(-0.15f,-0.15f,-0.35f );
    D3DVECTOR p7 = D3DVECTOR(-0.50f, 0.00f,-0.50f );
    D3DVECTOR n1 = Normalize( D3DVECTOR( 0.2f, 1.0f, 0.0f ) );
    D3DVECTOR n2 = Normalize( D3DVECTOR( 0.1f, 1.0f, 0.0f ) );
    D3DVECTOR n3 = Normalize( D3DVECTOR( 0.0f, 1.0f, 0.0f ) );
    D3DVECTOR n4 = Normalize( D3DVECTOR(-0.1f, 1.0f, 0.0f ) );
    D3DVECTOR n5 = Normalize( D3DVECTOR(-0.2f, 1.0f, 0.0f ) );
    D3DVECTOR n6 = Normalize( D3DVECTOR(-0.4f, 0.0f, -1.0f ) );
    D3DVECTOR n7 = Normalize( D3DVECTOR(-0.2f, 0.0f, -1.0f ) );
    D3DVECTOR n8 = Normalize( D3DVECTOR( 0.2f, 0.0f, -1.0f ) );
    D3DVECTOR n9 = Normalize( D3DVECTOR( 0.4f, 0.0f, -1.0f ) );

    // Vertices for the top
    m_pvBoidVertices[ 0] = D3DVERTEX( p1, n1, 0.000f, 0.500f );
    m_pvBoidVertices[ 1] = D3DVERTEX( p2, n2, 0.500f, 1.000f );
    m_pvBoidVertices[ 2] = D3DVERTEX( p3, n3, 0.425f, 0.575f );
    m_pvBoidVertices[ 3] = D3DVERTEX( p4, n4, 0.425f, 0.425f );
    m_pvBoidVertices[ 4] = D3DVERTEX( p7, n5, 0.500f, 0.000f );

    // Vertices for the bottom
    m_pvBoidVertices[ 5] = D3DVERTEX( p1, -n5, 1.000f, 0.500f );
    m_pvBoidVertices[ 6] = D3DVERTEX( p2, -n4, 0.500f, 1.000f );
    m_pvBoidVertices[ 7] = D3DVERTEX( p5, -n3, 0.575f, 0.575f );
    m_pvBoidVertices[ 8] = D3DVERTEX( p6, -n2, 0.575f, 0.425f );
    m_pvBoidVertices[ 9] = D3DVERTEX( p7, -n1, 0.500f, 0.000f );

    // Vertices for the  rear
    m_pvBoidVertices[10] = D3DVERTEX( p2, n6, 0.500f, 1.000f );
    m_pvBoidVertices[11] = D3DVERTEX( p3, n7, 0.425f, 0.575f );
    m_pvBoidVertices[12] = D3DVERTEX( p4, n8, 0.425f, 0.425f );
    m_pvBoidVertices[13] = D3DVERTEX( p7, n9, 0.500f, 0.000f );
    m_pvBoidVertices[14] = D3DVERTEX( p6, n8, 0.575f, 0.425f );
    m_pvBoidVertices[15] = D3DVERTEX( p5, n7, 0.575f, 0.575f );

    // Vertex inidices for the boid
    m_pwBoidIndices[ 0] =  0; m_pwBoidIndices[ 1] =  1; m_pwBoidIndices[ 2] =  2;
    m_pwBoidIndices[ 3] =  0; m_pwBoidIndices[ 4] =  2; m_pwBoidIndices[ 5] =  3;
    m_pwBoidIndices[ 6] =  0; m_pwBoidIndices[ 7] =  3; m_pwBoidIndices[ 8] =  4;
    m_pwBoidIndices[ 9] =  5; m_pwBoidIndices[10] =  7; m_pwBoidIndices[11] =  6;
    m_pwBoidIndices[12] =  5; m_pwBoidIndices[13] =  8; m_pwBoidIndices[14] =  7;
    m_pwBoidIndices[15] =  5; m_pwBoidIndices[16] =  9; m_pwBoidIndices[17] =  8;
    m_pwBoidIndices[18] = 10; m_pwBoidIndices[19] = 15; m_pwBoidIndices[20] = 11;
    m_pwBoidIndices[21] = 11; m_pwBoidIndices[22] = 15; m_pwBoidIndices[23] = 12;
    m_pwBoidIndices[24] = 12; m_pwBoidIndices[25] = 15; m_pwBoidIndices[26] = 14;
    m_pwBoidIndices[27] = 12; m_pwBoidIndices[28] = 14; m_pwBoidIndices[29] = 13;

    // seed the random number generator
    srand(time(NULL));

    m_vGoal = D3DVECTOR(0.0f, 0.0f, 0.0f);

    for( WORD i=0; i<NUM_BOIDS; i++ )
    {
        m_pBoids[i].vLoc   = D3DVECTOR(100.0f*(rnd()-rnd()), 10.0f*rnd(), 100.0f*(rnd()-rnd()));
        m_pBoids[i].vDir   = Normalize(D3DVECTOR(rnd()-rnd(), rnd()-rnd(), rnd()-rnd()));
        m_pBoids[i].fYaw   = 0.0f;
        m_pBoids[i].fPitch = 0.0f;
        m_pBoids[i].fRoll  = 0.0f;
        m_pBoids[i].fDYaw  = 0.0f;
        m_pBoids[i].fSpeed = 0.1f;
        m_pBoids[i].r      = rnd();
        m_pBoids[i].g      = rnd();
        m_pBoids[i].b      = rnd();

        FLOAT fMin = MIN( m_pBoids[i].r, MIN( m_pBoids[i].g, m_pBoids[i].b ) );
        FLOAT fMax = MAX( m_pBoids[i].r, MIN( m_pBoids[i].g, m_pBoids[i].b ) );

        m_pBoids[i].r = (m_pBoids[i].r - fMin) / (fMax-fMin);
        m_pBoids[i].g = (m_pBoids[i].g - fMin) / (fMax-fMin);
        m_pBoids[i].b = (m_pBoids[i].b - fMin) / (fMax-fMin);
    }

    // Position the obstacles
    m_vObstacleLocations[0] = D3DVECTOR( 100.0f, 10.0f,    0.0f );
    m_vObstacleLocations[1] = D3DVECTOR(-100.0f, 10.0f,    0.0f );
    m_vObstacleLocations[2] = D3DVECTOR(   0.0f, 10.0f,  100.0f );
    m_vObstacleLocations[3] = D3DVECTOR(   0.0f, 10.0f, -100.0f );
    for( i=4; i<NUM_OBSTACLES; i++ )
        m_vObstacleLocations[i] = D3DVECTOR( 100*(rnd()-rnd()), 10*rnd(), 100*(rnd()-rnd()) );

    FLOAT fSize   = GRID_WIDTH/(NUM_GRID-1.0f);
    FLOAT fOffset = GRID_WIDTH/2.0f;
    
    for( i=0; i<NUM_GRID; i++ )
    {
        for( WORD j=0; j<NUM_GRID; j++ )
        {
            m_pvGridVertices[j+i*NUM_GRID] = D3DVERTEX( 
                          D3DVECTOR( i*fSize-fOffset, 0.0f, j*fSize-fOffset ),
                          D3DVECTOR( 0.0f, 1.0f, 0.0f ), 0.0f, 0.0f );
        }
    }

    // Generate the sphere data
    CreateSphere( &m_pObstacleVertices, &m_dwNumObstacleVertices, 
                  &m_pObstacleIndices, &m_dwNumObstacleIndices, 1.0f, 8 );
 
    // Create some textures
    D3DTextr_CreateTextureFromFile( "earth.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateFlock()
// Desc: Update posiiton of each boid in flock
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::UpdateFlock()
{
    FLOAT fDist;

    // First update the dist array 0.0..1.0 with 0.0 being furthest away
    for( WORD i=0; i<NUM_BOIDS; i++ )
    {
        for( WORD j=i+1; j<NUM_BOIDS; j++ )
        {
            fDist = SquareMagnitude( m_pBoids[i].vLoc - m_pBoids[j].vLoc );
            fDist = InfluenceRadiusSquared - fDist;
            if( fDist < 0.0f )
                fDist = 0.0f;
            else
                fDist /= InfluenceRadiusSquared;

            m_pBoids[i].afDist[j] = m_pBoids[j].afDist[i] = fDist;
        }
        m_pBoids[i].afDist[i] = 0.0f;
        m_pBoids[i].vDeltaDir = D3DVECTOR( 0.0f, 0.0f, 0.0f );
        m_pBoids[i].vDeltaPos = D3DVECTOR( 0.0f, 0.0f, 0.0f );
        m_pBoids[i].wDeltaCnt = 0;
    }

    for( i=0; i<NUM_BOIDS; i++ )
    {
        for( WORD j=i+1; j<NUM_BOIDS; j++ )
        {
            // If i is near j have them influence each other
            if( m_pBoids[i].afDist[j] > 0.0f )
            {
                D3DVECTOR   vDiff = Normalize( m_pBoids[i].vLoc - m_pBoids[j].vLoc );
                D3DVECTOR   vDelta;
                FLOAT       fCollWeight = 0.0f;     // Collision weighting

                // Only do collision testing against the nearest ones
                if( m_pBoids[i].afDist[j] - CollisionFraction > 0.0f )
                    fCollWeight = (m_pBoids[i].afDist[j] - CollisionFraction) *
                                  InvCollisionFraction;

                // Add in a little flock centering
                if( m_pBoids[i].afDist[j] - (1.0f-CollisionFraction) > 0.0f )
                    fCollWeight -= m_pBoids[i].afDist[j] * (1.0f-fCollWeight);

                vDelta = fCollWeight * vDiff;

                // Add in the collision avoidance
                m_pBoids[i].vDeltaPos += vDelta;
                m_pBoids[j].vDeltaPos -= vDelta;

                // Add in the velocity influences
                m_pBoids[i].vDeltaDir += m_pBoids[j].vDir * m_pBoids[i].afDist[j];
                m_pBoids[j].vDeltaDir += m_pBoids[i].vDir * m_pBoids[i].afDist[j];
                m_pBoids[i].wDeltaCnt++;
                m_pBoids[j].wDeltaCnt++;
            }
        }
    }

    // Update the boids
    for( i=0; i<NUM_BOIDS; i++ )
    {
        if( m_pBoids[i].wDeltaCnt )
        {
            m_pBoids[i].vDeltaDir /= (FLOAT)m_pBoids[i].wDeltaCnt;
            m_pBoids[i].vDeltaDir -= m_pBoids[i].vDir;
            m_pBoids[i].vDeltaDir *= 1.5f;
        }
        D3DVECTOR vDelta = m_pBoids[i].vDeltaDir + m_pBoids[i].vDeltaPos;
        D3DVECTOR vOffset;

        // Add in the influence of the global goal
        D3DVECTOR vGoal = 0.5f * Normalize(m_vGoal-m_pBoids[i].vLoc );
        vDelta += vGoal;

        // Add in any obstacles
        for( WORD j=0; j<NUM_OBSTACLES; j++ )
        {
            D3DVECTOR vOb = m_pBoids[i].vLoc - m_vObstacleLocations[j];
            FLOAT     fDist = Magnitude(vOb);

            if( fDist > 2*OBSTACLE_RADIUS )
                continue;

            vOb /= fDist;   // Normalize
            fDist = 1.0f - fDist/(2*OBSTACLE_RADIUS);
            vDelta += fDist * vOb * 5.0f;
        }

        // First deal with pitch changes
        if( vDelta.y > 0.01f )
        {   // We're too low
            m_pBoids[i].fPitch += AngleTweak;
            if( m_pBoids[i].fPitch > 0.8f )
                m_pBoids[i].fPitch = 0.8f;
        }
        else if( vDelta.y < -0.01f )
        {   // We're too high
            m_pBoids[i].fPitch -= AngleTweak;
            if( m_pBoids[i].fPitch < -0.8f )
                m_pBoids[i].fPitch = -0.8f;
        } 
        else
        {
            // Add damping
            m_pBoids[i].fPitch *= 0.98f;
        }

        // Speed up or slow down depending on angle of attack
        m_pBoids[i].fSpeed -= m_pBoids[i].fPitch * PitchToSpeedRatio;
        // Damp back to normal
        m_pBoids[i].fSpeed = (m_pBoids[i].fSpeed-NormalSpeed)*0.99f + NormalSpeed;

        if( m_pBoids[i].fSpeed < NormalSpeed/2 )
            m_pBoids[i].fSpeed = NormalSpeed/2;
        if( m_pBoids[i].fSpeed > NormalSpeed*5 )
            m_pBoids[i].fSpeed = NormalSpeed*5;

        // Now figure out yaw changes
        vOffset    = vDelta;
        vOffset.y  = 0.0f;
        vDelta     = m_pBoids[i].vDir;
        vOffset    = Normalize( vOffset );
        FLOAT fDot = DotProduct( vOffset, vDelta );
        
        // Speed up slightly if not turning much
        if( fDot > 0.7f )
        {
            fDot -= 0.7f;
            m_pBoids[i].fSpeed += fDot * 0.005f;
        }
        vOffset = CrossProduct( vOffset, vDelta );
        fDot = (1.0f-fDot)/2.0f * 0.07f;
        if( vOffset.y > 0.05f )
            m_pBoids[i].fDYaw = (m_pBoids[i].fDYaw*19.0f + fDot) * 0.05f;
        else if( vOffset.y < -0.05f )
            m_pBoids[i].fDYaw = (m_pBoids[i].fDYaw*19.0f - fDot) * 0.05f;
        else
            m_pBoids[i].fDYaw *= 0.98f; // damp it

        m_pBoids[i].fYaw += m_pBoids[i].fDYaw;
        m_pBoids[i].fRoll = -m_pBoids[i].fDYaw * 20.0f;
    }
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Set the view and projection matrices. Note: these are static since
    // the view changes each frame
    D3DMATRIX matWorld;
    static D3DVECTOR vEyePt( 0.0f, 30.0f, 100.0f );
    static D3DVECTOR vLookatPt( 0.0f, 0.0f, 50.0f );
    static D3DVECTOR vUpVec( 0.0f, 1.0f, 0.0f );

    SetViewParams( &vEyePt, &vLookatPt, &vUpVec , 0.1f);

    static FLOAT tic = -200.0f * rnd();
    tic += 0.01f;

    // Update grid matrix
    D3DVECTOR vOffset;
    vOffset.x = (FLOAT)floor( vLookatPt.x/20 ) * 20.0f - 10.0f;
    vOffset.y = 0.0f;
    vOffset.z = (FLOAT)floor( vLookatPt.z/20 ) * 20.0f - 10.0f;

    D3DUtil_SetTranslateMatrix( m_matGrid, vOffset );

    UpdateFlock();

    vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    // draw the boids
    for( WORD i=0; i<NUM_BOIDS; i++)
    {
        D3DVECTOR   step;

        // Build the world matrix for the boid. First translate into place, 
        // then set orientation, then scale (if needed)
        D3DUtil_SetTranslateMatrix( matWorld, m_pBoids[i].vLoc );

        D3DMATRIX matTemp, matRotateX, matRotateY, matRotateZ;
        D3DUtil_SetRotateYMatrix( matRotateY, -m_pBoids[i].fYaw );
        D3DUtil_SetRotateXMatrix( matRotateX, -m_pBoids[i].fPitch );
        D3DUtil_SetRotateZMatrix( matRotateZ, -m_pBoids[i].fRoll );
        D3DMath_MatrixMultiply( matTemp, matRotateX, matRotateY );
        D3DMath_MatrixMultiply( matTemp, matRotateZ, matTemp );
        D3DMath_MatrixMultiply( matWorld, matTemp, matWorld );
        
        m_pBoids[i].matLocal = matWorld;

        m_pBoids[i].vDir.x = matWorld(2, 0);
        m_pBoids[i].vDir.y = matWorld(2, 1);
        m_pBoids[i].vDir.z = matWorld(2, 2);

        m_pBoids[i].vLoc += m_pBoids[i].vDir * m_pBoids[i].fSpeed;

        vLookatPt += m_pBoids[i].vLoc;
    }
    vLookatPt /= NUM_BOIDS;
    vEyePt.x = vLookatPt.x + (FLOAT)( 30.0f*sin(tic*0.223) );
    vEyePt.y = vLookatPt.y + (FLOAT)( 21.0f + 20.0f*sin(tic*0.33f) );
    vEyePt.z = vLookatPt.z + (FLOAT)( 30.0f*cos(tic*0.31f) );

    m_vGoal.x = 105.0f * (FLOAT)sin(tic*0.1f);
    m_vGoal.y = 10.0f;
    m_vGoal.z = 105.0f * (FLOAT)cos(tic*0.1f);

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Render()
// Desc: Called once per frame, the call is the entry point for 3d
//       rendering. This function sets up render states, clears the
//       viewport, and renders the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::Render()
{
    D3DMATERIAL7 mtrl;

    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                         0x00000000, 1.0f, 0L );

    // Begin the scene
    if( FAILED( m_pd3dDevice->BeginScene() ) )
        return S_OK; // Don't return a "fatal" error

    m_pd3dDevice->SetTexture( 0, NULL );

    // Draw the north-south lines
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matGrid );
    m_pd3dDevice->DrawPrimitive( D3DPT_LINELIST, D3DFVF_VERTEX,
                                 m_pvGridVertices, NUM_GRID*NUM_GRID, 0 );

    // Draw the east-west lines
    D3DMATRIX matRotateY;
    D3DUtil_SetRotateYMatrix( matRotateY, g_PI/2.0f );
    D3DMath_MatrixMultiply( matRotateY, matRotateY, m_matGrid );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matRotateY );
    m_pd3dDevice->DrawPrimitive( D3DPT_LINELIST, D3DFVF_VERTEX, 
                                 m_pvGridVertices, NUM_GRID*NUM_GRID, 0 );

    // Draw the boids
    for( WORD i=0; i<NUM_BOIDS; i++)
    {
        // Set the color for the boid
        D3DUtil_InitMaterial( mtrl, m_pBoids[i].r, m_pBoids[i].g, m_pBoids[i].b );
        m_pd3dDevice->SetMaterial( &mtrl );

        // Apply the boid's local matrix
        m_pd3dDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, &m_pBoids[i].matLocal );

        // Draw the boid
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                            m_pvBoidVertices, 16,
                                            m_pwBoidIndices, 30, 0 );
    }

    // Draw the obstacles
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("earth.bmp") );
        
    // Scaled matrix is used, so we have to enable normal normalization to
    // compute correct lighting
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_NORMALIZENORMALS, TRUE);
    for( i=0; i<NUM_OBSTACLES; i++ )
    {
        D3DMATRIX matScale, matTrans, matWorld;
        D3DUtil_SetTranslateMatrix( matTrans, m_vObstacleLocations[i] );
        D3DUtil_SetScaleMatrix( matScale, OBSTACLE_RADIUS, OBSTACLE_RADIUS,
                                OBSTACLE_RADIUS );
        D3DMath_MatrixMultiply( matWorld, matScale, matTrans );
        
        m_pd3dDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, &matWorld );

        // Draw the obstacle
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                              m_pObstacleVertices, m_dwNumObstacleVertices,
                              m_pObstacleIndices,  m_dwNumObstacleIndices, 0 );
    }

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_NORMALIZENORMALS, FALSE );
    // End the scene.
    m_pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0x0a0a0a0a );

    // Setup texture states
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );

    // Set the transform matrices
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, 0.75f, fAspect, 1.0f, 1000.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the lights
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;

        if( m_pDeviceInfo->ddDeviceDesc.dwMaxActiveLights > 0 )
        {
            D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.5f, -1.0f, -0.3f );
            m_pd3dDevice->SetLight( 0, &light );
            m_pd3dDevice->LightEnable( 0, TRUE );
        }

        if( m_pDeviceInfo->ddDeviceDesc.dwMaxActiveLights > 1 )
        {
            D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.5f, 1.0f, 0.3f );
            light.dcvDiffuse.r = 0.5f;
            light.dcvDiffuse.g = 0.5f;
            light.dcvDiffuse.b = 0.5f;
            m_pd3dDevice->SetLight( 1, &light );
            m_pd3dDevice->LightEnable( 1, TRUE );
        }

        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pObstacleVertices );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    D3DTextr_InvalidateAllTextures();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateSphere()
// Desc: Sets up the vertices for a sphere.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateSphere( D3DVERTEX** ppVertices,
                                         DWORD* pdwNumVertices,
                                         WORD** ppIndices, DWORD* pdwNumIndices,
                                         FLOAT fRadius, DWORD dwNumRings )
{
    // Allocate memory for the vertices and indices
    DWORD      dwNumVertices = (dwNumRings*(2*dwNumRings+1)+2);
    DWORD      dwNumIndices  = 6*(dwNumRings*2)*((dwNumRings-1)+1);
    D3DVERTEX* pVertices     = new D3DVERTEX[dwNumVertices];
    WORD*      pIndices      = new WORD[dwNumIndices];

    (*ppVertices) = pVertices;
    (*ppIndices)  = pIndices;
    
    // Counters
    WORD x, y, vtx = 0, index = 0;

    // Angle deltas for constructing the sphere's vertices
    FLOAT fDAng   = g_PI / dwNumRings;
    FLOAT fDAngY0 = fDAng;

    // Make the middle of the sphere
    for( y=0; y<dwNumRings; y++ )
    {
        FLOAT y0 = (FLOAT)cos(fDAngY0);
        FLOAT r0 = (FLOAT)sin(fDAngY0);
        FLOAT tv = (1.0f - y0)/2;

        for( x=0; x<(dwNumRings*2)+1; x++ )
        {
            FLOAT fDAngX0 = x*fDAng;
        
            D3DVECTOR v( r0*(FLOAT)sin(fDAngX0), y0, r0*(FLOAT)cos(fDAngX0) );
            FLOAT tu = 1.0f - x/(dwNumRings*2.0f);

            *pVertices++ = D3DVERTEX( fRadius*v, v, tu, tv );
            vtx ++;
        }
        fDAngY0 += fDAng;
    }

    for( y=0; y<dwNumRings-1; y++ )
    {
        for( x=0; x<(dwNumRings*2); x++ )
        {
            *pIndices++ = (WORD)( (y+0)*(dwNumRings*2+1) + (x+0) );
            *pIndices++ = (WORD)( (y+1)*(dwNumRings*2+1) + (x+0) );
            *pIndices++ = (WORD)( (y+0)*(dwNumRings*2+1) + (x+1) );
            *pIndices++ = (WORD)( (y+0)*(dwNumRings*2+1) + (x+1) );
            *pIndices++ = (WORD)( (y+1)*(dwNumRings*2+1) + (x+0) ); 
            *pIndices++ = (WORD)( (y+1)*(dwNumRings*2+1) + (x+1) );
            index += 6;
        }
    }
    // Make top and bottom
    D3DVECTOR vy( 0.0f, 1.0f, 0.0f );
    WORD wNorthVtx = vtx;
    *pVertices++ = D3DVERTEX( fRadius*vy, vy, 0.5f, 0.0f );
    vtx++;
    WORD wSouthVtx = vtx;
    *pVertices++ = D3DVERTEX( -fRadius*vy,-vy, 0.5f, 1.0f );
    vtx++;

    for( x=0; x<(dwNumRings*2); x++ )
    {
        WORD p1 = wSouthVtx;
        WORD p2 = (WORD)( (y)*(dwNumRings*2+1) + (x+1) );
        WORD p3 = (WORD)( (y)*(dwNumRings*2+1) + (x+0) );

        *pIndices++ = p1;
        *pIndices++ = p3;
        *pIndices++ = p2;
        index += 3;
    }

    for( x=0; x<(dwNumRings*2); x++ )
    {
        WORD p1 = wNorthVtx;
        WORD p2 = (WORD)( (0)*(dwNumRings*2+1) + (x+1) );
        WORD p3 = (WORD)( (0)*(dwNumRings*2+1) + (x+0) );

        *pIndices++ = p1;
        *pIndices++ = p3;
        *pIndices++ = p2;
        index += 3;
    }

    (*pdwNumVertices) = vtx;
    (*pdwNumIndices)  = index;

    return S_OK;
}



