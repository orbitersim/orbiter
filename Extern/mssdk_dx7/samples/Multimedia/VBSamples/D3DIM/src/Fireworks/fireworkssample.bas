Attribute VB_Name = "FireWorksSample"
'-----------------------------------------------------------------------------
' File: FireWorks.bas
'
' Desc: Example code showing how to use particles to simulate a fireworks
'       explosion.
'
'
' Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
'-----------------------------------------------------------------------------
Const NUM_PARTICLES = 100
Const RAND_MAX = &H7FFF
'-----------------------------------------------------------------------------
' Name: Particle
' Desc: Data structure for each particle
'-----------------------------------------------------------------------------
Public Type Particle
    vPosition As D3DVECTOR
    vLaunchVelocity As D3DVECTOR
    vInitialPosition As D3DVECTOR
    vInitialVelocity As D3DVECTOR
    r As Single
    g As Single
    b As Single
    fLifeTime As Single
    fMaturity As Single
    wType As Long
    fSize As Single
End Type

Dim m_Particle(NUM_PARTICLES - 1) As Particle
Dim m_Mesh(0 To 3) As D3DVERTEX
Dim m_Background(0 To 3) As D3DTLVERTEX
Dim m_fStartTimeKey As Single
Dim TexturePool As New Collection
Dim Texture1 As DirectDrawSurface7
Dim Texture2 As DirectDrawSurface7
    
'-----------------------------------------------------------------------------
' Name: SetParticle()
' Desc: Helper function to initialize a particle
'-----------------------------------------------------------------------------
Public Function SetParticle(wType As Integer, fLifeTime As Single, _
    vBasePosition As D3DVECTOR, vBaseVelocity As D3DVECTOR) As Particle
    Dim V3D As D3DVECTOR
    V3D = RVector(RND1, RND1, RND1)
    DX.VectorNormalize V3D
    With SetParticle
        .vInitialVelocity = VScale(V3D, 15)
        DX.VectorAdd .vInitialVelocity, .vInitialVelocity, vBaseVelocity
        DX.VectorAdd .vInitialVelocity, .vInitialVelocity, RVector(RND1, RND1, RND1)
        .vInitialPosition = vBasePosition
        .vLaunchVelocity = vBaseVelocity
        .r = 1
        .g = 1
        .b = 1
        .fLifeTime = fLifeTime + fLifeTime * RND1 / 2
        .fMaturity = 0
        .fSize = 0.2
        .wType = wType
    End With
End Function

'-----------------------------------------------------------------------------
' Name: Main()
' Desc: Core of the sample.
'-----------------------------------------------------------------------------

Sub Main()
    
    m_bAppUseZBuffer = False
    m_bShowStats = True
    m_fnConfirmDevice = ConfirmDevice
    m_fStartTimeKey = 0#
  
    InitDX
    InitDeviceObjects
    OneTimeSceneInit
    Do Until MustExit		
	on local error resume next
        FrameMove Timer
        Render
        DoEvents
    Loop
    IM7Terminate
End Sub

'-----------------------------------------------------------------------------
' Name: OneTimeSceneInit()
' Desc: Called during initial app startup, this subroutine performs all the
'       permanent initialization.
'-----------------------------------------------------------------------------
Sub OneTimeSceneInit()
    
'   Initialize the array of particles
    
    Dim i As Long
    For i = 0 To NUM_PARTICLES - 1
        m_Particle(i) = SetParticle((i Mod 3), 4, RVector(0, 0, 0), _
                                    RVector(0, 30, 0))
    Next

'   Initializes vertices used to render the particles

    Dim vNorm As D3DVECTOR: vNorm = RVector(0, 0, -1) '1
    m_Mesh(0) = RVertex(RVector(-1, -1, 1), vNorm, 0, 1)
    m_Mesh(1) = RVertex(RVector(-1, 1, 1), vNorm, 0, 0)
    m_Mesh(2) = RVertex(RVector(1, -1, 1), vNorm, 1, 1)
    m_Mesh(3) = RVertex(RVector(1, 1, 1), vNorm, 1, 0)

    Dim desc As DDSURFACEDESC2
    FireWorksForm.IMCanvas1.backSurface.GetSurfaceDesc desc
    
'   Initializes vertices used to render the background
    
    With FireWorksForm.IMCanvas1
        DX.CreateD3DTLVertex 0, desc.lHeight, 0.99, 0.5, &HFFFFFFFF, 0, 0, 1, m_Background(0)
        DX.CreateD3DTLVertex 0, 0, 0.99, 0.5, &HFFFFFFFF, 0, 0, 0, m_Background(1)
        DX.CreateD3DTLVertex desc.lWidth, desc.lHeight, 0.99, 0.5, &HFFFFFFFF, 0, 1, 1, m_Background(2)
        DX.CreateD3DTLVertex desc.lWidth, 0, 0.99, 0.5, &HFFFFFFFF, 0, 1, 0, m_Background(3)
    End With
    
'   Load in textures
    FindMediaDir "lake.bmp"
    Set Texture1 = FireWorksForm.IMCanvas1.CreateTextureSurface("lake.bmp", 0, 0, 0)
    Set Texture2 = FireWorksForm.IMCanvas1.CreateTextureSurface("firework.bmp", 0, 0, 0)
    
End Sub

'-----------------------------------------------------------------------------
' Name: FrameMove()
' Desc: Called once per frame, the call is the entry point for animating
'       the scene.
'-----------------------------------------------------------------------------
Private Function FrameMove(fTimeKey As Single)

    Dim a0 As D3DVECTOR: a0 = RVector(0, -9.8, 0)
    Dim t As Single: t = fTimeKey - m_fStartTimeKey
    Dim k As Single: k = 1.8
    Dim dwNumOldParticles As Long
    Dim v0 As D3DVECTOR
    Dim r0 As D3DVECTOR
    Dim fDamping As Single
    Dim st As Single
    Dim vLaunchVelocity As D3DVECTOR

'   Store the particles positions

    fDamping = (1 - Exp(-k * t)) / (k * k)
    
    For i = 0 To NUM_PARTICLES - 1
        If t < 0 Then ' Particle is in "launch" mode
            
            v0 = m_Particle(i).vLaunchVelocity
            r0 = m_Particle(i).vInitialPosition
            
            m_Particle(i).vPosition = VAdd(r0, VScale(VScale(v0, t - RND1 / 10), 1.5))
        
        Else ' Particle is in "explode" mode

            v0 = m_Particle(i).vInitialVelocity
            r0 = m_Particle(i).vInitialPosition
            
            m_Particle(i).vPosition = VAdd(r0, VAdd(VScale(VScale(a0, t), k), VScale(VAdd(VScale(v0, k), a0), fDamping)))
            m_Particle(i).fMaturity = t / m_Particle(i).fLifeTime
            st = m_Particle(i).fMaturity + 0.5
            m_Particle(i).r = Exp(-0.5 * st * st)
            m_Particle(i).g = Exp(-1.8 * st * st)
            m_Particle(i).b = Exp(-2# * st * st)
            m_Particle(i).fSize = Exp(-1# * st * st)
            If m_Particle(i).fMaturity > 1 Then
                dwNumOldParticles = dwNumOldParticles + 1
            End If
        End If
    Next
    If NUM_PARTICLES = dwNumOldParticles Then
        m_fStartTimeKey = fTimeKey + 1#
        vLaunchVelocity = RVector(40 * Rnd, 30, 0)
        For i = 0 To NUM_PARTICLES - 1
            m_Particle(i) = SetParticle((i Mod 3), 4#, RVector(0, 0, 0), _
                                        vLaunchVelocity)
        Next
    End If
End Function


'-----------------------------------------------------------------------------
' Name: Render()
' Desc: Called once per frame, the call is the entry point for 3d
'       rendering. This subroutine sets up render states, clears the
'       viewport, and renders the scene.
'-----------------------------------------------------------------------------
Private Sub Render()

    Dim fRestore As Boolean
    
    fRestore = False
    While FireWorksForm.IMCanvas1.DirectDraw.TestCooperativeLevel <> DD_OK
        DoEvents
        fRestore = True
    Wend
    If fRestore Then
        FireWorksForm.IMCanvas1.DirectDraw.RestoreAllSurfaces
        OneTimeSceneInit
    End If
'   Clear the backbuffer
    FireWorksForm.IMCanvas1.ClearBackSurface
    
'   Begin the scene
    d3ddev.BeginScene
    
'   Draw the background
    d3ddev.SetTexture 0, Texture1 'TexturePool.Item(1) ' GetTexture("c:\images\lake.bmp")
    d3ddev.DrawPrimitive D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, _
                            m_Background(0), 4, 0

'   Render the particles
    RenderParticles

'   End the scene.
    d3ddev.EndScene
    
'   Update the screen
    FireWorksForm.IMCanvas1.Update
    DoEvents
End Sub

'-----------------------------------------------------------------------------
' Name: RenderParticles()
' Desc: Draws the system of particles
'-----------------------------------------------------------------------------
Private Sub RenderParticles()

    Dim mtrl As D3DMATERIAL7
    Dim i As Long
    Dim matWorld As D3DMATRIX
    Dim matWorld2 As D3DMATRIX
    
'    Turn on alpha blending for the particles

    d3ddev.SetTexture 0, Texture2
    d3ddev.SetRenderState D3DRENDERSTATE_ALPHABLENDENABLE, True
    
    For i = 0 To NUM_PARTICLES - 1
    
    LSet matWorld = matWorld2

        If m_Particle(i).fMaturity < 1 Then
            
            mtrl.emissive.r = m_Particle(i).r
            mtrl.emissive.g = m_Particle(i).g
            mtrl.emissive.b = m_Particle(i).b
            
            d3ddev.SetMaterial mtrl
    
    '       Translate and scale the world matrix for each particle
            
            matWorld.rc11 = m_Particle(i).fSize
            matWorld.rc22 = m_Particle(i).fSize
            TranslateMatrix matWorld, m_Particle(i).vPosition
    
    '       Set the new world transform and render the particle
            
            d3ddev.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld
            d3ddev.DrawPrimitive D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, _
                                    m_Mesh(0), 4, D3DDP_DEFAULT
        End If
    Next
    
'   Restore the material and states
    
    mtrl.emissive.r = 1
    mtrl.emissive.g = 1
    mtrl.emissive.b = 1
    d3ddev.SetMaterial mtrl
    
    d3ddev.SetRenderState D3DRENDERSTATE_ALPHABLENDENABLE, False

End Sub

'-----------------------------------------------------------------------------
' Name: InitDeviceObjects()
' Desc: Initialize scene objects
'-----------------------------------------------------------------------------
Public Sub InitDeviceObjects()



          Dim lProp As D3DLIGHT7
          Dim c As D3DCOLORVALUE
          
          With FireWorksForm.IMCanvas1
              Set DDraw = .DirectDraw
              Set DDSFront = .screenSurface
              Set DDSBack = .backSurface
              Set D3D = .Direct3d
              Set d3ddev = .Direct3DDevice
          End With
          
          With DX
              .IdentityMatrix MatWorld1
              .IdentityMatrix matView1
              .IdentityMatrix matProj1
              .ViewMatrix matView1, RVector(0, -20, -32), RVector(0, 0, 400), RVector(0, 1, 0), 0
              .ProjectionMatrix matProj1, 1, 100, 1.57
          End With
  
          d3ddev.SetTransform D3DTRANSFORMSTATE_WORLD, MatWorld1
          d3ddev.SetTransform D3DTRANSFORMSTATE_VIEW, matView1
          d3ddev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj1
  
          With ViewPort1
              .lHeight = FireWorksForm.IMCanvas1.Height
              .lWidth = FireWorksForm.IMCanvas1.Width
              .minz = 0#
              .maxz = 1#
          End With
          DX.GetWindowRect FireWorksForm.hWnd, RWDestRect
  
          With c
              .a = 1
              .r = 1
              .g = 1
              .b = 1
          End With
          With lProp
              .dltType = D3DLIGHT_POINT
              .Ambient = c
              .diffuse = c
              .specular = c
              .position.y = 100
          End With
          d3ddev.SetLight 0, lProp
          d3ddev.LightEnable 0, True
  
          With c
              .a = 1
              .r = 1
              .g = 1
              .b = 1
          End With
  
          Material1.Ambient = c
          Material1.diffuse = c
          Material1.emissive = c
          Material1.power = 1
          d3ddev.SetMaterial Material1

'    Set any appropiate state
    
          d3ddev.SetRenderState D3DRENDERSTATE_AMBIENT, &HFFFFFFFF
          d3ddev.SetRenderState D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE
          d3ddev.SetRenderState D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE
          d3ddev.SetRenderState D3DRENDERSTATE_DITHERENABLE, True
          d3ddev.SetRenderState D3DRENDERSTATE_SPECULARENABLE, False
          d3ddev.SetRenderState D3DRENDERSTATE_ZENABLE, False
          d3ddev.SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
          d3ddev.SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
          d3ddev.SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
          d3ddev.SetTextureStageState 0, D3DTSS_MINFILTER, D3DTFN_LINEAR
          d3ddev.SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR
  
End Sub

Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Dir$(sFile) <> "" Then Exit Sub
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If Dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub

Public Function RND1() As Double
    RND1 = (Rnd - Rnd) / 2
End Function

Public Function RND2() As Double
    RND2 = Rnd
End Function




