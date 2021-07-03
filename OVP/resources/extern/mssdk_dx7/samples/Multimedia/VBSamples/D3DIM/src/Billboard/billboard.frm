VERSION 5.00
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMControl.ocx"
Begin VB.Form Form1 
   Caption         =   "VB D3DIM Billboard"
   ClientHeight    =   4005
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6120
   Icon            =   "billboard.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   267
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   408
   StartUpPosition =   3  'Windows Default
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   3975
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6135
      _ExtentX        =   10821
      _ExtentY        =   7011
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit


Const NUM_DEBRIS = 512     ' Tree, shadow, and point data
Const NUM_TREES = 100
Const MAX_WIDTH = 50

Dim m_bDeviceDoesAlphaTest As Boolean
Dim m_TreeMesh(4) As D3DLVERTEX
Dim m_BackgroundMesh(4) As D3DTLVERTEX
Dim m_Debris(NUM_DEBRIS) As D3DLVERTEX
Dim m_TreePositions(NUM_TREES) As D3DVECTOR
Dim m_TreeLOD(NUM_TREES) As Integer
Dim m_fViewAngle As Single
Dim m_TexClouds As DirectDrawSurface7
Dim m_TexShadow As DirectDrawSurface7
Dim m_TexTrees As DirectDrawSurface7
Dim m_dx As DirectX7

Dim m_light As D3DLIGHT7
Dim m_bUseAlphaTest As Boolean
Dim m_bUseRGB As Boolean


Dim m_lastTime As Single
Dim m_bDontRender As Boolean
Dim m_running As Boolean
Dim m_bInit As Boolean
Dim m_bEndInit As Boolean

Const m_pi = 3.14159265358972


Function RandomPos() As Single
    RandomPos = (Rnd(1) - Rnd(0)) * MAX_WIDTH
End Function


Function RandomColor() As Single
    RandomColor = 1 '(0.5 * Rnd(1) + 0.1)
End Function


Sub InitScene()
    
    Dim d3d As Direct3D7
    Dim d3ddev As Direct3DDevice7
    Set d3d = IMCanvas1.Direct3d
    Set d3ddev = IMCanvas1.Direct3DDevice
    Dim i As Integer
    
    'set random tree positions
    For i = 0 To NUM_TREES - 1
        m_TreePositions(i).x = RandomPos()
        m_TreePositions(i).y = 0
        m_TreePositions(i).z = RandomPos()
    Next
        
    ' Initialize  debris data
    For i = 0 To NUM_DEBRIS - 1
        m_Debris(i).color = &HFF00FF00 'RGBA(RandomColor(), RandomColor(), 0, 1)
        m_Debris(i).x = RandomPos()
        m_Debris(i).y = 0
        m_Debris(i).z = RandomPos()
        m_Debris(i).specular = -1
    Next
      
 
    ' Initialize the tree and background meshes
    m_TreeMesh(0) = RLVertex(-1, 0, 0, -1, 0.5, 0#, 1#)
    m_TreeMesh(1) = RLVertex(-1, 2, 0, -1, 0.5, 0#, 0#)
    m_TreeMesh(2) = RLVertex(1, 0, 0, -1, 0.5, 1#, 1#)
    m_TreeMesh(3) = RLVertex(1, 2, 0, -1, 0.5, 1#, 0#)

    m_BackgroundMesh(0) = RTLVertex(0, 0, 0.99, 0.5, -1, 0, 0, 1)
    m_BackgroundMesh(1) = RTLVertex(0, 0, 0.99, 0.5, -1, 0, 0, 0)
    m_BackgroundMesh(2) = RTLVertex(0, 0, 0.99, 0.5, -1, 0, 1, 1)
    m_BackgroundMesh(3) = RTLVertex(0, 0, 0.99, 0.5, -1, 0, 1, 0)

End Sub
 
Sub InitDeviceObjects()
    
    Set m_dx = IMCanvas1.dx
    
    Dim vp As D3DVIEWPORT7
    Dim d3ddev As Direct3DDevice7
    Dim devdesc As D3DDEVICEDESC7


    
    IMCanvas1.Direct3DDevice.GetViewport vp
    Set d3ddev = IMCanvas1.Direct3DDevice


    d3ddev.GetCaps devdesc
    m_bUseAlphaTest = ((devdesc.dpcTriCaps.lAlphaCmpCaps And D3DPCMPCAPS_GREATEREQUAL) = D3DPCMPCAPS_GREATEREQUAL)

    
    m_BackgroundMesh(0).sy = vp.lHeight
    m_BackgroundMesh(2).sy = vp.lHeight
    m_BackgroundMesh(2).sx = vp.lWidth
    m_BackgroundMesh(3).sx = vp.lWidth
    
    Dim matProj As D3DMATRIX
    Call m_dx.ProjectionMatrix(matProj, 1, 100, 1.57)
    d3ddev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj

    ' restore all surfaces
    IMCanvas1.DirectDraw.RestoreAllSurfaces

    ' Set up the default texture states
    Call d3ddev.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE)
    Call d3ddev.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE)
    Call d3ddev.SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE)
    Call d3ddev.SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1)
    Call d3ddev.SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE)
    Call d3ddev.SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR)
    Call d3ddev.SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR)
    Call d3ddev.SetRenderState(D3DRENDERSTATE_DITHERENABLE, True)

    ' Note: in DX7, setting D3DRENDERSTATE_LIGHTING to FALSE is needed to
    ' turn off vertex lighting (and use the color in the D3DLVERTEX instead.)
    Call d3ddev.SetRenderState(D3DRENDERSTATE_LIGHTING, False)


End Sub



Sub MoveFrame()
    Dim X1 As Single
    Dim Y1 As Single
    Dim z1 As Single
    
    Dim X2 As Single
    Dim Y2 As Single
    Dim z2 As Single
    
    Dim upV As D3DVECTOR
    Dim fromV As D3DVECTOR
    Dim toV As D3DVECTOR
    Dim atV As D3DVECTOR
    Dim tempV As D3DVECTOR
    
    Dim dev As Direct3DDevice7
    Dim matView As D3DMATRIX
    Dim tu As Single
    Dim tv As Single
    Dim newTime As Single
    
    Static fTimeKey As Single
    
    If m_lastTime <> 0 Then
        newTime = Timer
        fTimeKey = fTimeKey + (newTime - m_lastTime) * 0.6
    End If
    m_lastTime = Timer
    
    Set dev = IMCanvas1.Direct3DDevice
    
    ' Move the camera about a large circle through the trees
    m_fViewAngle = ((fTimeKey / 12) - CInt(fTimeKey / 12)) * 2 * m_pi

    X1 = 20# * Cos(m_fViewAngle)
    Y1 = 3#
    z1 = 20# * Sin(m_fViewAngle)

    X2 = 20# * Cos(m_fViewAngle + 0.1)
    Y2 = 3#
    z2 = 20# * Sin(m_fViewAngle + 0.1)

    upV.y = 1
    fromV.x = X1
    fromV.y = Y1
    fromV.z = z1
    toV.x = X2
    toV.y = Y2
    toV.z = z2
    
    Call m_dx.VectorSubtract(tempV, toV, fromV)
    Call m_dx.VectorNormalize(tempV)
    Call m_dx.VectorScale(tempV, tempV, 10)
    Call m_dx.VectorAdd(atV, tempV, fromV)
    
    m_dx.ViewMatrix matView, fromV, atV, upV, 0
    dev.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    
    ' Scroll the background texture
    tu = (fTimeKey / 9) - CInt(fTimeKey / 9)
    m_BackgroundMesh(0).tu = tu
    m_BackgroundMesh(1).tu = tu
    m_BackgroundMesh(2).tu = tu - 1
    m_BackgroundMesh(3).tu = tu - 1


End Sub

Sub DrawBackground()
    Dim d3ddevice As Direct3DDevice7
    Set d3ddevice = IMCanvas1.Direct3DDevice
    
    Call IMCanvas1.Direct3DDevice.SetTexture(0, m_TexClouds)
    Call d3ddevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, _
                               m_BackgroundMesh(0), 4, 0)

End Sub

Sub DrawDebris()

    Dim d3ddevice As Direct3DDevice7
    Dim matIdentity As D3DMATRIX
    m_dx.IdentityMatrix matIdentity

    Set d3ddevice = IMCanvas1.Direct3DDevice
    
    ' Render the debris
    Call d3ddevice.SetTexture(0, Nothing)
    Call d3ddevice.SetTransform(D3DTRANSFORMSTATE_WORLD, matIdentity)
    Call d3ddevice.DrawPrimitive(D3DPT_POINTLIST, D3DFVF_LVERTEX, _
                                 m_Debris(0), NUM_DEBRIS, 0)
End Sub

Sub DrawTreeShadows()
    
    Dim i As Long
    Dim d3ddevice As Direct3DDevice7
    Set d3ddevice = IMCanvas1.Direct3DDevice
    
    Call d3ddevice.SetTexture(0, m_TexShadow)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, 1)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_ZERO)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR)

    ' Enable alpha testing (avoids drawing pixels with less than a certain alpha
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, m_bUseAlphaTest)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHAREF, &H8&)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHAFUNC, D3DCMP_GREATEREQUAL)
     
    ' Rotate the world matrix, to lay the shadow on the ground
    Dim matWorld As D3DMATRIX
    m_dx.RotateXMatrix matWorld, -m_pi / 2
     
    ' Loop through the trees rendering the shadows
    For i = 0 To NUM_TREES - 1

        ' Add Translation to the world matrix
        matWorld.rc41 = m_TreePositions(i).x
        matWorld.rc42 = m_TreePositions(i).y
        matWorld.rc43 = m_TreePositions(i).z
        d3ddevice.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld

        'draw the tree
        Call d3ddevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX, m_TreeMesh(0), 4, D3DDP_DEFAULT)

    Next

    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, False)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, False)

End Sub

Sub DrawTrees()

    Dim i As Long
    Dim d3ddevice As Direct3DDevice7
    Set d3ddevice = IMCanvas1.Direct3DDevice
    
    ' Set state for drawing trees
    Call d3ddevice.SetTexture(0, m_TexTrees)

    
    ' Set diffuse blending for alpha set in vertices.
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, True)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA)

    ' Enable alpha testing (avoids drawing pixels with less than a certain alpha
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, m_bUseAlphaTest)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHAREF, &H8&)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHAFUNC, D3DCMP_GREATEREQUAL)

    ' Set up a rotation matrix to orient the billboard towards the camera.
    ' This is for billboards that are fixed about the Y-axis. See note at top
    ' of file for more info.
    Dim matWorld As D3DMATRIX
    m_dx.RotateYMatrix matWorld, m_fViewAngle

    For i = 0 To NUM_TREES - 1
                
                matWorld.rc41 = m_TreePositions(i).x
                matWorld.rc42 = m_TreePositions(i).y
                matWorld.rc43 = m_TreePositions(i).z
                d3ddevice.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld
                Call d3ddevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, _
                                                  D3DFVF_LVERTEX, m_TreeMesh(0), 4, 0)
    Next
        

    ' Restore state
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, False)
    Call d3ddevice.SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, False)
End Sub



                






Private Sub Form_Load()

    Dim b As Boolean
    If m_bInit Then End
    m_bInit = True
    
    If IMCanvas1.dx.SystemBpp = 8 Then
        MsgBox "Sample must be run in at least 16 bit color"
        End
    End If
    
    Me.Show

    ' Try to have the imcanvas control select a device
    b = IMCanvas1.StartWindowed()

    ' See if that device supports all you want to do
    ' and if not restart in RGB
    If CheckCaps() = False Then
        b = IMCanvas1.InitWindowed("", "IID_IDirect3DRGBDevice")
        m_bUseRGB = True
    End If
        
    If b = False Then
        MsgBox "unable to initialize 3d"
        Exit Sub
    End If
    
    GetViewDim  'get extent of viewport
    InitScene   '
    InitDeviceObjects
    
    If CreateTextures() = False Then
        CleanUp
        m_bUseRGB = True
        b = IMCanvas1.InitWindowed("", "IID_IDirect3DRGBDevice")
        GetViewDim  'get extent of viewport
        InitScene   '
        InitDeviceObjects
        CreateTextures
    End If
    
    
    Dim fRestore As Boolean
    m_running = True
    Do While m_running
        On Local Error Resume Next
        If Not m_bDontRender Then
            fRestore = False
            While IMCanvas1.DirectDraw.TestCooperativeLevel <> DD_OK
                'Only render while we have control
                fRestore = True
                DoEvents
            Wend
            If fRestore Then
                IMCanvas1.DirectDraw.RestoreAllSurfaces
                CreateTextures
            End If
            MoveFrame
            IMCanvas1.ClearBackSurface
            IMCanvas1.Direct3DDevice.BeginScene
            DrawBackground
            DrawDebris
            DrawTreeShadows
            DrawTrees
            IMCanvas1.Direct3DDevice.EndScene

            IMCanvas1.Update
            DoEvents
        End If
    Loop

errOut:

    Unload Me
End Sub

Private Sub Form_Resize()
    
    If m_running = False Then Exit Sub
    
    On Local Error GoTo error1
    IMCanvas1.Width = Me.ScaleWidth
    IMCanvas1.Height = Me.ScaleHeight

    CleanUp
    
    If m_bUseRGB = True Then
        IMCanvas1.InitWindowed "", "IID_IDirect3DRGBDevice"
    Else
        IMCanvas1.StartWindowed
    End If
    
    CreateTextures
    InitDeviceObjects
    GetViewDim
    
    IMCanvas1.ClearBackSurface
    IMCanvas1.Update
    
error1:

End Sub



Function CreateTextures() As Boolean
    On Local Error GoTo errOut
    
    FindMediaDir "clouds3.bmp"
    
    Dim i As Integer
    Dim d3d As Direct3D7
    Dim d3ddev As Direct3DDevice7
    Set d3d = IMCanvas1.Direct3d
    Set d3ddev = IMCanvas1.Direct3DDevice
            
    Set m_TexClouds = Nothing
    Set m_TexShadow = Nothing
    Set m_TexTrees = Nothing
        
     'Create some textures - we limit the texture size to work on 2 meg cards
    With IMCanvas1
     Set m_TexClouds = IMCanvas1.CreateTextureSurface("cloud3.bmp", 0, 64, 64)
     Set m_TexShadow = IMCanvas1.CreateTextureSurface("Shadow1.bmp", 1, 64, 64)
     Set m_TexTrees = IMCanvas1.CreateTextureSurface("Tree0.bmp", 1, 64, 64)
    End With
    CreateTextures = True
errOut:

End Function


Function CheckCaps() As Boolean
    Dim hw As D3DDEVICEDESC7

    IMCanvas1.Direct3DDevice.GetCaps hw
    If IMCanvas1.Direct3DDevice.GetDeviceGuid() <> "IID_IDirect3DHALDevice" Then Exit Function
        
    CheckCaps = True

    If ((hw.dpcTriCaps.lTextureCaps And D3DPTEXTURECAPS_ALPHAPALETTE) = D3DPTEXTURECAPS_ALPHAPALETTE) Then
        Exit Function
    End If
    
    If ((hw.dpcTriCaps.lTextureCaps And D3DPTEXTURECAPS_ALPHA) = D3DPTEXTURECAPS_ALPHA) Then
        Exit Function
    End If
    
    CheckCaps = False
    

End Function



Sub FindMediaDir(sFile As String)
   
    
    On Local Error Resume Next
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

Private Sub Form_Unload(Cancel As Integer)
    m_running = False
End Sub




Sub CleanUp()
    Dim i As Long
    
    Set m_TexClouds = Nothing
    Set m_TexShadow = Nothing
    Set m_TexTrees = Nothing
    
    Set m_dx = Nothing
End Sub


Sub GetViewDim()
    Dim vp As D3DVIEWPORT7
    IMCanvas1.Direct3DDevice.GetViewport vp
        
    m_BackgroundMesh(0).sy = vp.lHeight
    m_BackgroundMesh(2).sy = vp.lHeight
    m_BackgroundMesh(2).sx = vp.lWidth
    m_BackgroundMesh(3).sx = vp.lWidth
    
End Sub

Private Sub IMCanvas1_NewDDraw()
        InitDeviceObjects
        CreateTextures
End Sub

