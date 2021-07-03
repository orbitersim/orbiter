VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "DrawPrimsVB"
   ClientHeight    =   3195
   ClientLeft      =   90
   ClientTop       =   660
   ClientWidth     =   3720
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   213
   ScaleMode       =   0  'User
   ScaleWidth      =   254
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const pi As Single = 3.141592
Const NUM_OBJECTS As Integer = 4

' Define the walls.
Const NUM_WALL_SIDES As Integer = 6
Const NUM_WALL_VERTICES = 2 * NUM_WALL_SIDES
Dim g_vWall(NUM_WALL_VERTICES + 4) As D3DVERTEX

' Define the cone.
Private Type MyFlexibleVertex
    vPosition As D3DVECTOR
    vNormal As D3DVECTOR
End Type

Const CONE_HEIGHT As Single = 3#
Const CONE_RADIUS As Single = 1#
Const NUM_CONE_SIDES As Integer = 20
Const NUM_CONE_VERTICES = NUM_CONE_SIDES + 1
Dim g_vCone(NUM_CONE_VERTICES) As MyFlexibleVertex

' Define the cube.
Const NUM_CUBE_VERTICES As Integer = 8
Const NUM_CUBE_INDICES As Integer = 6 * 3 * 2
Dim g_vCube(NUM_CUBE_VERTICES) As D3DTLVERTEX
Dim g_nCubeIndices(NUM_CUBE_INDICES) As Integer

Dim g_dx As New DirectX7
Dim g_dd As DirectDraw7
Dim g_ddsd As DDSURFACEDESC2
Dim g_ddsPrimary As DirectDrawSurface7, _
    g_ddsBackBuffer As DirectDrawSurface7, _
    g_ddsZBuffer As DirectDrawSurface7
Dim g_d3d As Direct3D7
Dim g_d3dDevice As Direct3DDevice7
Dim g_TriangleVert(5) As D3DVERTEX
Dim g_rcDest As RECT, _
    g_rcSrc As RECT
Dim g_d3drcViewport(0) As D3DRECT
Dim g_matLocal(NUM_OBJECTS) As D3DMATRIX
Dim g_bRunning As Boolean

Private Sub Form_Load()
    Dim CNT As Single
    Dim j As Long

    ' Initialize the DirectDraw and Direct3D objects that this
    ' sample application will use to render and display the triangle.
    InitDDraw
    InitD3D
    InitGeometry
    
    Me.Show
    
    g_bRunning = True
    Do While g_bRunning = True
        CNT = CNT + 1
        RenderScene
        FrameMove (CNT / 360)
        
        g_dx.GetWindowRect Me.hWnd, g_rcDest
    
        j = g_ddsPrimary.Blt(g_rcDest, g_ddsBackBuffer, g_rcSrc, DDBLT_WAIT)
        If j <> DD_OK Then
            MsgBox "Couldn't copy the source rectangle to the destination surface." & Chr$(13) & Hex(j)
            End
        End If
        DoEvents
    Loop
End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'   Animate the scene.
'
'   Called once per frame, the call is used for animating the scene. The device is
'   used for changing various render states, and the stepVal parameter is used for
'   the timing of the dynamics of the scene.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub FrameMove(stepVal As Single)
    
    ' Move the viewpoint around in a circle. The view is most conveniently defined by an
    ' eye point, a lookat point, and a vector defining the up direction.
    Dim vEyePt As D3DVECTOR, _
        vLookatPt As D3DVECTOR, _
        vUpVec As D3DVECTOR
        
    vEyePt = MakeVector(5 * Sin(stepVal), 3, 5 * Cos(stepVal))
    vLookatPt = MakeVector(4 * Sin(stepVal + 0.1), 2.5, 4 * Cos(stepVal + 0.1))
    vUpVec = MakeVector(0, 1, 0)
    
    ' Use the above parameters to build a new view matrix and put it into effect.
    Dim matView As D3DMATRIX
    
    Call g_dx.ViewMatrix(matView, vEyePt, vLookatPt, vUpVec, stepVal / 360)
                              
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_VIEW, matView
        
   '
   ' For TLVERTEX-type vertices, the application needs to do it's own transform and
   ' lighting. That will be done here, after the matrices are set up. This tutorial uses
   ' D3DTLVERTEX types to draw a cube. We start with untransformed vertices for the cube,
   ' manually transform them, and then render them as normal during the render loop.
   '
      
   ' Fill the array of TL vertices with untransformed data for the cube.
    g_dx.CreateD3DTLVertex -1, -1, -1, 0, &HFFFFFF, 0, 0, 0, g_vCube(0)
    g_dx.CreateD3DTLVertex 1, -1, -1, 0, &HFF&, 0, 0, 0, g_vCube(1)
    g_dx.CreateD3DTLVertex -1, 1, -1, 0, &HFF00&, 0, 0, 0, g_vCube(2)
    g_dx.CreateD3DTLVertex 1, 1, -1, 0, &HFF0000, 0, 0, 0, g_vCube(3)
    g_dx.CreateD3DTLVertex -1, -1, 1, 0, &HFFFF&, 0, 0, 0, g_vCube(4)
    g_dx.CreateD3DTLVertex 1, -1, 1, 0, &HFF0066, 0, 0, 0, g_vCube(5)
    g_dx.CreateD3DTLVertex -1, 1, 1, 0, &H9966FF, 0, 0, 0, g_vCube(6)
    g_dx.CreateD3DTLVertex 1, 1, 1, 0, &HFF00FF, 0, 0, 0, g_vCube(7)
    
    ' Let the application transform the cube's 8 vertices.
    Call TransformVertices(g_d3dDevice, g_vCube, NUM_CUBE_VERTICES)
      
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Render the scene
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub RenderScene()

    Dim i As Integer
          
    ' Clear the viewport to a blue color, and clear the z-buffer.
    g_d3dDevice.Clear 1, g_d3drcViewport(), D3DCLEAR_TARGET Or D3DCLEAR_ZBUFFER, &HFF, 1, 0
    
    ' Begin the scene.
    g_d3dDevice.BeginScene
    
    ' Draw the wall, composed of a D3DVERTEX-type triangle strip.
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, g_vWall(0), _
         NUM_WALL_VERTICES, D3DDP_DEFAULT)
                                   
    ' Draw the cone, which is a triangle fan of custom, flexible vertices.
    Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLEFAN, D3DFVF_XYZ Or D3DFVF_NORMAL, g_vCone(0), _
         NUM_CONE_VERTICES, D3DDP_DEFAULT)
         
    ' Draw the cube, which is application transformed (and lit), indexed vertices.
    Call g_d3dDevice.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, D3DFVF_TLVERTEX, g_vCube(0), _
         NUM_CUBE_VERTICES, g_nCubeIndices, NUM_CUBE_INDICES, D3DDP_DEFAULT)
   
    ' End the scene.
    g_d3dDevice.EndScene
End Sub

Private Sub Form_Unload(Cancel As Integer)
    g_bRunning = False
End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize DirectDraw.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub InitDDraw()
    ' Create the DirectDraw object and set the application
    ' cooperative level.
    Set g_dd = g_dx.DirectDrawCreate("")
    
    g_dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    
    ' Prepare and create the primary surface.
    g_ddsd.lFlags = DDSD_CAPS
    g_ddsd.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    
    Set g_ddsPrimary = g_dd.CreateSurface(g_ddsd)
    
    ' Now create the render-target surface. We are reusing g_ddsd here.
    g_ddsd.lFlags = DDSD_HEIGHT Or DDSD_WIDTH Or DDSD_CAPS
    g_ddsd.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_3DDEVICE
    
    ' Use the size of the form to determine the size of the render target
    ' and viewport rectangle.
    g_dx.GetWindowRect Me.hWnd, g_rcDest
    
    ' Set the dimensions of the surface description
    g_ddsd.lWidth = g_rcDest.Right - g_rcDest.Left
    g_ddsd.lHeight = g_rcDest.Bottom - g_rcDest.Top
    
    ' Create the render-target surface
    Set g_ddsBackBuffer = g_dd.CreateSurface(g_ddsd)
        
    ' Cache the dimensions of the render target. We'll use
    ' it for blitting operations.
    With g_rcSrc
        .Left = 0: .Top = 0
        .Bottom = g_ddsd.lHeight
        .Right = g_ddsd.lWidth
    End With
    
    ' Create a DirectDrawClipper and attach it to the primary surface.
    Dim pcClipper As DirectDrawClipper
    
    Set pcClipper = g_dd.CreateClipper(0)
    pcClipper.SetHWnd Me.hWnd
    
    g_ddsPrimary.SetClipper pcClipper
    
    '
    ' Create the z-buffer after creating the backbuffer and before creating the
    ' Direct3D device. Note: before creating the z-buffer, applications may want to check
    ' the device capabilities for the D3DPRASTERCAPS_ZBUFFERLESSHSR flag. This flag is true
    ' for certain hardware that can do HSR (hidden-surface removal) without a z-buffer.
    ' For those devices, there is no need to create a z-buffer.
    '
    Dim ddpfZBuffer As DDPIXELFORMAT
    Dim d3dEnumPFs As Direct3DEnumPixelFormats
        
    Set g_d3d = g_dd.GetDirect3D
    Set d3dEnumPFs = g_d3d.GetEnumZBufferFormats("IID_IDirect3DRGBDevice")
    
    ' For this tutorial we are only interested in z-buffers, so ignore any other
    ' formats (e.g. DDPF_STENCILBUFFER) the get enumerated. An application could also
    ' check the depth fo the z-buffer (16-bit, etc. and make a choice based on that,
    ' as well. For this tutorial, we'll take the first one that we get.
    Dim i As Long
          
    For i = 1 To d3dEnumPFs.GetCount()
        Call d3dEnumPFs.GetItem(i, ddpfZBuffer)
        If ddpfZBuffer.lFlags = DDPF_ZBUFFER Then
            Exit For
        End If
    Next i
    
    ' Prepare and create the z-buffer surface.
    g_ddsd.lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT Or DDSD_PIXELFORMAT
    g_ddsd.ddsCaps.lCaps = DDSCAPS_ZBUFFER
    g_ddsd.lWidth = g_rcDest.Right - g_rcDest.Left
    g_ddsd.lHeight = g_rcDest.Bottom - g_rcDest.Top
    g_ddsd.ddpfPixelFormat = ddpfZBuffer
    
    ' Specify DDSCAPS_VIDEOMEMORY if your surface exists in display memory.
    ' See the SDK documentation for more information.
    g_ddsd.ddsCaps.lCaps = g_ddsd.ddsCaps.lCaps Or DDSCAPS_SYSTEMMEMORY
        
    Set g_ddsZBuffer = g_dd.CreateSurface(g_ddsd)
    
    ' Attach the z-buffer surface to the back buffer surface.
    g_ddsBackBuffer.AddAttachedSurface g_ddsZBuffer
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize Direct3D, including the rendering device, lighting,
' the viewport, and the material.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub InitD3D()
    Dim d3d As Direct3D7
    Dim ddsd As DDSURFACEDESC2
        
    ' Retrieve a reference to the Direct3D7 class from the
    ' DirectDraw7 object.
    Set d3d = g_dd.GetDirect3D
    
    ' Query DirectDraw for the current display mode. For simplicity, this
    ' does not support palleted display modes (8-bit and lower).
    g_dd.GetDisplayMode ddsd
    
    If ddsd.ddpfPixelFormat.lRGBBitCount <= 8 Then
        MsgBox "This application does not support screen display " & _
               "modes lower than 16-bit."
        End
    End If
    
    '
    ' Create the device. The GUID is hardcoded for now, but should come from
    ' device enumeration, which is the topic of a future tutorial. The device
    ' is created off of our back buffer, which becomes the render target for
    ' the newly created device.
    '
    Set g_d3dDevice = d3d.CreateDevice("IID_IDirect3DRGBDevice", g_ddsBackBuffer)
    
    ' Define the viewport rectangle.
    Dim VPDesc As D3DVIEWPORT7
        
    VPDesc.lWidth = g_rcDest.Right - g_rcDest.Left
    VPDesc.lHeight = g_rcDest.Bottom - g_rcDest.Top
    VPDesc.minz = 0#
    VPDesc.maxz = 1#
    
    g_d3dDevice.SetViewport VPDesc
    
    ' Cache the viewport rectangle for use in clearing operations later.
    With g_d3drcViewport(0)
        .X1 = 0: .Y1 = 0
        .X2 = VPDesc.lWidth
        .Y2 = VPDesc.lHeight
    End With
        
    '
    ' Prepare a material
    '
    Dim mtrl As D3DMATERIAL7
   
    mtrl.diffuse.r = 1#: mtrl.diffuse.g = 1#: mtrl.diffuse.b = 1#
    mtrl.Ambient.r = 1#: mtrl.Ambient.g = 1#: mtrl.Ambient.b = 1#
    
    ' Commit the material to the device.
    g_d3dDevice.SetMaterial mtrl
    
    '
    ' Set up transformation matrices.
    '
    
    ' The world matrix controls the position and orientation of the polygons
    ' in world space. We'll use it later to spin the triangle.
    Dim matWorld As D3DMATRIX
    
    g_dx.IdentityMatrix matWorld
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_WORLD, matWorld
    
    ' The view matrix defines the position and orientation of the camera.
    ' Here, we are just moving it back along the z-axis by 15 units.
    Dim matView  As D3DMATRIX
    
    g_dx.IdentityMatrix matView
    
    Call g_dx.ViewMatrix(matView, MakeVector(0, 0, -10), MakeVector(0, 0, 0), _
                         MakeVector(0, 1, 0), 0)
    
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    
    ' The projection matrix defines how the 3D scene is "projected" onto the
    ' 2D render target (the backbuffer surface). Refer to the docs for more
    ' info about projection matrices.
    Dim matProj As D3DMATRIX
    
    g_dx.IdentityMatrix matProj
    
    Call g_dx.ProjectionMatrix(matProj, 1, 1000, pi / 3#)
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj
    
    ' Enable z-buffering for the application.
    g_d3dDevice.SetRenderState D3DRENDERSTATE_ZENABLE, D3DZB_TRUE
    
    'Set up the light.
    Dim light As D3DLIGHT7
    
    light.dltType = D3DLIGHT_POINT
    light.diffuse.r = 1#
    light.diffuse.g = 1#
    light.diffuse.b = 1#
    light.position.x = light.direction.x = 5#
    light.position.y = light.direction.y = 5#
    light.position.z = light.direction.z = 0#
    'light.range = D3DLIGHT_RANGE_MAX
    light.attenuation1 = 1#
    
    g_d3dDevice.SetLight 0, light
    g_d3dDevice.LightEnable 0, True
    g_d3dDevice.SetRenderState D3DRENDERSTATE_LIGHTING, True
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Initalize the geometry.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub InitGeometry()

    ' Set up the wall geometry.
    Dim x As Double, _
        y As Double, _
        z As Double
        
    Dim i As Integer
    
    For i = 0 To NUM_WALL_SIDES - 1
        x = Sin(2 * pi * i / (NUM_WALL_SIDES - 1))
        z = Cos(2 * pi * i / (NUM_WALL_SIDES - 1))
        
        Call g_dx.CreateD3DVertex(x * 10#, -0.1 * 10#, z * 10#, -x, 0, -z, 0, 0, g_vWall(2 * i + 0))
        Call g_dx.CreateD3DVertex(x * 10#, 0.1 * 10#, z * 10#, -x, 0, -z, 0, 0, g_vWall(2 * i + 1))
                
    Next i
    
    ' Set up the cone geometry.
    Dim vectorNorm As D3DVECTOR
    
    g_vCone(0).vPosition = MakeVector(0, CONE_HEIGHT / 2, 0)
        
    vectorNorm = MakeVector(0, 1, 0)
    Call g_dx.VectorNormalize(vectorNorm)
    g_vCone(0).vNormal = vectorNorm
        
    For i = 0 To NUM_CONE_SIDES - 1
        x = Sin(2 * pi * i / (NUM_CONE_SIDES - 1))
        y = -CONE_HEIGHT / 2
        z = Cos(2 * pi * i / (NUM_CONE_SIDES - 1))
        
        g_vCone(i + 1).vPosition = MakeVector(x * CONE_RADIUS, y * CONE_RADIUS, z * CONE_RADIUS)
        
        vectorNorm = MakeVector(x, 0.5, z)
        Call g_dx.VectorNormalize(vectorNorm)
        g_vCone(i + 1).vNormal = vectorNorm
   Next i
   
   'Set the cube indices.
    g_nCubeIndices(0) = 1: g_nCubeIndices(1) = 2: g_nCubeIndices(2) = 3
    g_nCubeIndices(3) = 2: g_nCubeIndices(4) = 1: g_nCubeIndices(5) = 0
    g_nCubeIndices(6) = 4: g_nCubeIndices(7) = 5: g_nCubeIndices(8) = 6
    g_nCubeIndices(9) = 6: g_nCubeIndices(10) = 5: g_nCubeIndices(11) = 7
    g_nCubeIndices(12) = 3: g_nCubeIndices(13) = 2: g_nCubeIndices(14) = 6
    g_nCubeIndices(15) = 3: g_nCubeIndices(16) = 6: g_nCubeIndices(17) = 7
    g_nCubeIndices(18) = 0: g_nCubeIndices(19) = 1: g_nCubeIndices(20) = 4
    g_nCubeIndices(21) = 4: g_nCubeIndices(22) = 1: g_nCubeIndices(23) = 5
    g_nCubeIndices(24) = 2: g_nCubeIndices(25) = 0: g_nCubeIndices(26) = 4
    g_nCubeIndices(27) = 2: g_nCubeIndices(28) = 4: g_nCubeIndices(29) = 6
    g_nCubeIndices(30) = 1: g_nCubeIndices(31) = 3: g_nCubeIndices(32) = 5
    g_nCubeIndices(33) = 5: g_nCubeIndices(34) = 3: g_nCubeIndices(35) = 7
   
End Sub

Private Function MakeVector(a As Double, b As Double, c As Double) As D3DVECTOR
    Dim vecOut As D3DVECTOR
    
    With vecOut
        .x = a
        .y = b
        .z = c
    End With
    
    MakeVector = vecOut
End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Manually transform vertices from local 3-D space to 2-D screen coordinates.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub TransformVertices(d3dDevice As Direct3DDevice7, vVertices() As D3DTLVERTEX, nNumVertices As Integer)
    
    Dim vp As D3DVIEWPORT7
    Dim nClipWidth As Integer
    Dim nClipHeight As Integer
        
    ' Get the height and width of the viewport. This is needed to scale the transformed vertices
    ' to fit the render window.
    d3dDevice.GetViewport vp
    nClipWidth = vp.lWidth / 2
    nClipHeight = vp.lHeight / 2
    
    ' Get the current matrix set. This is needed for the transformation.
    Dim matWorld As D3DMATRIX
    Dim matView As D3DMATRIX
    Dim matProj As D3DMATRIX
    
    d3dDevice.GetTransform D3DTRANSFORMSTATE_WORLD, matWorld
    d3dDevice.GetTransform D3DTRANSFORMSTATE_VIEW, matView
    d3dDevice.GetTransform D3DTRANSFORMSTATE_PROJECTION, matProj
    
    Dim matSet As D3DMATRIX
    
    g_dx.IdentityMatrix matSet
     
    ' Concatenate the matrices using the right-to-left rule.
    g_dx.MatrixMultiply matSet, matWorld, matView
    g_dx.MatrixMultiply matSet, matSet, matProj
    
    Dim i As Integer
    
    Dim xp As Single
    Dim yp As Single
    Dim zp As Single
    Dim wp As Single
    
    Dim x As Double
    Dim y As Double
    Dim z As Double
      
    ' Transform each vertex through the current matrix set.
    For i = 0 To nNumVertices - 1
        ' Get the untransformed vertex position.
        x = vVertices(i).sx
        y = vVertices(i).sy
        z = vVertices(i).sz
        
       ' Transform the vertex position through the current matrix set.
        xp = matSet.rc11 * x + matSet.rc21 * y + matSet.rc31 * z + matSet.rc41
        yp = matSet.rc12 * x + matSet.rc22 * y + matSet.rc32 * z + matSet.rc42
        zp = matSet.rc13 * x + matSet.rc23 * y + matSet.rc33 * z + matSet.rc43
        wp = matSet.rc14 * x + matSet.rc24 * y + matSet.rc34 * z + matSet.rc44
        
        ' Finally, scale the vertices to screen coordinates. This first step "flattens"
        ' the coordinates from 3-D space to 2-D device coordinates, by dividing each
        ' coordinate by the wp value. Then, the x- and y-componenets are transformed from
        ' device coordinates to screen coordinates.
        ' Note 1: device coords range from -1 to +1 in the viewport
        ' Note 2: the sz-coordinate will be used in the z-buffer
        vVertices(i).sx = (1# + (xp / wp)) * nClipWidth
        vVertices(i).sy = (1# - (yp / wp)) * nClipHeight
        vVertices(i).sz = zp / wp
        vVertices(i).rhw = wp
   Next i
                  
End Sub

