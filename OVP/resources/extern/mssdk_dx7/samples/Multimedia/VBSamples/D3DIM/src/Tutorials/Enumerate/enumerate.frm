VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Device Enumeration"
   ClientHeight    =   3195
   ClientLeft      =   840
   ClientTop       =   1125
   ClientWidth     =   3720
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   213
   ScaleMode       =   0  'User
   ScaleWidth      =   254
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const pi As Single = 3.141592
Const NUM_CONES As Integer = 10

' Define the cone.
Private Type MyFlexibleVertex
    vPosition As D3DVECTOR
    vNormal As D3DVECTOR
End Type

Const CONE_HEIGHT As Single = 8#
Const CONE_RADIUS As Single = 1#
Const NUM_CONE_SIDES As Integer = 20
Const NUM_CONE_VERTICES = NUM_CONE_SIDES + 1
Dim g_vCone(NUM_CONE_VERTICES) As MyFlexibleVertex
Dim g_matConePos(NUM_CONES) As D3DMATRIX
Dim g_afConeColor(0 To NUM_CONES - 1, 0 To 2) As Double

Dim g_dx As New DirectX7
Dim g_dd As DirectDraw7
Dim g_ddsd As DDSURFACEDESC2
Dim g_ddsPrimary As DirectDrawSurface7, _
    g_ddsBackBuffer As DirectDrawSurface7, _
    g_ddsZBuffer As DirectDrawSurface7
Dim g_d3d As Direct3D7
Dim g_d3dDevice As Direct3DDevice7
Dim g_rcDest As RECT, _
    g_rcSrc As RECT
Dim g_d3drcViewport(0) As D3DRECT
Dim g_bRunning As Boolean

' Variables to hold enumerated values
Dim g_sDriverGUID As String
Dim g_sDeviceGUID As String
Dim g_ddsdMode As DDSURFACEDESC2
Dim g_bUsingFullScreen As Boolean
Dim g_bUsing3DHardware As Boolean


Public Sub RunApp()
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
        
        If Not g_bUsingFullScreen Then
            g_dx.GetWindowRect Me.hWnd, g_rcDest
    
            j = g_ddsPrimary.Blt(g_rcDest, g_ddsBackBuffer, g_rcSrc, DDBLT_WAIT)
            If j <> DD_OK Then
                MsgBox "Couldn't copy the source rectangle to the destination surface." & Chr$(13) & Hex(j)
                End
            End If
        Else
            g_ddsPrimary.Flip Nothing, DDFLIP_WAIT
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
    
    ' Initialize the matrix to the identity.
    g_dx.IdentityMatrix matView

    Call g_dx.ViewMatrix(matView, vEyePt, vLookatPt, vUpVec, stepVal / 360)
                              
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_VIEW, matView
        
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Render the scene. This tutorial draws a bunch of random-colored cones.
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''
Private Sub RenderScene()

    Dim i As Integer
          
    ' Clear the viewport to a blue color, and clear the z-buffer.
    g_d3dDevice.Clear 1, g_d3drcViewport(), D3DCLEAR_TARGET Or D3DCLEAR_ZBUFFER, &HFF, 1, 0
    
    ' Begin the scene.
    g_d3dDevice.BeginScene
    
    ' Draw the set of cones.
    For i = 0 To NUM_CONES - 1
        ' Set the base material color for the cone
        Dim mtrl As D3DMATERIAL7
        mtrl.diffuse.r = g_afConeColor(i, 0)
        mtrl.diffuse.g = g_afConeColor(i, 1)
        mtrl.diffuse.b = g_afConeColor(i, 2)
        g_d3dDevice.SetMaterial mtrl
        
        ' Position the cone with the world matrix
        g_d3dDevice.SetTransform D3DTRANSFORMSTATE_WORLD, g_matConePos(i)
        
        ' Draw the cone, which is a triangle fan of custom, flexible vertices.
         Call g_d3dDevice.DrawPrimitive(D3DPT_TRIANGLEFAN, D3DFVF_XYZ Or D3DFVF_NORMAL, g_vCone(0), _
              NUM_CONE_VERTICES, D3DDP_DEFAULT)
        
    Next i
    
    ' End the scene.
    g_d3dDevice.EndScene
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
    If KeyAscii = vbKeyEscape Then
        Unload Me
    End If
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
    ' Get information about the currently selected driver and device. This
    ' information is from driver/device/mode enumeration.
    Call Dialog.GetSelectedDriver(g_sDriverGUID, g_sDeviceGUID, g_ddsdMode.lWidth, g_ddsdMode.lHeight, g_ddsdMode.ddpfPixelFormat.lRGBBitCount, g_bUsingFullScreen, g_bUsing3DHardware)
    
    
    ' Get the enumerated constant values.
    Dim ddEnum As DirectDrawEnum
    Set ddEnum = g_dx.GetDDEnum()
    
    ' Create the DirectDraw object and set the application
    ' cooperative level.
    Set g_dd = g_dx.DirectDrawCreate(g_sDriverGUID)
        
    ' Do we want to render in windowed mode or in full screen exclusive mode?
    If g_bUsingFullScreen Then
        g_dd.SetCooperativeLevel Me.hWnd, DDSCL_EXCLUSIVE Or DDSCL_FULLSCREEN
    Else
        g_dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    End If
    
    ' If full screen, we need to set the display mode.
    If g_bUsingFullScreen Then
        g_dd.SetDisplayMode g_ddsdMode.lWidth, g_ddsdMode.lHeight, g_ddsdMode.ddpfPixelFormat.lRGBBitCount, 0, DDSDM_STANDARDVGAMODE
    End If
    
    ' Prepare the primary surface depending on the mode.
    If g_bUsingFullScreen Then
        g_ddsd.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
        g_ddsd.ddscaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX Or DDSCAPS_3DDEVICE
        g_ddsd.lBackBufferCount = 1
    Else
        g_ddsd.lFlags = DDSD_CAPS
        g_ddsd.ddscaps.lCaps = DDSCAPS_PRIMARYSURFACE
    End If
    
    ' Create the primary surface.
    Set g_ddsPrimary = g_dd.CreateSurface(g_ddsd)
    
    If Not g_bUsingFullScreen Then
        ' Create a DirectDrawClipper and attach it to the primary surface.
        Dim pcClipper As DirectDrawClipper
    
        Set pcClipper = g_dd.CreateClipper(0)
        pcClipper.SetHWnd Me.hWnd
    
        g_ddsPrimary.SetClipper pcClipper
    End If
    
    If Not g_bUsingFullScreen Then
        ' Now create the render-target surface. We are reusing g_ddsd here.
        g_ddsd.lFlags = DDSD_HEIGHT Or DDSD_WIDTH Or DDSD_CAPS
        g_ddsd.ddscaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_3DDEVICE
    
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
    Else
        With g_rcSrc
            .Left = 0: .Top = 0
            .Bottom = g_ddsdMode.lHeight
            .Right = g_ddsdMode.lWidth
        End With
        
        With g_rcDest
            .Left = 0: .Top = 0
            .Bottom = g_ddsdMode.lHeight
            .Right = g_ddsdMode.lWidth
        End With
        
        ' Create the render-target surface
        Dim ddscaps As DDSCAPS2
        
        With ddscaps
            .lCaps = DDSCAPS_BACKBUFFER
            .lCaps2 = 0
            .lCaps3 = 0
            .lCaps4 = 0
        End With
        
       ' Retrieve the back buffer, which will be our render target.
       Set g_ddsBackBuffer = g_ddsPrimary.GetAttachedSurface(ddscaps)
        
    End If
    
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
    Set d3dEnumPFs = g_d3d.GetEnumZBufferFormats(g_sDeviceGUID)
    
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
    g_ddsd.ddscaps.lCaps = DDSCAPS_ZBUFFER
    g_ddsd.lWidth = g_rcDest.Right - g_rcDest.Left
    g_ddsd.lHeight = g_rcDest.Bottom - g_rcDest.Top
    g_ddsd.ddpfPixelFormat = ddpfZBuffer
    
    ' For hardware devices, the z-buffer should be in video memory. For software
    ' devices, create the z-buffer in system memory.
    If g_sDeviceGUID = "IID_IDirect3DHALDevice" Then
        g_ddsd.ddscaps.lCaps = g_ddsd.ddscaps.lCaps Or DDSCAPS_VIDEOMEMORY
    Else
        g_ddsd.ddscaps.lCaps = g_ddsd.ddscaps.lCaps Or DDSCAPS_SYSTEMMEMORY
    End If
       
    On Error Resume Next
    Set g_ddsZBuffer = g_dd.CreateSurface(g_ddsd)
    If g_ddsZBuffer Is Nothing Then
        MsgBox ("Surface creation failed.")
        End
    End If
       
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
    ' Create the device. The device is created off of our back buffer, which
    ' becomes the render target for the newly created device.
    '
    On Error Resume Next
    Set g_d3dDevice = d3d.CreateDevice(g_sDeviceGUID, g_ddsBackBuffer)
    If g_d3dDevice Is Nothing Then
        MsgBox ("Device creation failed.")
        End
    End If
    
    
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
    
    Call g_dx.ViewMatrix(matView, MakeVector(0, 0, -15), MakeVector(0, 0, 0), _
                         MakeVector(0, 1, 0), 0)
    
    g_d3dDevice.SetTransform D3DTRANSFORMSTATE_VIEW, matView
    
    ' The projection matrix defines how the 3D scene is "projected" onto the
    ' 2D render target (the backbuffer surface). Refer to the docs for more
    ' info about projection matrices.
    Dim matProj As D3DMATRIX
    
    g_dx.IdentityMatrix matProj
    
    Call g_dx.ProjectionMatrix(matProj, 1, 1000, pi / 2#)
    
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
    light.range = 100#       ' D3DLIGHT_RANGE_MAX
    light.attenuation0 = 1#
        
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

    Dim i As Integer
   
    Dim x As Double, _
        y As Double, _
        z As Double
   
   '
   ' Set the cone's vertices. The cone is a triangle fan, so the zeroth vertex is the
   ' tip of the cone (center of the fan) and the rest of the vertices are on the base
   ' on the cone.
   '
  
    Dim vectorNorm As D3DVECTOR
    
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
   
    'Set the position and color attributes for each cone.
    For i = 0 To NUM_CONES - 1
        ' Set the cone's scale.
        g_matConePos(i).rc11 = 1#
        g_matConePos(i).rc22 = 1#
        g_matConePos(i).rc33 = 1#
        g_matConePos(i).rc44 = 1#
        
        ' Set the cone's position.
        g_matConePos(i).rc41 = 10 * RandomVal
        g_matConePos(i).rc42 = 0#
        g_matConePos(i).rc43 = 10 * RandomVal
        
        ' Set the cone's color
        g_afConeColor(i, 0) = (1 + RandomVal) / 2
        g_afConeColor(i, 1) = (1 + RandomVal) / 2
        g_afConeColor(i, 2) = (1 + RandomVal) / 2
                
    Next i
   
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

Private Function RandomVal() As Double
    ' Here we just use the Rnd() function.
    RandomVal = Rnd()
End Function





