VERSION 5.00
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMControl.ocx"
Begin VB.Form vertexBufferFrm 
   Caption         =   "VB Vertex Buffer Sample"
   ClientHeight    =   4575
   ClientLeft      =   1530
   ClientTop       =   930
   ClientWidth     =   4935
   Icon            =   "VertexBufferForm.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   4575
   ScaleWidth      =   4935
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   4575
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4935
      _ExtentX        =   8705
      _ExtentY        =   8070
   End
End
Attribute VB_Name = "vertexBufferFrm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' Constants
Const FLAG_SIZE = 10
Const NUM_FLAG_VERTICES = ((FLAG_SIZE + 1) * (FLAG_SIZE + 1))
Const NUM_FLAG_INDICES = (FLAG_SIZE * FLAG_SIZE * 6)
Const NUM_POLE_VERTICES = 8 * 2

' Global data
Dim g_iFlagIndices(NUM_FLAG_INDICES) As Integer          ' Holds index values into the following vertex array
Dim g_vertPoleVertices(NUM_POLE_VERTICES) As D3DVERTEX   ' Holds vertex data for the flag
Dim g_tlvertBackground(4) As D3DTLVERTEX                 ' Holds vertex data for the background
Dim g_dTime As Single
Dim g_bResizing As Boolean
Dim g_bReadyToRender As Boolean
Dim g_bRunning As Boolean
Dim g_binit As Boolean

' Global D3D object variables
Dim g_FlagTex As DirectDrawSurface7
Dim g_BGTex As DirectDrawSurface7
Dim g_VertexBuffer As Direct3DVertexBuffer7

Dim g_dx As DirectX7


' Win32 Declares
Private Declare Sub Sleep Lib "kernel32" (ByVal lMilliseconds As Long)

Private Sub Form_Load()
    If g_binit Then End
    g_binit = True

    On Local Error GoTo ErrorDone
        
    ' Set initial execution states.
    g_bReadyToRender = True
    g_bRunning = True
    
    
    ' Initialize the canvas object.
    If IMCanvas1.StartWindowed() = False Then GoTo ErrorDone
    
    Set g_dx = IMCanvas1.dx
    
    'create Some geometry
    InitGeometry
    
    ' Initialize Direct3D.
    If InitD3D = False Then GoTo ErrorDone
    g_dTime = Timer
    
    Me.Show
    
    Do While g_bRunning
	on local error resume next
        If g_bReadyToRender = True Then
            UpdateFrame (Timer - g_dTime) ' Use the relative time to animate the flag.
            RenderFrame
            DoEvents
            IMCanvas1.Update
        Else
            SleepUntilReady
        End If
    Loop
    
    Unload Me
    Exit Sub
    
ErrorDone:
    If IMCanvas1.dx.SystemBpp < 16 Then
        MsgBox "This sample will only run in 16bpp color or better"
        End
    End If
    MsgBox "Couldn't initialize the 3D scene. Terminating execution.", _
        vbOKOnly, "Initialization Error."
    Unload Me
End Sub

'
' Description:
'     InitD3D attempts to set up the components needed to
'     display a 3D scene. It calls methods of the helper
'     IMCanvas1 object to prepare for rendering in windowed
'     state, and calls internal functions to setup
'
Private Function InitD3D() As Boolean
    Dim bRet As Boolean
    InitD3D = True
        
    InitVertexBuffer
    
    bRet = InitTextures
    If bRet = False Then
        Debug.Print "Could not load the texture maps, exiting application."
        InitD3D = False: Exit Function
    End If
    
    InitViewportAndMaterials
    
    ' Return a success value.
    InitD3D = True
End Function



Private Sub InitGeometry()
   Dim i As Integer, r As Integer
   Dim ix As Integer, iy As Integer
          
   i = 0
   For ix = 0 To FLAG_SIZE - 1
      For iy = 0 To FLAG_SIZE - 1
         g_iFlagIndices(i) = (ix + 0) + (iy + 1) * (FLAG_SIZE + 1)
         g_iFlagIndices(i + 1) = (ix + 1) + (iy + 0) * (FLAG_SIZE + 1)
         g_iFlagIndices(i + 2) = (ix + 0) + (iy + 0) * (FLAG_SIZE + 1)
         g_iFlagIndices(i + 3) = (ix + 0) + (iy + 1) * (FLAG_SIZE + 1)
         g_iFlagIndices(i + 4) = (ix + 1) + (iy + 1) * (FLAG_SIZE + 1)
         g_iFlagIndices(i + 5) = (ix + 1) + (iy + 0) * (FLAG_SIZE + 1)
         i = i + 6
      Next iy
   Next ix

   For r = 0 To 7
      Dim theta As Double, x As Double, z As Double
      Dim vecNorm As D3DVECTOR
      
      theta = (r / 8) * 2 * 3.1415926283
      x = Cos(theta) * 0.1
      z = -Sin(theta) * 0.1
      
      vecNorm = MakeVector(x, 0, z)
      Call g_dx.VectorNormalize(vecNorm)

      g_vertPoleVertices(2 * r + 0) = MakeVertex(MakeVector(x, 10, z), vecNorm, 0, r / 7)
      g_vertPoleVertices(2 * r + 1) = MakeVertex(MakeVector(x, 0, z), vecNorm, 1, r / 7)
   Next r

   Dim bgArray
   bgArray = Array(0, 0, 0.99, 0.5, -1, 0, 0, 0.6, _
                   0, 0, 0.99, 0.5, -1, 0, 1, 0, _
                   0, 0, 0.99, 0.5, -1, 0, 0, 0.6, _
                   0, 0, 0.99, 0.5, -1, 0, 1, 0)
   
   ' Reuse r and i here
   r = 0
   For i = 0 To 3
      With g_tlvertBackground(i)
         .sx = bgArray(r)
         .sy = bgArray(r + 1)
         .sz = bgArray(r + 2)
         .rhw = bgArray(r + 3)
         .Color = bgArray(r + 4)
         .specular = bgArray(r + 5)
         .tu = bgArray(r + 6)
         .tv = bgArray(r + 7)
         r = r + 8
      End With
   Next i
End Sub





Private Function InitTextures() As Boolean
    On Local Error GoTo TEXTURE_ERROR
    FindMediaDir ("dx_logo.bmp")
    
    With IMCanvas1
        Dim bRet As Boolean
        Set g_FlagTex = .CreateTextureSurface("dx_logo.bmp", D3DTEXTR_DEFAULT, 0, 0)
        Set g_BGTex = .CreateTextureSurface("cloud3.bmp", D3DTEXTR_DEFAULT, 0, 0)
    End With

    InitTextures = True
    Exit Function
TEXTURE_ERROR:
    Debug.Print "Could not load the texture files required for this sample. Make sure they are present in the sample directory, or in the Media folder."
    InitTextures = False
End Function

Public Function InitVertexBuffer() As Boolean
    On Local Error Resume Next
    Dim d3d As Direct3D7
    Dim d3dDevice As Direct3DDevice7
    
    Set d3d = IMCanvas1.Direct3d
    Set d3dDevice = IMCanvas1.Direct3DDevice
    
    Dim VBDesc As D3DVERTEXBUFFERDESC
    Dim HWDesc As D3DDEVICEDESC7, SWDesc As D3DDEVICEDESC7
    
    InitVertexBuffer = True
    
    '
    ' Prepare the vertex buffer description.
    '
    VBDesc.lCaps = D3DVBCAPS_SYSTEMMEMORY
    VBDesc.lFVF = D3DFVF_VERTEX
    VBDesc.lNumVertices = NUM_FLAG_VERTICES
    
    
    
    ' Create the vertex buffer.
    Set g_VertexBuffer = d3d.CreateVertexBuffer(VBDesc, D3DDP_DEFAULT)
    If Err.Number = D3DERR_VBUF_CREATE_FAILED Then
        InitVertexBuffer = False: Exit Function
    End If

    '
    ' Fill the buffer with vertex data.
    '
    With g_VertexBuffer
        '   Note: You can only access the vertices in a vertex
        '         buffer while it is locked.
        .Lock (DDLOCK_WAIT)
        
        ' Prep an array to pass into the vertex buffer.
        Dim Verts() As D3DVERTEX
        ReDim Verts((FLAG_SIZE + 1) ^ 2)
        Dim ix As Integer, iy As Integer
        Dim x As Double, y As Double
        Dim tu As Double, tv As Double
        
        For ix = 0 To FLAG_SIZE
            For iy = 0 To FLAG_SIZE
                tu = ix / FLAG_SIZE
                tv = iy / FLAG_SIZE
                x = 0.2 + tu * 3.31
                y = 8# + tv * 1.82

                Verts(ix + iy * (FLAG_SIZE + 1)) = MakeVertex(MakeVector(x, y, 0), _
                                                            MakeVector(0, 0, -1), _
                                                            tu, 1 - tv)
            Next iy
        Next ix
        
        ' Set the vertices, then unlock the buffer.
        .SetVertices 0, NUM_FLAG_VERTICES, Verts(0)
        .Unlock
    End With
End Function

Private Sub InitViewportAndMaterials()
    ' D3D Objects
    Dim d3dDevice As Direct3DDevice7
    Dim d3d As Direct3D7
    
    ' Locals
    Dim VPDesc As D3DVIEWPORT7
    Dim MatDesc As D3DMATERIAL7
    Dim vecEye As D3DVECTOR
    Dim vecLookAt As D3DVECTOR
    Dim vecUp As D3DVECTOR
    Dim matWorld As D3DMATRIX, _
        matView As D3DMATRIX, _
        matProj As D3DMATRIX
    Dim LightDesc As D3DLIGHT7
       
    ' Prep object variables
    Set d3dDevice = IMCanvas1.Direct3DDevice
    Set d3d = IMCanvas1.Direct3d
       
    ' Size the background set of polygons (the ones that
    ' are textured with the sky bitmap).
    Call d3dDevice.GetViewport(VPDesc)
      
    g_tlvertBackground(0).sy = VPDesc.lHeight
    g_tlvertBackground(2).sy = VPDesc.lHeight
    g_tlvertBackground(2).sx = VPDesc.lWidth
    g_tlvertBackground(3).sx = VPDesc.lWidth

    '
    ' Setup the object material
    '
    ' Use the global object for its shortcut color methods.
    Dim dx As New DirectX7
    MatDesc.Ambient = RGBAToD3DCOLORVALUE(1, 1, 1, 0)
    MatDesc.diffuse = RGBAToD3DCOLORVALUE(1, 1, 1, 0)
    
    ' Set the material properties for the device.
    Call d3dDevice.SetMaterial(MatDesc)

    '
    ' Setup the transformation matrices.
    '
    vecEye = MakeVector(-1#, 7.5, -3#)
    vecLookAt = MakeVector(2#, 7.5, 0#)
    vecUp = MakeVector(0#, 1#, 0#)
    
    ' Use the helper methods of the global object to create the matrices
    dx.IdentityMatrix matWorld
    dx.ViewMatrix matView, vecEye, vecLookAt, vecUp, 0
    dx.ProjectionMatrix matProj, 1#, 100#, 3.141592 / 2#
        
    ' Set the transformations.
    Call d3dDevice.SetTransform(D3DTRANSFORMSTATE_WORLD, matWorld)
    Call d3dDevice.SetTransform(D3DTRANSFORMSTATE_VIEW, matView)
    Call d3dDevice.SetTransform(D3DTRANSFORMSTATE_PROJECTION, matProj)
 
    '
    ' Setup lighting.
    '
    With LightDesc
        .dltType = D3DLIGHT_POINT
        .position = MakeVector(-1#, 8#, -2#)
        .Ambient = RGBAToD3DCOLORVALUE(1#, 1#, 1#, 0#) ' The alpha component isn't used for lights.
        .diffuse = RGBAToD3DCOLORVALUE(1#, 1#, 1#, 0#) ' The alpha component isn't used for lights.
        .specular = RGBAToD3DCOLORVALUE(1#, 1#, 1#, 0#) ' The alpha component isn't used for lights.
        .attenuation0 = 1# ' Don't attenuate the light
    End With
    
    d3dDevice.SetLight 0, LightDesc
    d3dDevice.LightEnable 0, True
    d3dDevice.SetRenderState D3DRENDERSTATE_LIGHTING, True
    
    ' Set up the default texture states.
    Call d3dDevice.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE)
    Call d3dDevice.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE)
    Call d3dDevice.SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE)
    Call d3dDevice.SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR)
    Call d3dDevice.SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR)
    Call d3dDevice.SetRenderState(D3DRENDERSTATE_DITHERENABLE, True)
    Call d3dDevice.SetRenderState(D3DRENDERSTATE_SPECULARENABLE, False)
    Call d3dDevice.SetRenderState(D3DRENDERSTATE_AMBIENT, &H88888888)
    Call d3dDevice.SetRenderState(D3DRENDERSTATE_ZENABLE, False)
    
    ' Dump the global DirectX object now, it's no longer needed.
    Set dx = Nothing
End Sub

Private Sub UpdateFrame(Time As Single)
    Dim Verts(NUM_FLAG_INDICES) As D3DVERTEX
    Dim ix As Integer, iy As Integer
    Dim t As Single, u As Single
    
    On Local Error Resume Next
    
    ' Adjust the depth of each vertex in the vertex buffer, as
    ' a function of time, to make the flag "wave".
    With g_VertexBuffer
        Call .Lock(DDLOCK_WAIT)
        Call .GetVertices(0, NUM_FLAG_INDICES, Verts(0))
        
        For ix = 0 To FLAG_SIZE
            For iy = 0 To FLAG_SIZE
                Verts(ix + iy * (FLAG_SIZE + 1)).z = ix * 0.2 * Sin(ix - Time * 6) / (FLAG_SIZE + 1)
            Next iy
        Next ix
        
        ' Set the adjusted vertices in the vertex buffer.
        Call .SetVertices(0, NUM_FLAG_INDICES, Verts(0))
        Call .Unlock
    End With
    
    ' Move the clouds by updating the texture coordinates
    ' of the background polygons, as a function of time.
    t = Time / 40!
    u = ((t * 10000!) Mod 10000!) / 10000!
    g_tlvertBackground(0).tu = 0 - u
    g_tlvertBackground(1).tu = 0 - u
    g_tlvertBackground(2).tu = 1 - u
    g_tlvertBackground(3).tu = 1 - u
End Sub



Private Sub RenderFrame()
    Dim d3dDevice As Direct3DDevice7
    On Local Error Resume Next
    
    Set d3dDevice = IMCanvas1.Direct3DDevice
        
    ' Clear the previous frame.
    IMCanvas1.ClearBackSurface
    
    ' Render the scene.
    d3dDevice.BeginScene
    If Err.Number <> 0 Then
        Debug.Print "BeginScene failed."
        g_bReadyToRender = False
    End If
    If Err.Number = 0 And g_bReadyToRender = True Then
        ' Draw the background
        Call d3dDevice.SetTexture(0, g_BGTex)
        Call d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, _
                                     g_tlvertBackground(0), 4, 0)
                                     
        Call d3dDevice.SetRenderState(141, True)
        Call d3dDevice.LightEnable(0, True)
        
        ' Draw the pole
        Call d3dDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, _
                                     g_vertPoleVertices(0), 16, 0)
        
        ' Draw  the flag
        Call d3dDevice.SetTexture(0, g_FlagTex)
        Call d3dDevice.DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, _
                                              g_VertexBuffer, _
                                              0, NUM_FLAG_VERTICES, _
                                              g_iFlagIndices, _
                                              NUM_FLAG_INDICES, _
                                              D3DDP_WAIT)
    End If
    
    ' End the scene.
    d3dDevice.EndScene
    If Err.Number <> 0 Then
        Debug.Print "EndScene failed."
        g_bReadyToRender = False
    End If
    
    Set d3dDevice = Nothing
End Sub

Private Function MakeVertex(posVec As D3DVECTOR, normVec As D3DVECTOR, _
                            tu As Double, tv As Double) As D3DVERTEX

    Dim vertOut As D3DVERTEX
    
    With vertOut
        .x = posVec.x
        .y = posVec.y
        .z = posVec.x
        .nx = normVec.x
        .ny = normVec.y
        .nz = normVec.z
        .tu = tu
        .tv = tv
    End With
    
    MakeVertex = vertOut
End Function

Private Function MakeVector(a As Double, b As Double, c As Double) As D3DVECTOR
    Dim vecOut As D3DVECTOR
    
    With vecOut
        .x = a
        .y = b
        .z = c
    End With
    
    MakeVector = vecOut
End Function

Private Function RGBAToD3DCOLORVALUE(r As Single, g As Single, b As Single, a As Single) As D3DCOLORVALUE
    Dim c As D3DCOLORVALUE
    c.r = r
    c.g = g
    c.b = b
    c.a = a
    
    RGBAToD3DCOLORVALUE = c
End Function

Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    If Dir$(sFile) = "" Then
         ChDir App.Path
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub


Private Sub Form_Paint()
    ' If the application is being resized, clear the viewport
    ' to avoid graphic artifacts. Otherwise, do nothing.
    If g_bResizing = True Then
        IMCanvas1.ClearBackSurface
    End If
End Sub

'HANDLE RESIZE AND END CONDITIONS
Private Sub Form_Resize()
    If g_bResizing = True Then Exit Sub
    g_bResizing = True
    
    On Local Error Resume Next
    IMCanvas1.Width = Me.ScaleWidth
    IMCanvas1.Height = Me.ScaleHeight
    
    Cleanup
    IMCanvas1.StartWindowed
    
    InitD3D
    ' Force the Paint event to occur.
    Refresh
    g_bResizing = False
End Sub

Private Sub Form_Unload(Cancel As Integer)
    ' Set the global flag to terminate the loop in Form_Load
    g_bRunning = False
    End
End Sub

' Hang out in a sleep loop until we can restore
' our surfaces and continue rendering.
Private Sub SleepUntilReady()
    Debug.Print "Entered SleepUntilReady"
    
    Dim dd As DirectDraw7
    Set dd = IMCanvas1.DirectDraw
    
    ' If the display moide changed, we can't just restore the surfaces,
    ' they need to be completely recreated. Use the IMCanvas startup
    ' services to reinitialize everything.
     
    If dd.TestCooperativeLevel = DDERR_WRONGMODE Then
        Debug.Print "TestCooperativeLevel returned DDERR_WRONGMODE. Trying to recreate objects."
        Set dd = Nothing
        'IMCanvas1.RestoreDisplay
        IMCanvas1.InitWindowed "", ""
        
        
        ' Reinitialize Direct3D, the Vertex Buffer, and the viewport.
        InitD3D
        
        ' Reset the reference to reflect the new DirectDraw object.
        Set dd = IMCanvas1.DirectDraw
    End If
    
    While dd.TestCooperativeLevel <> DD_OK
        ' While waiting for the availability of our surfaces
        ' use the Sleep Win32 function to give up time slices
        ' to other processes.
        Debug.Print "Sleeping for 1/10th of a second. . . "
        Call Sleep(100)
    Wend
    
    ' If TestCooperativeLevel returns DD_OK, the surfaces we
    ' need are ready to be restored. We don't need to reload
    ' their contents because we're going to overwrite the contents
    ' of the render target surface; as for the bitmaps in the
    ' textures, they were created as managed textures, and are
    ' automatically restored and reloaded as needed by Direct3D.
    Debug.Print "Restoring surfaces."
    dd.RestoreAllSurfaces
    
    ' Reset the global flag to indicate that it's ok to
    ' start rendering again.
    g_bReadyToRender = True
    
    Set dd = Nothing
End Sub

Private Sub Cleanup()
    'all surfaces must be release before resize so that the
    'parent 3ddevice object can be released and a new one created
    Set g_FlagTex = Nothing
    Set g_BGTex = Nothing
    Set g_VertexBuffer = Nothing
End Sub

Private Sub IMCanvas1_NewDDraw()
    InitTextures
    InitViewportAndMaterials
End Sub





