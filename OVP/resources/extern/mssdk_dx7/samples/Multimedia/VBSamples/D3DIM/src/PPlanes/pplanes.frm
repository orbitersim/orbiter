VERSION 5.00
Object = "{D0B8DDCE-E796-11D2-A21E-00C04F68AD33}#1.1#0"; "IMControl.ocx"
Begin VB.Form PPlaneForm 
   Caption         =   "VB PPlanes"
   ClientHeight    =   4680
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   5385
   Icon            =   "pplanes.frx":0000
   LinkTopic       =   "Form2"
   ScaleHeight     =   312
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   359
   Begin DirectXIMControl.IMCanvas IMCanvas1 
      Height          =   4215
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   5055
      _ExtentX        =   8916
      _ExtentY        =   7435
   End
End
Attribute VB_Name = "PPlaneForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function GetWindowRect Lib "user32.dll" (ByVal hwnd As Long, ByRef r As RECT) As Long


Option Explicit

Const NUM_PLANES = 42
Const NUM_VERTEX = 36&
Const NUM_INDEX = 60&
Const GRID_WIDTH = 800&
Const NUM_GRID = 22&


'geometry
Dim m_planes(NUM_PLANES) As planedata
Dim m_pplane_indices(200) As Integer
Dim m_vPPlane(100) As D3DVERTEX
Dim m_grid(NUM_GRID * NUM_GRID) As D3DVERTEX
Dim m_nvert As Integer


Dim matPlaneInfo    As D3DMATERIAL7

'lighting
Dim m_light As D3DLIGHT7

'current 3d matrices
Dim m_world As D3DMATRIX
Dim m_proj As D3DMATRIX
Dim m_view As D3DMATRIX

'animation
Dim tic         As Single
Dim speed       As Single
Dim angle_tweak As Single
Dim running As Boolean

Dim m_bResizing As Boolean
Dim m_bInit As Boolean
Dim m_bInRender As Boolean
Dim m_bFullscreen As Boolean

Private Type planedata
     loc As D3DVECTOR         'current location of plane
     goal As D3DVECTOR        'where the plane is going to
     delta As D3DVECTOR       'unit vector of plane direction
     yaw As Single      'plane yaw
     pitch As Single    'plane pitch
     roll As Single     'plane roll
     dyaw As Single     'change in yaw
End Type


Private Sub Form_Load()
    If m_bInit Then End
    m_bInit = True
    
    If IMCanvas1.dx.SystemBpp <= 8 Then
        MsgBox "This sample was designed to run in High Color (16 bit) displays"
        End
    End If
    
    Dim b As Boolean
    Dim l
    
    m_bFullscreen = False
    Me.Show
            
    b = IMCanvas1.StartWindowed()
    If b = False Then
        MsgBox "Unable to initialize 3d device"
        End
    End If
    
    InitPPlaneData
    InitPPViewport
                
    running = True
    Dim fRestore As Boolean
    Do While running
	on local error resume next
        m_bInRender = True
        
        fRestore = False
        While IMCanvas1.DirectDraw.TestCooperativeLevel <> DD_OK
            fRestore = True
            DoEvents
        Wend
        If fRestore Then
            IMCanvas1.DirectDraw.RestoreAllSurfaces
            InitPPViewport
        End If
        DoFrame
        Render
        
        m_bInRender = False
        DoEvents
    Loop
    
    End
    
End Sub



Sub DrawPPlane(dev As Direct3DDevice7)
    Call dev.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, D3DFVF_VERTEX, m_vPPlane(0), NUM_VERTEX, m_pplane_indices, NUM_INDEX, 1)
End Sub
    


Sub InitPPlaneData()

    Dim i As Integer
    Dim j As Integer
    
        
    ' generate the paper plane data
    m_nvert = 0
    
    

   
    Dim planevert
    planevert = Array(0.125, 0.03125, 3.5, -0.25, 1, 0, 0.75, 0.1875, 3.25, -0.25, 1, 0, _
    2.75, 0.6875, -2, -0.25, 1, 0, 2, 0.5, -4.25, -0.25, 1, 0, _
    0.5, 0.125, -3.5, -0.25, 1, 0, 0.125, 0.03125, 3.5, 0.25, 1, 0, _
    0.75, 0.1875, 3.25, 0.25, 1, 0, 2.75, 0.6875, -2, 0.25, 1, 0, _
    2, 0.5, -4.25, 0.25, 1, 0, 0.5, 0.125, -3.5, 0.25, 1, 0, _
   -0.125, 0.03125, 3.5, -0.25, 1, 0, -0.75, 0.1875, 3.25, -0.25, 1, 0, _
    -2.75, 0.6875, -2, -0.25, 1, 0, -2, 0.5, -4.25, -0.25, 1, 0, _
    -0.5, 0.125, -3.5, -0.25, 1, 0, -0.125, 0.03125, 3.5, 0.25, 1, 0, _
    -0.75, 0.1875, 3.25, 0.25, 1, 0, -2.75, 0.6875, -2, 0.25, 1, 0, _
    -2, 0.5, -4.25, 0.25, 1, 0, -0.5, 0.125, -3.5, 0.25, 1, 0, _
    0.125, 0.03125, 3.5, 1, -0.1, 0, 0.5, 0.125, -3.5, 1, -0.1, 0, _
    0, -2.25, -2.75, 1, -0.1, 0, 0, -0.5, 3.625, 1, -0.1, 0, _
    0.125, 0.03125, 3.5, -1, 0.1, 0, 0.5, 0.125, -3.5, -1, 0.1, 0, _
    0, -2.25, -2.75, -1, 0.1, 0, 0, -0.5, 3.625, -1, 0.1, 0, _
    -0.125, 0.03125, 3.5, -1, -0.1, 0, -0.5, 0.125, -3.5, -1, -0.1, 0, _
    0, -2.25, -2.75, -1, -0.1, 0, 0, -0.5, 3.625, -1, -0.1, 0, _
    -0.125, 0.03125, 3.5, 1, 0.1, 0, -0.5, 0.125, -3.5, 1, 0.1, 0, _
    0, -2.25, -2.75, 1, 0.1, 0, 0, -0.5, 3.625, 1, 0.1, 0)

   j = 0
    For i = 0 To NUM_VERTEX - 1
        With m_vPPlane(i)
            .x = planevert(j)
            .y = planevert(j + 1)
            .z = planevert(j + 2)
            .nx = planevert(j + 3)
            .ny = planevert(j + 4)
            .nz = planevert(j + 5)
            
            j = j + 6
        End With
        
    Next
   

    Dim plane
    plane = Array(0, 1, 4, 1, 2, 4, 4, 2, 3, _
    5, 9, 6, 6, 9, 7, 7, 9, 8, _
    10, 11, 14, 11, 12, 14, 14, 12, 13, _
    15, 19, 16, 16, 19, 17, 17, 19, 18, _
    20, 23, 21, 21, 23, 22, _
    24, 25, 27, 25, 26, 27, _
    28, 29, 31, 29, 30, 31, _
    32, 35, 33, 33, 35, 34)
    
    For i = 0 To UBound(plane)
        m_pplane_indices(i) = plane(i)
    Next
    
    For i = 0 To NUM_PLANES - 1
                
        
        m_planes(i).loc.x = 0#
        m_planes(i).loc.y = 0#
        m_planes(i).loc.z = 0#
        
        m_planes(i).goal.x = 10# * (Rnd() - Rnd())
        m_planes(i).goal.y = 10# * (Rnd() - Rnd())
        m_planes(i).goal.z = 10# * (Rnd() - Rnd())
        
        m_planes(i).delta.x = 0
        m_planes(i).delta.y = 0
        m_planes(i).delta.z = 1
        
        m_planes(i).yaw = 0#
        m_planes(i).pitch = 0#
        m_planes(i).roll = 0#
        m_planes(i).dyaw = 0#
    Next
    
    
    Dim size As Single
    Dim offset As Single
    Dim v As D3DVERTEX
    Dim n As D3DVECTOR
    
    offset = GRID_WIDTH / 2
    size = GRID_WIDTH / (NUM_GRID - 1#)
    
    For i = 0 To NUM_GRID - 1
        For j = 0 To NUM_GRID - 1
            With m_grid(j + i * NUM_GRID)
            .x = i * size - offset
            .y = 0#
            .z = j * size - offset
            .nx = 0
            .ny = 1
            .nz = 0
            End With
        Next
    Next
            

       
    speed = 2
    angle_tweak = 0.02
     
End Sub

Sub DoFrame()
    
    Dim dev As Direct3DDevice7
    
    Dim rSrc As RECT
    Dim i As Integer
    Dim recs(1) As D3DRECT
    
    Set dev = IMCanvas1.Direct3DDevice
    
    If dev Is Nothing Then Exit Sub
    
    
    IMCanvas1.ClearBackSurface
    
    
    dev.BeginScene
    
    dev.SetMaterial matPlaneInfo
    
    Call ProjectionMatrix(m_proj, 10#, 500#, 3.141592 / 2#)
    Call dev.SetTransform(D3DTRANSFORMSTATE_PROJECTION, m_proj)
        
    Call DrawGrid(dev)
    Call PlayWithPlanes(dev)
    


    dev.EndScene
           
                          
End Sub

Sub Render()
    Dim r As RECT
    
    IMCanvas1.Update
End Sub

Sub PlayWithPlanes(dev As Direct3DDevice7)
    Dim offset      As D3DVECTOR
    Dim loc         As D3DVECTOR
    Dim goal        As D3DVECTOR
    Dim delta       As D3DVECTOR
    Dim result      As D3DVECTOR
    Dim from        As D3DVECTOR
    Dim at          As D3DVECTOR
    Dim local_up    As D3DVECTOR
    Dim up          As D3DVECTOR

    Dim dot         As Single
    Dim pitch       As Single
    Dim yaw         As Single
    Dim roll        As Single
    Dim dyaw         As Single
    Dim RotZMatrix As D3DMATRIX
    Dim RotYMatrix As D3DMATRIX
    Dim RotXMatrix As D3DMATRIX
    Dim RotYXMatrix As D3DMATRIX
    Dim RotYXZMatrix As D3DMATRIX
    Dim transMatrix As D3DMATRIX
    
    Dim i As Integer
    
                    
    
    up.y = 1
    
    For i = 0 To NUM_PLANES - 1
                    
        With m_planes(i)
            VectorCopy loc, .loc
            VectorCopy goal, .goal
            VectorCopy delta, .delta
            pitch = .pitch
            yaw = .yaw
            roll = .roll
            dyaw = .dyaw
        End With
        
        ' tweek orientation based on last position and goal
        Call VectorSubtract(offset, goal, loc)
        
        
        ' first, tweak the pitch
        If (offset.y > 1) Then
            pitch = pitch + angle_tweak
            If (pitch > 0.8) Then pitch = 0.8
        ElseIf (offset.y < -1) Then
            pitch = pitch - angle_tweak
            If (pitch < -0.8) Then pitch = -0.8
        Else
            ' add damping
            pitch = pitch * 0.95
        End If

        ' now figure out yaw changes in xz plane
        offset.y = 0#
        delta.y = 0#
    
        Call VectorNormalize(delta)
        Call VectorNormalize(offset)
        dot = VectorDotProduct(offset, delta)
        Call VectorCrossProduct(result, offset, delta)
        Call VectorCopy(offset, result)
        
         
        dot = ((1# - dot) / 2#) * angle_tweak * 10#
        If (offset.y > 0.01) Then
            dyaw = (dyaw * 9# + dot) * 0.1
        ElseIf (offset.y < 0.01) Then
            dyaw = (dyaw * 9# - dot) * 0.1
        End If
        
        yaw = yaw + dyaw
        roll = -dyaw * 9#

        '5% chance of changing direction
        If (Rnd() < 0.05) Then
            goal.x = 60# * (Rnd() - Rnd())
            goal.y = 60# * (Rnd() - Rnd())
            goal.z = Rnd() * 300# - 100#
        End If

        ' build the an orientation matrix for the pplane
        Call RotateZMatrix(RotZMatrix, roll)
        Call RotateXMatrix(RotXMatrix, pitch)
        Call RotateYMatrix(RotYMatrix, yaw)
        Call MatrixMult(RotYXMatrix, RotXMatrix, RotYMatrix)
        Call MatrixMult(RotYXZMatrix, RotZMatrix, RotYXMatrix)
              
        delta.x = RotYXZMatrix.rc31
        delta.y = RotYXZMatrix.rc32
        delta.z = RotYXZMatrix.rc33
        
                
        
        ' update position
        'LOC = 1*LOC + speed * delta
        Call VectorAddAndScale(loc, 1, loc, speed, delta)
                
        'save info back into array
        With m_planes(i)
            VectorCopy .loc, loc
            VectorCopy .delta, delta
            VectorCopy .goal, goal
            
            .yaw = yaw
            .dyaw = dyaw
            .roll = roll
            .pitch = pitch
        End With
        
                
        ' before we draw the first plane use it's position to update the camera
        If (i = 0) Then
            local_up.x = RotYXZMatrix.rc21
            local_up.y = RotYXZMatrix.rc22
            local_up.z = RotYXZMatrix.rc23
            Call VectorAddAndScale(from, 1, loc, -20, delta)
            Call VectorAddAndScale(from, 1, from, 3, local_up)
            Call VectorAdd(at, loc, delta)
                                
            Call ViewMatrix(m_view, from, at, up, roll)
            Call dev.SetTransform(D3DTRANSFORMSTATE_VIEW, m_view)
            
        End If
        
        ' first translate into place, then set orientation
        Call TranslateMatrix(transMatrix, loc)
        Call MatrixMult(m_world, RotYXZMatrix, transMatrix)
                
        
        ' apply the world matrix
        Call dev.SetTransform(D3DTRANSFORMSTATE_WORLD, m_world)

        ' display the pplane
        Call DrawPPlane(dev)
        
        
       
    Next    ' end of loop for each pplane
 
End Sub


Sub DrawGrid(dev As Direct3DDevice7)
    Dim world As D3DMATRIX
    Dim rotMat As D3DMATRIX

    
    
    Dim v As D3DVECTOR
   
    v.x = 0
    v.y = -60
    v.z = 0
    Call TranslateMatrix(world, v)
    Call dev.SetTransform(D3DTRANSFORMSTATE_WORLD, world)
    
         
    Call dev.DrawPrimitive(D3DPT_LINELIST, D3DFVF_VERTEX, m_grid(0), NUM_GRID * NUM_GRID, 1)
    
    Call RotateYMatrix(rotMat, 3.141592 / 2)
    Call MatrixMult(world, rotMat, world)
    Call dev.SetTransform(D3DTRANSFORMSTATE_WORLD, world)
        
    Call dev.DrawPrimitive(D3DPT_LINELIST, D3DFVF_VERTEX, m_grid(0), NUM_GRID * NUM_GRID, 1)
    
End Sub


  

Sub InitPPViewport()

    
    
    Dim d3d             As Direct3D7
    Dim dev             As Direct3DDevice7
        
    Set d3d = IMCanvas1.Direct3d
    Set dev = IMCanvas1.Direct3DDevice
    

    ' Create and set up the background material
    
    Dim c As D3DCOLORVALUE
    
    

    ' create and set up the plane material
    With c
        .a = 1#
        .r = 0.6
        .g = 0.6
        .b = 0.6
    End With
        matPlaneInfo.Ambient = c
    With c
        .a = 1
        .r = 0.5
        .g = 0.5
        .b = 0.5
    End With
        matPlaneInfo.diffuse = c
    With c
        .a = 1#
        .r = 0.5
        .g = 0.5
        .b = 0.5
    End With
    matPlaneInfo.specular = c
    matPlaneInfo.emissive = c
    
    dev.SetMaterial matPlaneInfo

    Call ProjectionMatrix(m_proj, 1#, 1000#, 90#)
    Call IdentityMatrix(m_world)

    ' set up the light
    m_light.dltType = D3DLIGHT_POINT
    
    
    With c
        .a = 1
        .r = 0.5
        .g = 0.5
        .b = 0.5
    
    End With
    With m_light
        .dltType = D3DLIGHT_DIRECTIONAL
        .Ambient = c
        .diffuse = c
        .specular = c
    End With
    
    
    
    ' position light behind viewer
    m_light.position.x = 0#
    m_light.position.y = 1000#
    m_light.position.z = -100#
    m_light.direction.x = -1
    m_light.direction.y = -1
    m_light.direction.z = 1
    
    
    dev.SetLight 0, m_light
    dev.LightEnable 0, True
    
    IMCanvas1.BackBufferClearValue = &H4040FF
    
    
End Sub




'HANDLE RESIZE AND END CONDITIONS
Private Sub Form_Resize()
    
    
    
    On Local Error Resume Next
    
    IMCanvas1.Width = Me.ScaleWidth
    IMCanvas1.Height = Me.ScaleHeight
    
    If running = False Then Exit Sub
    
    If m_bFullscreen = False Then
        IMCanvas1.StartWindowed
    End If
    InitPPViewport
        
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
    running = False
End Sub


Private Sub IMCanvas1_NewDDraw()
    InitPPViewport
End Sub
