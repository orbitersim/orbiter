VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.0#0"; "RMCONTROL.OCX"
Begin VB.Form RMFlyForm 
   Caption         =   "VB RM Fly"
   ClientHeight    =   4020
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6285
   Icon            =   "fly.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   268
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   419
   StartUpPosition =   3  'Windows Default
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   3948
      Left            =   24
      TabIndex        =   0
      Top             =   24
      Width           =   6192
      _ExtentX        =   10927
      _ExtentY        =   6959
   End
End
Attribute VB_Name = "RMFlyForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'
'  The RMCanvas control now will let you use various properties after intialization
'
'  Most relevant to Retained Mode users are
'
'  RMCanvas1.D3DRM
'           allows access to the root RM object
'           it is from this object we can create new objects for exanple
'           set f=RMCanvas1.D3DRM.createFrame(RMCanvas1.D3DRMFrameScene)
'           set mb = RMCanvas1.D3DRM.createMeshBuilder()
'    '
'  About frames and meshbuilders
'           which are visible to the ocx. A frame is a container for geometry
'           a frame has variable position and orientation but geometry
'           have fixed position and orientation. Meshbuilders hold geometry and
'           can be afixed to frames so they can be placed and oriented.
'
'
'           Other frames can be attached to frames (know as a scene graph)
'           When a frame moves all frames and mesh builders attached to it
'           move the same way. However the attached frames may move on their
'           own relative to their parent. A good analogy is the solar system
'
'           The sun is the root frame, attached to it is a child frame which holds
'           the earth frame.
'
'           Attached to the earth frame is geometry of a smaller sphere
'
'           Also attached is another child frame for the moon which holds the moon
'           geometry
'
'           Imagine the sun frame rotating which makes the earth frame and everything
'           in the earth frame rotate in an orbit around the sun.
'
'           The earth frame can rotates inside the sun frame, rotating the earth sphere
'           and also the moon frame.
'
'           The moon frame doesnt have to rotate but just hold the smallest sphere to represent
'           the moon. It will be rotated around the earth by the earths frame rotation
'
'
'  RMCanvas1.SceneFrame
'           this object is the parent for all objects (frame objects in particular)
'           this is where we build our world
'
'  RMCanvas1.CameraFrame
'           this object is a child of the scene. Its position and orientation
'           determine where the camera is at and what direction its looking toward
'           By default the camera is -10 units back along the z axis looking toward
'           0,0,0
'
'  RMCanvas1.DirLightFrame
'           there are 2 default lights that are setup for you. One is an ambient light
'           which is omnidirectional and does not have location or direction. The other
'           is a directional light. This frame is a child of the scene and holds the
'           directional light. You can use functions like setPosition and lookAt to
'           position and orient the light. By default the light is at 5,5,5 looking
'           toward 0,0,0
'
'  RMCanvas1.DirLight
'           This is the light object placed in the FrameDirLight frame
'           you can use the setColorRGB method to change the lights color
'
'  RMCanvas1.AmbientLight
'           This determine how much outdoor type light is in the scene.
'           use the setColorRGB to set the color and intensity of the light
'           be aware the white will white out our scene so use low levels
'           of grey
'
' RMCanvas1.Viewport
'           The viewport object describes how the camera works
'           the setField method is used to determine how narrow an
'           area you are looking at.
'           The setFront and setBack methods are used to define how far
'           away you would like to see to and how close an object you can see
'           picking objects is also done through the viewport interface
'
'  RMCanvas1.Device
'           The device object can control this such as quality
'           SetQuality can be use to change your scene from wire frame
'           to flat shaded to smooth shaded (gauraud)
'
'  RMCanvas1.SceneSpeed
'           (in units per second) can be set to adjust how rotation
'           and velocity effect an object. The default is 30 units/second
'
'
'  Other properties that let you draw ontop of the 3d scence
'  are also available (DDBackSurface for example). Use the
'


Dim m_planeFrame As Direct3DRMFrame3
Dim m_chaseFrame As Direct3DRMFrame3
Dim m_flightAnim As Direct3DRMAnimation2
Dim m_time As Single
Dim m_bRetDown As Boolean
Dim m_bAltDown As Boolean
Dim m_bRunning As Boolean


Private Sub Form_Load()
    
    Static b As Boolean
    If b = True Then End
    b = True
    Me.Show
    DoEvents
    init
    End

End Sub

'- Add Handler to resize the ActiveX control when form resizes
Private Sub Form_Resize()
    
    RMCanvas1.Width = Me.ScaleWidth
    RMCanvas1.Height = Me.ScaleHeight
End Sub


'
Sub init()
    
    Dim b As Boolean
    Dim sFile As String
        
    
    
   
    
    '  We want to run a windowed application
    '
    '  by default StartWindowed will try and find 3d hardware
    '  on your primary display. If it doesnt find it,
    '  it will default to using the slower sofware
    '  RGB rasterizer
    '
    b = RMCanvas1.StartWindowed
    If b = False Then
        MsgBox "problem starting DirectX RM"
        End
    End If
    
    InitScene
    
    RenderLoop
    
End Sub


Sub InitScene()
    
    
    FindMediaDir "land4.x"
            
    CreateLandScape
    CreatePlaneAndChaseFrame
    CreatePathAnimation
    
    
  
End Sub

Sub CreateLandScape()
    Dim mbLand As Direct3DRMMeshBuilder3
    Dim fLand As Direct3DRMFrame3
    Dim box As D3DRMBOX
    Dim i As Integer, j As Integer
    
    '- Create a Frame object which is parented to the scene
    Set fLand = RMCanvas1.D3DRM.CreateFrame(RMCanvas1.SceneFrame)
        
    '- Create an empty meshbuilder object
    Set mbLand = RMCanvas1.D3DRM.CreateMeshBuilder()
                
    '- Load land geometry from a file into the meshbuilder and attach it to the frame
    mbLand.LoadFromFile "land4.x", 0, 0, Nothing, Nothing
    fLand.AddVisual mbLand
    
    '- scale the land to be larger and get its extent
    mbLand.ScaleMesh 10, 8, 10
    mbLand.GetBox box
    
    
    Dim range As Single
    
        
    
    range = box.Max.y - box.Min.y
    
    
        
    
    RMCanvas1.SceneFrame.SetSceneBackground &H6060E0
    RMCanvas1.AmbientLight.SetColorRGB 0.36, 0.36, 0.36
    'color faces acording to height
    Dim vert As D3DVECTOR, norm As D3DVECTOR, y As Single
    For i = 0 To mbLand.GetFaceCount() - 1
        y = box.Min.y
        For j = 0 To mbLand.GetFace(i).GetVertexCount() - 1
            mbLand.GetFace(i).GetVertex j, vert, norm
            If vert.y > y Then y = vert.y
        Next
        If (y - box.Min.y) / range < 0.05 Then
            Call mbLand.GetFace(i).SetColorRGB((y - box.Min.y) / range, 0.6, 1 - (y - box.Min.y) / range)
        Else
            Call mbLand.GetFace(i).SetColorRGB(0.2 + (y - box.Min.y) / range, 1 - (y - box.Min.y) / range, 0.5)
        End If
    Next
    
                       
End Sub

Sub CreatePlaneAndChaseFrame()
    Dim mbPlane As Direct3DRMMeshBuilder3
    Set mbPlane = RMCanvas1.D3DRM.CreateMeshBuilder()
    mbPlane.LoadFromFile "dropship.x", 0, 0, Nothing, Nothing
    mbPlane.ScaleMesh 0.015, 0.008, 0.015
    mbPlane.SetColorRGB 0.8, 0.8, 0.8
    Set m_planeFrame = RMCanvas1.D3DRM.CreateFrame(RMCanvas1.SceneFrame)
    m_planeFrame.AddVisual mbPlane
    
    Set m_chaseFrame = RMCanvas1.D3DRM.CreateFrame(RMCanvas1.SceneFrame)
    
    
    Dim verts(1000) As D3DRMVERTEX
    
    
End Sub

Sub CreatePathAnimation()
    Dim pathdata
    Dim x As Single, y As Single, z As Single, i As Integer
    
    pathdata = Array( _
            -8, 3, -12, _
            -4, 2, -8, _
            -2, 0, -4, _
             9, -1, 7, _
             4, 6, 10, _
            -4, 5, 9, _
             5.5, 3.5, -6.5, _
             2, 5, -10, _
             0, 4, -15, _
            -5, 4, -15, _
            -8, 3, -12)
                 
    
    Set m_flightAnim = RMCanvas1.D3DRM.CreateAnimation()
    
    m_flightAnim.SetOptions D3DRMANIMATION_CLOSED Or D3DRMANIMATION_SPLINEPOSITION Or D3DRMANIMATION_POSITION
    
    Dim key As D3DRMANIMATIONKEY
    
    For i = 0 To 10
        x = pathdata(i * 3)
        y = pathdata(i * 3 + 1)
        z = pathdata(i * 3 + 2)
        'm_flightAnim.AddPositionKey i, x, y, z
        
        key.dvX = x
        key.dvY = y
        key.dvZ = z
        key.lKeyType = 3
        key.dvTime = i
        m_flightAnim.AddKey key
        
    Next
    
End Sub

Sub UpdatePlaneAndCamera(delta As Single)
    Dim dir As D3DVECTOR
    Dim up As D3DVECTOR
    Dim dirCam As D3DVECTOR
    Dim upCam As D3DVECTOR
    Dim a_bit As Single
    
    RMCanvas1.SceneSpeed = 1
    
    m_time = m_time + delta
    
    'set up camera frame position
    m_flightAnim.SetFrame RMCanvas1.CameraFrame
    m_flightAnim.SetTime m_time + 0
    
    'set up plane frame position
    m_flightAnim.SetFrame m_planeFrame
    m_flightAnim.SetTime m_time + 0.5
    
    'set up chase frame
    m_flightAnim.SetFrame m_chaseFrame
    m_flightAnim.SetTime m_time + 1
    
    'orient the camera to look at the plane
    RMCanvas1.CameraFrame.LookAt m_planeFrame, Nothing, D3DRMCONSTRAIN_Z
    
    'orient the plane to look at the chase frame
    m_planeFrame.LookAt m_chaseFrame, Nothing, D3DRMCONSTRAIN_Y
            
                
    'figure out the bank for the plane
    RMCanvas1.CameraFrame.GetOrientation Nothing, dirCam, upCam
    m_planeFrame.GetOrientation Nothing, dir, up
        
    up.x = dir.x - dirCam.x
    up.y = dir.y - dirCam.y + 1#
    up.z = dir.z - dirCam.z
        
    m_planeFrame.SetOrientation Nothing, dir.x, dir.y, dir.z, up.x, up.y, up.z
    

    


End Sub

Private Sub Form_Unload(Cancel As Integer)
    m_bRunning = False
End Sub


Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub

Sub RenderLoop()

    
    '- RenderLoop
    '  a tight infinite loop will keep the frame right high
    '  but hog the CPU .. for OCX design and non graphics centric applications
    '  use a Timer to update the scene, allowing more of the CPU open
    '  to other tasks
    '  Note we use the unload event to break out of this loop with an end
    '
    m_bRunning = True
    Do While m_bRunning
        
        '- We update the scene.
        RMCanvas1.Update
        
        '- Doevents is necessary to allow events (such as key down, click)
        '  to be processed
        DoEvents
        
    Loop
    
End Sub



Private Sub RMCanvas1_SceneMove(delta As Single)
    UpdatePlaneAndCamera delta
End Sub

