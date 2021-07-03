VERSION 5.00
Begin VB.Form EggForm 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Advanced Retained Mode Tutorial 1"
   ClientHeight    =   4710
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5535
   Icon            =   "egg.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4710
   ScaleWidth      =   5535
   ShowInTaskbar   =   1   'True
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture1 
      Height          =   4695
      Left            =   0
      ScaleHeight     =   309
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   365
      TabIndex        =   0
      Top             =   0
      Width           =   5535
   End
End
Attribute VB_Name = "EggForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit


Dim g_dx As New DirectX7
Dim m_dd As DirectDraw7
Dim m_ddClipper As DirectDrawClipper
Dim m_rm As Direct3DRM3

Dim m_rmDevice As Direct3DRMDevice3
Dim m_rmViewport As Direct3DRMViewport2

Dim m_rootFrame As Direct3DRMFrame3
Dim m_lightFrame As Direct3DRMFrame3
Dim m_cameraFrame As Direct3DRMFrame3
Dim m_objectFrame As Direct3DRMFrame3
Dim m_uvFrame As Direct3DRMFrame3
Dim m_light As Direct3DRMLight
Dim m_meshBuilder As Direct3DRMMeshBuilder3
Dim m_object As Direct3DRMMeshBuilder3

Dim m_width As Long
Dim m_height As Long
Dim m_running As Boolean
Dim m_finished As Boolean


Private Sub Form_Load()
        Show
        DoEvents
        InitRM
        FindMediaDir "egg.x"
        InitScene "egg.x"
        RenderLoop
        CleanUp
        End
End Sub

Sub CleanUp()
    m_running = False
    
    Exit Sub
    Set m_light = Nothing
    Set m_meshBuilder = Nothing
    Set m_object = Nothing

    Set m_lightFrame = Nothing
    Set m_cameraFrame = Nothing
    Set m_objectFrame = Nothing
    Set m_rootFrame = Nothing

    Set m_rmDevice = Nothing
    Set m_ddClipper = Nothing
    Set m_rm = Nothing
    Set m_dd = Nothing
 
End Sub

Sub InitRM()


    'Create Direct Draw From Current Display Mode
    Set m_dd = g_dx.DirectDrawCreate("")
    
    'Create new clipper object and associate it with a window'
    Set m_ddClipper = m_dd.CreateClipper(0)
    m_ddClipper.SetHWnd Picture1.hWnd
        
    
    'save the widht and height of the picture in pixels
    m_width = Picture1.ScaleWidth
    m_height = Picture1.ScaleHeight
    
    'Create the Retained Mode object
    Set m_rm = g_dx.Direct3DRMCreate()

    
    'Create the Retained Mode device to draw to
    Set m_rmDevice = m_rm.CreateDeviceFromClipper(m_ddClipper, "", m_width, m_height)
    
    m_rmDevice.SetQuality D3DRMRENDER_GOURAUD
    
End Sub

Sub InitScene(sMesh As String)


    'Setup a scene graph with a camera light and object
    Set m_rootFrame = m_rm.CreateFrame(Nothing)
    Set m_cameraFrame = m_rm.CreateFrame(m_rootFrame)
    Set m_lightFrame = m_rm.CreateFrame(m_rootFrame)
    Set m_objectFrame = m_rm.CreateFrame(m_rootFrame)
    
    'position the camera and create the Viewport
    'provide the device thre viewport uses to render, the frame whose orientation and position
    'is used to determine the camera, and a rectangle describing the extents of the viewport
    m_cameraFrame.SetPosition Nothing, 0, 0, -10
    Set m_rmViewport = m_rm.CreateViewport(m_rmDevice, m_cameraFrame, 0, 0, m_width, m_height)
    
    
    'create a white light and hang it off the light frame
    Set m_light = m_rm.CreateLight(D3DRMLIGHT_DIRECTIONAL, &HFFFFFFFF)
    m_lightFrame.AddLight m_light
    
    'For this sample we will load x files with geometry only
    'so create a meshbuilder object
    Set m_meshBuilder = m_rm.CreateMeshBuilder()
    m_meshBuilder.LoadFromFile sMesh, 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
    
    'add the meshbuilder to the scene graph
    m_objectFrame.AddVisual m_meshBuilder
    
    'Have the object rotating
    m_objectFrame.SetRotation Nothing, 1, 1, 1, 0.05
    
    
End Sub

Sub RenderLoop()
    Dim t1 As Long
    Dim t2 As Long
    
    Dim delta As Single
    On Local Error Resume Next
    m_running = True
    t1 = g_dx.TickCount()
    Do While m_running = True
        t2 = g_dx.TickCount()
        delta = (t2 - t1) / 10
        t1 = t2
        m_rootFrame.Move delta  'increment velocities
        m_rmViewport.Clear D3DRMCLEAR_ALL    'clear the rendering surface rectangle described by the viewport
        m_rmViewport.Render m_rootFrame 'render to the device
        FixFloat
        m_rmDevice.Update   'blt the image to the screen
        DoEvents    'allows events to be processed even though we are in a tight loop
    Loop
End Sub

Sub FixFloat()
    On Local Error Resume Next
    Dim l As Single
    l = 6
    
    l = l / 0
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    CleanUp
    End
End Sub


Private Sub Picture1_Paint()
    On Local Error Resume Next
    m_rmDevice.HandlePaint Picture1.hDC
End Sub

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

