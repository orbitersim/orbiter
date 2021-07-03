VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.0#0"; "RMCONTROL.OCX"
Begin VB.Form DxBot 
   Caption         =   "Form1"
   ClientHeight    =   5055
   ClientLeft      =   1455
   ClientTop       =   1560
   ClientWidth     =   6750
   Icon            =   "DxBot.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   337
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   450
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   4770
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6330
      _ExtentX        =   11165
      _ExtentY        =   8414
   End
End
Attribute VB_Name = "DxBot"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



Private m_botAnimation As Direct3DRMAnimationSet2
Private m_botFrame As Direct3DRMFrame3
Private m_time As Single
Private m_running As Boolean

' DxBot init code loads castle file and displays it
'
Sub init()


    Dim hr As Long
    
    
    '- initialize this form to use 3d
    b = RMCanvas1.StartWindowed
    If (b = False) Then
        MsgBox "Unable to start 3d window"
        Exit Sub
    End If
    
    
    ' setting the background color to grey
    RMCanvas1.SceneFrame.SetSceneBackgroundRGB 0.5, 0.5, 0.8
                
    
    '- load
    '  have it have it positioned at the root.
    
    Me.Caption = "Loading ..."
    
    FindMediaDir "skmech.x"
    
    '- create a new frame and load the geometry and first animation
    Set m_botFrame = RMCanvas1.D3DRM.CreateFrame(RMCanvas1.SceneFrame)
    Set m_botAnimation = RMCanvas1.D3DRM.CreateAnimationSet()
    m_botAnimation.LoadFromFile "skmech.x", 0, 0, Nothing, Nothing, m_botFrame
                            
    
    'position things so we can see the bot
    m_botFrame.SetOrientation Nothing, 0, 0, -1, 0, 1, 0
    m_botFrame.AddScale D3DRMCOMBINE_BEFORE, 0.3, 0.3, 0.3
    RMCanvas1.CameraFrame.SetPosition Nothing, 0, 0, -100
    RMCanvas1.Viewport.SetBack 1000
    
    '- d3d makes lots of computations of first render
    RMCanvas1.Update

    Me.Caption = "DxBot"

    '- start the render loop
    RMCanvas1.SceneSpeed = 30
    
    m_running = True
    Do While m_running
        RMCanvas1.Update
        DoEvents
    Loop
End Sub
    
    
    

    
Private Sub Form_Load()
    Static b As Boolean
    If b = True Then End
    b = True
    
    Me.Show
    init
    
    End
End Sub


Private Sub Form_Resize()
    On Local Error Resume Next
    RMCanvas1.Width = Me.ScaleWidth
    RMCanvas1.Height = Me.ScaleHeight
    RMCanvas1.Viewport.SetBack 1000
End Sub


Private Sub Form_Unload(Cancel As Integer)
    m_running = False
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

Private Sub RMCanvas1_SceneMove(delta As Single)
        m_time = m_time + delta
        m_botAnimation.SetTime m_time
End Sub
