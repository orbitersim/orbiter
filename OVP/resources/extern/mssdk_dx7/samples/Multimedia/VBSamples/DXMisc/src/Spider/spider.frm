VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.0#0"; "RMCONTROL.OCX"
Begin VB.Form SpiderForm 
   Caption         =   "Spider"
   ClientHeight    =   8955
   ClientLeft      =   30
   ClientTop       =   1545
   ClientWidth     =   9690
   Icon            =   "spider.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   597
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   646
   Begin VB.CommandButton CMDPlay 
      Caption         =   "Play"
      Height          =   375
      Left            =   240
      TabIndex        =   3
      Top             =   0
      Width           =   735
   End
   Begin VB.CommandButton CMDStop 
      Caption         =   "Stop"
      Height          =   375
      Left            =   1080
      TabIndex        =   2
      Top             =   0
      Width           =   735
   End
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   8655
      Left            =   0
      TabIndex        =   0
      Top             =   360
      Width           =   9735
      _ExtentX        =   17171
      _ExtentY        =   15266
      FullScreenWidth =   0
      FullScreenHeight=   0
      FullScreenBpp   =   0
      SceneSpeed      =   30
   End
   Begin VB.Timer Timer1 
      Left            =   7320
      Top             =   1680
   End
   Begin MSComctlLib.Slider Slider1 
      Height          =   375
      Left            =   1800
      TabIndex        =   1
      Top             =   0
      Width           =   4095
      _ExtentX        =   7223
      _ExtentY        =   661
      _Version        =   393216
      Min             =   1
      Max             =   8
      SelStart        =   4
      Value           =   4
   End
End
Attribute VB_Name = "SpiderForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim dx As New DirectX7


Dim D3D As Direct3DRM3
Dim pi As Single, pio2 As Single, twopi As Single
Dim vX As D3DVECTOR, vY As D3DVECTOR, vZ As D3DVECTOR

'Mouse control vars
Dim m_bMouseDown As Boolean
Dim m_lastx As Integer
Dim m_lasty As Integer

'This is the spider object
Dim Alt As Single           ' Height of body above terrain
Dim iLeg, NumLegs As Integer
Dim PrismMesh As Direct3DRMMeshBuilder3
Dim ToeConeMesh As Direct3DRMMeshBuilder3
Dim BugMesh As Direct3DRMMeshBuilder3
Dim BugFrame As Direct3DRMFrame3
Dim BugPos As D3DVECTOR
Dim BasePt  As D3DVECTOR
Dim Span As D3DVECTOR
Dim Seg0Frame(8) As Direct3DRMFrame3
Dim Seg1Frame(8) As Direct3DRMFrame3
Dim Seg2Frame(8) As Direct3DRMFrame3

Dim scene As Direct3DRMFrame3

' light stuff

Dim LightFrame1 As Direct3DRMFrame3
Dim lInterpolator As Direct3DRMLightInterpolator
Dim Light1 As Direct3DRMLight
Dim LightIndex As Integer


'Music Control variables
Dim perf As DirectMusicPerformance
Dim seg As DirectMusicSegment
Dim sty As DirectMusicStyle
Dim seg2 As DirectMusicSegment
Dim loader As DirectMusicLoader
Dim notification As Long
Dim GrooveLevel As Byte
Dim mtNow As Long
Implements DirectXEvent
Dim w As Integer
Dim a As Boolean
Dim b As Boolean
Dim Delay As Integer
Dim TotalMotifs As Integer
Dim CurrentMotif As Integer
'BasePos and RestPos don't ever change at run-time
'so they could have come from the model
'Goalpos gets changed by the stepping logic
Dim RestPos(8)  As D3DVECTOR  ' ideal (resting) position of toes in BugCoords
Dim BasePos(8)  As D3DVECTOR  ' positions of leg roots in BugCoords
Dim GoalPos(8)  As D3DVECTOR  ' toe goal points in wc


Private Sub CMDPlay_Click()
    Call perf.PlaySegment(seg, 0, 0)
End Sub

Private Sub CMDStop_Click()
    Call perf.Stop(seg, Nothing, 0, 0)
End Sub

Private Sub Form_Resize()
    RMCanvas1.Width = Me.ScaleWidth
    RMCanvas1.Height = Me.ScaleHeight
    
End Sub

Private Sub RMCanvas1_KeyDown(keyCode As Integer, Shift As Integer)


   Select Case keyCode
    Case Asc("F")
        Ax3d1.Device.EnterFullScreenMode 640, 480, 16
    Case vbKeyLeft
        BugFrame.AddRotation 1, 0, 1, 0, -0.05
    Case vbKeyRight
        BugFrame.AddRotation 1, 0, 1, 0, 0.05
    Case vbKeyUp
        BugFrame.AddTranslation 1, 0, 0, 0.1
    Case vbKeyDown
        BugFrame.AddTranslation 1, 0, 0, -0.1
  End Select
  UpdateCallback
End Sub

Private Sub RMCanvas1_Mousedown(Button As Integer, Shift As Integer, x As Single, y As Single)


    m_lastx = x
    m_lasty = y
     
    m_bMouseDown = True
     
End Sub

Private Sub AutoMouseUp()
    m_bMouseDown = False
End Sub

Private Sub RMCanvas1_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
   
    Dim deltax As Single
    Dim deltay As Single
         
    If m_bMouseDown = False Then Exit Sub
        
    deltax = x - m_lastx
    deltay = y - m_lasty
    m_lastx = x
    m_lasty = y
        
    If Button = 2 Then
      BugPos.x = BugPos.x + deltax * 0.03
      Alt = Alt + deltay * -0.02
    Else
      BugPos.x = BugPos.x + deltax * 0.03
      BugPos.z = BugPos.z + deltay * -0.03
    End If
    BugPos.y = yTerrain(BugPos.x, BugPos.z) + Alt
    
    'Anything that changes the scene will require this
    UpdateCallback
    
End Sub

Private Sub RMCanvas1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    m_bMouseDown = False
    If (CurrentMotif) < TotalMotifs Then
        Set seg2 = sty.GetMotif(sty.GetMotifName(CurrentMotif))
        CurrentMotif = CurrentMotif + 1
    Else
        CurrentMotif = 0
        Set seg2 = sty.GetMotif(sty.GetMotifName(CurrentMotif))
    End If
    Call perf.PlaySegment(seg2, DMUS_SEGF_SECONDARY, 0)
End Sub


Sub Stepper()
'Stepping logic

    iLeg = iLeg + 1
    If iLeg > NumLegs - 1 Then iLeg = 0
    
    Dim tmpvec As D3DVECTOR
    Dim ideal As D3DVECTOR
    Dim dx As Single, dz As Single, delta2 As Single
    
    tmpvec.x = RestPos(iLeg).x
    tmpvec.y = RestPos(iLeg).y
    tmpvec.z = RestPos(iLeg).z
    
    'Convert RestPos from BugFrame to WC
    BugFrame.Transform ideal, tmpvec
    
    'Compare current goalpos with converted restposWC
    dx = ideal.x - GoalPos(iLeg).x
    dz = ideal.z - GoalPos(iLeg).z
    delta2 = dx * dx + dz * dz
    
    ' if too far, step.  (set goalpos to restposWC)
    If delta2 > 0.1 Then
        GoalPos(iLeg).x = ideal.x
        GoalPos(iLeg).y = yTerrain(ideal.x, ideal.z)
        GoalPos(iLeg).z = ideal.z
        UpdateCallback
    End If
        
End Sub

Sub form_Load()

    CurrentMotif = 0
    pi = 3.141592653589
    twopi = 2 * pi
    pio2 = pi / 2
    NumLegs = 6
    iLeg = 0
          
    vX.x = 1
    vY.y = 1
    vZ.z = 1
    
    Alt = 0.5
    
    BugPos.y = Alt
    
    FindMediaDir "sphere1.x"
    
    Me.Show
    DoEvents
    
    RMCanvas1.StartWindowed
    Set D3D = RMCanvas1.D3drm
     
    Set BugMesh = D3D.CreateMeshBuilder()
    Set BugFrame = D3D.CreateFrame(Nothing)
    Set PrismMesh = D3D.CreateMeshBuilder()
    Set ToeConeMesh = D3D.CreateMeshBuilder()
    'RMCanvas1.SceneFrame.SetSceneBackgroundRGB 0, 0.5, 1
    RMCanvas1.SceneFrame.SetSceneBackgroundRGB 0, 0, 0
    RMCanvas1.CameraFrame.SetPosition Nothing, 0, 1, -10
    
'Spider
    
    BugMesh.LoadFromFile "sphere1.x", 0, 0, Nothing, Nothing
    BugMesh.ScaleMesh 0.25, 0.25, 0.25
    BugFrame.AddVisual BugMesh
    RMCanvas1.SceneFrame.AddChild BugFrame
    
    PrismMesh.LoadFromFile "prism2.x", 0, 0, Nothing, Nothing
    ToeConeMesh.LoadFromFile "toecone.x", 0, 0, Nothing, Nothing
    
    
        
'   Define base (hip) positions and default (resting) toe positions
'   Base positions could have come from the .X-File
    Dim rr As Single, rb As Single, th As Single, cth As Single
    Dim sth As Single
    
    rr = 2.5
    rb = 0.5
    For i = 0 To NumLegs - 1
      th = i * twopi / NumLegs
      cth = Cos(th)
      sth = Sin(th)
      BasePos(i).x = cth * rb
      BasePos(i).y = 0
      BasePos(i).z = sth * rb
      RestPos(i).x = cth * rr
      RestPos(i).y = 0
      RestPos(i).z = sth * rr
      GoalPos(i).x = 0
      GoalPos(i).y = 0
      GoalPos(i).z = 0
            
    Next
    
' Set up the leg hierarchy (could have been done in the .X-File)
    For i = 0 To NumLegs - 1
      Set Seg0Frame(i) = D3D.CreateFrame(Nothing)
      BugFrame.AddChild Seg0Frame(i)
      Seg0Frame(i).SetPosition BugFrame, BasePos(i).x, BasePos(i).y, BasePos(i).z
      Seg0Frame(i).AddVisual PrismMesh
    
      Set Seg1Frame(i) = D3D.CreateFrame(Nothing)
      Seg0Frame(i).AddChild Seg1Frame(i)
      Seg1Frame(i).SetPosition Seg0Frame(i), 0, 0, 1
      Seg1Frame(i).AddVisual PrismMesh
    
      Set Seg2Frame(i) = D3D.CreateFrame(Nothing)
      Seg1Frame(i).AddChild Seg2Frame(i)
      Seg2Frame(i).SetPosition Seg1Frame(i), 0, 0, 1
      Seg2Frame(i).AddVisual ToeConeMesh
    Next
    
    ' Start out in rest position
    For i = 0 To NumLegs - 1
      GoalPos(i).x = RestPos(i).x
      GoalPos(i).y = yTerrain(RestPos(i).x, RestPos(i).z)
      GoalPos(i).z = RestPos(i).z
    Next
    
    Set scene = RMCanvas1.SceneFrame
    
    
    ' turn off existing lights in the canvas
    RMCanvas1.DirLightFrame.DeleteLight RMCanvas1.DirLight
    
    ' create light
    Set LightFrame1 = D3D.CreateFrame(scene)
    Set Light1 = D3D.CreateLight(D3DRMLIGHT_SPOT, &HFFFFFFFF)
    LightFrame1.AddLight Light1
    LightFrame1.SetPosition scene, 30, 30, -30
    LightFrame1.LookAt BugFrame, scene, D3DRMCONSTRAIN_Z

    Set lInterpolator = D3D.CreateInterpolatorLight

    lInterpolator.SetIndex 0
    lInterpolator.SetColorRGB 1, 0, 0

    lInterpolator.SetIndex 200
    lInterpolator.SetColorRGB 0, 1, 0

    lInterpolator.SetIndex 400
    lInterpolator.SetColorRGB 0, 0, 1

    lInterpolator.SetIndex 600
    lInterpolator.SetColorRGB 1, 1, 1

    lInterpolator.AttachObject Light1

    LightIndex = 0
      
    'Set up the music
    Set perf = dx.DirectMusicPerformanceCreate()
    Set loader = dx.DirectMusicLoaderCreate()
    
    Call perf.Init(Nothing, 0)
    Call perf.SetPort(-1, 4)
    Call perf.SetMasterAutoDownload(True)
    Set seg = loader.LoadSegment("heartland2.sgt")
    Set sty = loader.LoadStyle("HEARTLND.sty")
    
    TotalMotifs = sty.GetMotifCount()
    perf.AddNotificationType (DMUS_NOTIFY_ON_MEASUREANDBEAT)
    perf.AddNotificationType (DMUS_NOTIFY_ON_CHORD)
    perf.AddNotificationType (DMUS_NOTIFY_ON_SEGMENT)
    perf.AddNotificationType (DMUS_NOTIFY_ON_COMMAND)
    notification = dx.CreateEvent(Me)
    
    Call perf.SetNotificationHandle(notification)
    w = -1
    UpdateCallback
    Timer1.Interval = 50
    Timer1.Enabled = True

    
End Sub



Public Sub UpdateCallback()
    'Main update routine.  Does the IK solution.

    Dim spanlen As Single, delta As Single, theta As Single
    Dim dx As Single, dy As Single, dz As Single

    BugFrame.SetPosition RMCanvas1.SceneFrame, BugPos.x, BugPos.y, BugPos.z
    
       
    '   Do the Inverse Kinematics on each leg
    For i = 0 To NumLegs - 1
    
    '     Convert BasePos to WC
      Call BugFrame.Transform(BasePt, BasePos(i))
      Span.x = GoalPos(i).x - BasePt.x
      Span.y = GoalPos(i).y - BasePt.y
      Span.z = GoalPos(i).z - BasePt.z
      
      spanlen = Sqr(Span.x * Span.x + Span.y * Span.y + Span.z * Span.z)
      
            
      
      If spanlen > 2.95 Then spanlen = 2.95
      delta = 0.5 * (spanlen - 1)
      theta = pio2 - Atn(delta / Sqr(1 - delta * delta))
    
'     Orient the IK plane
      Seg0Frame(i).SetOrientation RMCanvas1.SceneFrame, Span.x, Span.y, Span.z, 0, 1, 0
      
'     Set the joint angles

      Seg0Frame(i).AddRotation 1, 1, 0, 0, -theta
      
      Dim pos As D3DVECTOR
      
      Seg1Frame(i).GetPosition Nothing, pos
      Seg1Frame(i).AddRotation 0, 1, 0, 0, theta
      Seg1Frame(i).SetPosition Nothing, pos.x, pos.y, pos.z
      
      Seg2Frame(i).GetPosition Nothing, pos
      Seg2Frame(i).AddRotation 0, 1, 0, 0, theta
      Seg2Frame(i).SetPosition Nothing, pos.x, pos.y, pos.z
      
      
      
    
    Next
    
    'The UpdateCallback always calls Update
    RMCanvas1.Update

End Sub
Sub vPrint(s, v)
    Debug.Print s, Format(v.x, "0.00"), Format(v.y, "0.00"), Format(v.z, "0.00")
End Sub

Public Function yTerrain(x, z)
    yTerrain = 0
End Function



Private Sub Slider1_Scroll()
    perf.SetMasterTempo (Slider1.Value / 4)
End Sub

Private Sub Timer1_Timer()
    Stepper
    RMCanvas1.Update
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

Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
   Dim msg As DMUS_NOTIFICATION_PMSG

    w = w * -1
    Dim UpDown As Integer
    Dim Steps As Integer
Do While perf.GetNotificationPMSG(msg)
    If msg.lNotificationType = 2 Then 'Command
        If msg.lNotificationOption = 1 Then 'Embellisnment
            Delay = 3
        End If
    End If
        
    If msg.lNotificationType = 3 Then 'Measure and beat
        ' First, we'll find out the groove level
        If Delay = 0 Then 'We're not delaying
            mtNow = perf.GetMusicTime()
            If perf.IsPlaying(seg, Nothing) Then
                GrooveLevel = perf.GetGrooveLevel(mtNow, mtNow + 200)
            End If
            ' Next, we'll base the dancing on the groove level
        
            If GrooveLevel > 0 And GrooveLevel <= 20 Then
                If msg.lField1 = 0 Then
                    Call RMCanvas1_Mousedown(1, 1, 0, 0)
                    Call RMCanvas1_MouseMove(1, 1, (20 * (w * -1)), 20 * (w * -1))
                    AutoMouseUp
                    
                End If
            ElseIf GrooveLevel > 20 And GrooveLevel <= 40 Then
                If msg.lField1 = 0 Or msg.lField1 = 2 Then
                    Call RMCanvas1_Mousedown(1, 1, 0, 0)
                    Call RMCanvas1_MouseMove(2, 1, 0, (20 * w))
                    AutoMouseUp
                End If
            ElseIf GrooveLevel > 40 And GrooveLevel <= 60 Then
                Call RMCanvas1_Mousedown(1, 1, 0, 0)
                Call RMCanvas1_MouseMove(2, 1, (-20 * w), (20 * w))
                AutoMouseUp
            ElseIf GrooveLevel > 60 And GrooveLevel <= 80 Then
                    UpDown = Int(mtNow Mod 2 + 1)
                    Steps = Int(mtNow Mod 50 + 10)
                    Call RMCanvas1_Mousedown(1, 1, 0, 0)
                    Call RMCanvas1_MouseMove(UpDown, 1, (Steps * (w * -1)), (Steps * w))
                    AutoMouseUp
            Else '80 - 100
                    UpDown = Int(mtNow Mod 2 + 1)
                    Steps = Int(mtNow Mod 50 + 10)
                    Call RMCanvas1_Mousedown(1, 1, 0, 0)
                    Call RMCanvas1_MouseMove(UpDown, 1, (Steps * (w * -1)), (Steps * w))
                    BugFrame.AddRotation 1, 0, 1, 0, ((Steps / 10) * w)
                    AutoMouseUp
            End If
        Else    'We're in the middle of an embellishment
            If Delay = 3 Then
                'come forward
                Call RMCanvas1_Mousedown(1, 1, 0, 0)
                Call RMCanvas1_MouseMove(1, 1, 0, 100)
                AutoMouseUp
            ElseIf Delay = 1 Then
                'go back
                Call RMCanvas1_Mousedown(1, 1, 0, 0)
                Call RMCanvas1_MouseMove(1, 1, 0, -100)
                AutoMouseUp
            Else
                'just spin
                BugFrame.AddRotation 1, 0, 1, 0, 300
            End If
            Delay = Delay - 1
        End If
    End If
            
    If msg.lNotificationType = 1 Then 'Chord
        LightIndex = LightIndex + 50
        If (LightIndex > 600) Then LightIndex = 1
        lInterpolator.Interpolate LightIndex, Light1, D3DRMINTERPOLATION_LINEAR Or D3DRMINTERPOLATION_CLOSED
    End If
    

Loop
End Sub

