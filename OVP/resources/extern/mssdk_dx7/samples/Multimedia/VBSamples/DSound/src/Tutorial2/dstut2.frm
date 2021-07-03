VERSION 5.00
Begin VB.Form DS3DPositionForm 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "DS 3D Positioning"
   ClientHeight    =   4365
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   8400
   Icon            =   "dstut2.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4365
   ScaleWidth      =   8400
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture2 
      BackColor       =   &H00FFFFFF&
      FillStyle       =   7  'Diagonal Cross
      Height          =   2775
      Left            =   4800
      ScaleHeight     =   181
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   213
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   1320
      Width           =   3255
   End
   Begin VB.PictureBox Picture1 
      Height          =   1935
      Index           =   1
      Left            =   120
      ScaleHeight     =   1875
      ScaleWidth      =   4395
      TabIndex        =   20
      TabStop         =   0   'False
      Top             =   2160
      Width           =   4455
      Begin VB.HScrollBar scrlAngle 
         Height          =   255
         Index           =   1
         LargeChange     =   20
         Left            =   1080
         Max             =   360
         Min             =   -360
         SmallChange     =   10
         TabIndex        =   16
         Top             =   720
         Value           =   -90
         Width           =   2895
      End
      Begin VB.HScrollBar scrlVol 
         Height          =   255
         Index           =   1
         LargeChange     =   20
         Left            =   1080
         Max             =   0
         Min             =   -3000
         SmallChange     =   500
         TabIndex        =   15
         Top             =   360
         Width           =   2895
      End
      Begin VB.CheckBox chLoop 
         Caption         =   "Loop Play"
         Height          =   315
         Index           =   1
         Left            =   3000
         TabIndex        =   14
         Top             =   1200
         Width           =   1455
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "Stop"
         Height          =   375
         Index           =   1
         Left            =   2040
         TabIndex        =   13
         Top             =   1200
         Width           =   735
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "Pause"
         Height          =   375
         Index           =   1
         Left            =   1080
         TabIndex        =   12
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play"
         Height          =   375
         Index           =   1
         Left            =   120
         TabIndex        =   11
         Top             =   1200
         Width           =   855
      End
      Begin VB.Label Label2 
         Caption         =   "Direction"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   23
         Top             =   720
         Width           =   975
      End
      Begin VB.Label Label1 
         Caption         =   "Volume"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   22
         Top             =   360
         Width           =   1095
      End
      Begin VB.Label Label3 
         Caption         =   "SOUND2"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FF0000&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   21
         Top             =   0
         Width           =   1695
      End
   End
   Begin VB.PictureBox Picture1 
      Height          =   1935
      Index           =   0
      Left            =   120
      ScaleHeight     =   1875
      ScaleWidth      =   4395
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   120
      Width           =   4455
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play"
         Height          =   375
         Index           =   0
         Left            =   120
         TabIndex        =   5
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "Pause"
         Height          =   375
         Index           =   0
         Left            =   1080
         TabIndex        =   6
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "Stop"
         Height          =   375
         Index           =   0
         Left            =   2040
         TabIndex        =   7
         Top             =   1200
         Width           =   735
      End
      Begin VB.CheckBox chLoop 
         Caption         =   "Loop Play"
         Height          =   315
         Index           =   0
         Left            =   3000
         TabIndex        =   8
         Top             =   1200
         Width           =   1455
      End
      Begin VB.HScrollBar scrlVol 
         Height          =   255
         Index           =   0
         LargeChange     =   20
         Left            =   1080
         Max             =   0
         Min             =   -3000
         SmallChange     =   500
         TabIndex        =   9
         Top             =   360
         Width           =   2895
      End
      Begin VB.HScrollBar scrlAngle 
         Height          =   255
         Index           =   0
         LargeChange     =   20
         Left            =   1080
         Max             =   360
         Min             =   -360
         SmallChange     =   10
         TabIndex        =   10
         Top             =   720
         Value           =   -90
         Width           =   2895
      End
      Begin VB.Label Label3 
         Caption         =   "SOUND1"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000000FF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   19
         Top             =   0
         Width           =   1695
      End
      Begin VB.Label Label1 
         Caption         =   "Volume"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   18
         Top             =   360
         Width           =   1095
      End
      Begin VB.Label Label2 
         Caption         =   "Direction"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   17
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Label Label6 
      Caption         =   "Right mouse click in picture box to move Sound 2"
      Height          =   375
      Left            =   4800
      TabIndex        =   2
      Top             =   840
      Width           =   2535
   End
   Begin VB.Label Label5 
      Caption         =   "Left mouse click in picture box to move Sound 1"
      Height          =   495
      Left            =   4800
      TabIndex        =   3
      Top             =   360
      Width           =   2535
   End
   Begin VB.Label Label4 
      Caption         =   "Sound Positions"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   4800
      TabIndex        =   1
      Top             =   120
      Width           =   1575
   End
End
Attribute VB_Name = "DS3DPositionForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Dim m_dx As New DirectX7
Dim m_ds As DirectSound
Dim m_dsBuffer(2) As DirectSoundBuffer
Dim m_ds3dBuffer(2) As DirectSound3DBuffer
Dim m_dsPrimaryBuffer As DirectSoundBuffer
Dim m_dsListener As DirectSound3DListener
Dim m_pos(2) As D3DVECTOR
Dim m_bMouseDown As Boolean
Dim mediapath As String






Private Sub Form_Load()
    
    Dim i As Integer
    
    Me.Show
    
    FindMediaDir "tutb.wav"

    DoEvents
    mediapath = CurDir$

    
    '- Step1 initialize DirectX object
    '  We pass in vbnullstring to indicate that we want the
    '  default sound device.

    On Local Error Resume Next
    Set m_ds = m_dx.DirectSoundCreate(vbNullString)
    
    '- Step2
    '  we can check for error which would indicate
    '  a sound card is not present or directX is not
    '  installed - the On Local Error Resume Next
    '  allows us to check error values immediately
    '  after execution. 0 indicates no error
    If Err.Number <> 0 Then
        MsgBox "Unable to start DirectSound. Check to see that your sound card is properly installed"
        End
    End If

    '- Step3 set the cooperative level by associating
    '  our dsound object with a window.
    '  this tells windows if the sounds created with
    '  the object should be only heard when the window
    '  has focus, COOPERATING  with other sounds from
    '  other applications, or have EXLUSIVE ACCESS to
    '  the sound card which allows us to change the
    '  output wave format and not allow sounds from
    '  other applications to be heard
    '

     
    m_ds.SetCooperativeLevel Me.hWnd, DSSCL_PRIORITY
    
    '- Step4
    '  We create a primary sound buffer object so
    '  we can get at the listener
    Dim primDesc As DSBUFFERDESC, format As WAVEFORMATEX
    
    primDesc.lFlags = DSBCAPS_CTRL3D Or DSBCAPS_PRIMARYBUFFER
    Set m_dsPrimaryBuffer = m_ds.CreateSoundBuffer(primDesc, format)
    Set m_dsListener = m_dsPrimaryBuffer.GetDirectSound3DListener()
    
    m_pos(0).x = 10:  m_pos(0).z = 50
    m_pos(1).x = -10:  m_pos(1).z = 50
    
    '- Make sure we pickup the correct volume and orientation
    scrlAngle_Change (i)
    scrlVol_Change (i)
    
    DrawPositions
    
End Sub


Sub Load(i As Integer, file As String)

    
    '- Step5 create a sound buffer from a file.
    '  we use the DSBUFFERDESC type to indicate
    '  what features we want the sound to have
    '
    '  This time around we add DSBCAPS_CTRL3D
    '  to obtain 3d sound capabilities - be aware that
    '  their is a performance penalty for doing this
    '
    '  the function fills in the other members of bufferDesc which lets
    '  us know how large the buffer is.  It also fills in the wave Format
    '  type giving information about the waves quality and if it supports
    '  stereo
    '  the function returns an initialized SoundBuffer

    Dim bufferDesc1 As DSBUFFERDESC
    Dim waveFormat1 As WAVEFORMATEX
    
        
    bufferDesc1.lFlags = (DSBCAPS_CTRL3D Or DSBCAPS_CTRLFREQUENCY Or DSBCAPS_CTRLPAN Or DSBCAPS_CTRLVOLUME) Or DSBCAPS_STATIC
    Set m_dsBuffer(i) = m_ds.CreateSoundBufferFromFile(file, bufferDesc1, waveFormat1)
    
    '- Step 7 we now get the 3d interfaces to the buffers
    '  we had to create the soundbuffers with the 3d flag
    '  for this method not to fail. The original SoundBuffer
    '  interface is still used to control the starting and
    '  stopping of play but the 3d interface now lets us
    '  position the sounds. Note by default the listener
    '  is at position 0,0
    Set m_ds3dBuffer(i) = m_dsBuffer(i).GetDirectSound3DBuffer
    
    
    
    '- Step 8
    ' Set initial parameters
    
    'setup our directions
    scrlAngle_Change (i)
    
    ' Cone angle indicates how sensitive a sound is to direction
    ' Sounds are omni-directional by default

    ' define a narrow cone of sound
    ' these calls  makes the sound sensitive to direction
    m_ds3dBuffer(i).SetConeAngles DS3D_MINCONEANGLE, 100, DS3D_IMMEDIATE
    m_ds3dBuffer(i).SetConeOutsideVolume -400, DS3D_IMMEDIATE
    

    ' position our sound
    m_ds3dBuffer(i).SetPosition m_pos(i).x / 50, 0, m_pos(i).z / 50, DS3D_IMMEDIATE
    
    
    
End Sub

'- Step 9 Add Play
'
Private Sub cmdPlay_Click(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Call Load(i, mediapath + "\tut" + Chr(Asc("A") + i) + ".wav")
    
    'See if the loop check box is checked
    Dim flag As Long
    flag = 0
    If chLoop(i).Value <> 0 Then flag = 1
           
    'Play plays the sound from the current position
    'if the sound was paused using the stop command
    'then play will begin where it last left off
    m_dsBuffer(i).Play flag
    
End Sub

'- Step 10 Add Stop
'
Private Sub cmdStop_Click(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    
    '- stop does not reset the position of the sound
    m_dsBuffer(i).Stop
    
    '- here we explicity reset the position to the begining
    '  of the sound
    m_dsBuffer(i).SetCurrentPosition 0
    
End Sub


Private Sub chLoop_Click(Index As Integer)
    If m_dsBuffer(Index) Is Nothing Then Exit Sub
    If chLoop(Index).Value = 0 Then
        cmdStop_Click (Index)
    End If
End Sub





'- Step 11 Add Pause
'
Private Sub cmdPause_Click(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    '- stop does not reset the position of the sound
    '  so play will resume in the middle of the  sound
    m_dsBuffer(i).Stop
    
End Sub



'- Step 12 Add Handlers for setting the volume
'
'  changing volume is enabled because we enabled it
'  when we created the buffer other wise this call would fail
'
'  volume is set in dB and ranges from -10000 to 0
'  (direct sound doesn't amplify sounds just attenuates them)
'  because dB is a log scale -6000 is almost the same as
'  off and changes near zero have more effect on the volume
'  than those at -6000. we use a -5000 to 0

Private Sub scrlVol_Change(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).SetVolume scrlVol(i).Value
End Sub

Private Sub scrlVol_Scroll(Index As Integer)
    scrlVol_Change (Index)
End Sub


'- Step 13 Add Handler for changing direction
'
Private Sub scrlAngle_Change(i As Integer)
    
    'fist we must calculate a vector of what direction
    'the sound is traveling in.
    '
    Dim x As Single
    Dim z As Single
    'we take the current angle in degrees convert to radians
    'and get the cos or sin to find the direction from an angle
    x = 5 * Cos(3.141 * scrlAngle(0).Value / 180)
    z = 5 * Sin(3.141 * scrlAngle(0).Value / 180)
    
    'Update the UI
    DrawPositions
    
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    
    'the zero at the end indicates we want the postion updated immediately
    m_ds3dBuffer(i).SetConeOrientation x, 0, z, DS3D_IMMEDIATE
    
End Sub



Private Sub scrlAngle_Scroll(Index As Integer)
    scrlAngle_Change (Index)
End Sub

'- Step 14 add handler for changing position
'  Mouse methods call this function
'  i is 0 for sound1
'  i is 1 for sound2
'  x and y are the coordinates that are being clicked on
'
Sub UpdatePosition(i As Integer, x As Single, z As Single)
    m_pos(i).x = x - Picture2.ScaleWidth / 2
    m_pos(i).z = z - Picture2.ScaleHeight / 2
    
    DrawPositions
    
    'the zero at the end indicates we want the postion updated immediately
    If m_ds3dBuffer(i) Is Nothing Then Exit Sub
    
    m_ds3dBuffer(i).SetPosition m_pos(i).x / 50, 0, m_pos(i).z / 50, DS3D_IMMEDIATE
    
    

End Sub


'- UI Support Code
'
' We use the picture box to give feedback as to the orientation
' of the sound and position

Private Sub Picture2_MouseDown(Button As Integer, Shift As Integer, x As Single, z As Single)
    Dim i As Integer
    If Button = 2 Then i = 1
    UpdatePosition i, x, z
    m_bMouseDown = True
End Sub

Private Sub Picture2_MouseMove(Button As Integer, Shift As Integer, x As Single, z As Single)
    
    Dim i As Integer
    If m_bMouseDown = False Then Exit Sub
    If Button = 2 Then i = 1
    UpdatePosition i, x, z
End Sub


Private Sub Picture2_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
    m_bMouseDown = False
End Sub

Private Sub Picture2_Paint()
    DrawPositions
End Sub




Sub DrawPositions()
    Dim x As Integer
    Dim z As Integer
    
    Picture2.Cls
    
    'listener is in center and is black
    DrawTriangle 0, Picture2.ScaleWidth / 2, Picture2.ScaleHeight / 2, 90
    
    'draw sound 1 as RED
    x = CInt(m_pos(0).x) + Picture2.ScaleWidth / 2
    z = CInt(m_pos(0).z) + Picture2.ScaleHeight / 2
    DrawTriangle RGB(256, 0, 0), x, z, scrlAngle(0).Value
    
    'draw sound2 as BLUE
    x = CInt(m_pos(1).x) + Picture2.ScaleWidth / 2
    z = CInt(m_pos(1).z) + Picture2.ScaleHeight / 2
    DrawTriangle RGB(0, 0, 256), x, z, scrlAngle(1).Value
    
End Sub


Sub DrawTriangle(col As Long, x As Integer, z As Integer, ByVal a As Single)
    
    Dim x1 As Integer
    Dim z1 As Integer
    Dim x2 As Integer
    Dim z2 As Integer
    Dim x3 As Integer
    Dim z3 As Integer
    
    a = 3.141 * (a - 90) / 180
    Dim q As Integer
    q = 10
    
    x1 = q * Sin(a) + x
    z1 = q * Cos(a) + z
    
    x2 = q * Sin(a + 3.141 / 1.3) + x
    z2 = q * Cos(a + 3.141 / 1.3) + z
    
    x3 = q * Sin(a - 3.141 / 1.3) + x
    z3 = q * Cos(a - 3.141 / 1.3) + z
    
    
    
    Picture2.Line (x1, z1)-(x2, z2), col
    Picture2.Line (x1, z1)-(x3, z3), col
    Picture2.Line (x2, z2)-(x3, z3), col
End Sub

Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If Dir$(sFile) = vbNullString Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = vbNullString Then
        ChDir "..\..\media"
    End If
    DoEvents
    Err.Number = 0
End Sub

