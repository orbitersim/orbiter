VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Begin VB.Form SoundMix 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "SoundMix"
   ClientHeight    =   3975
   ClientLeft      =   2475
   ClientTop       =   2385
   ClientWidth     =   7815
   Icon            =   "soundmix.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   3975
   ScaleWidth      =   7815
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   960
      Top             =   3960
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      Flags           =   4
   End
   Begin VB.PictureBox Picture1 
      Height          =   3735
      Index           =   3
      Left            =   5880
      ScaleHeight     =   3675
      ScaleWidth      =   1755
      TabIndex        =   27
      Top             =   120
      Width           =   1815
      Begin VB.CheckBox chkLoop 
         Caption         =   "Loop Play"
         ForeColor       =   &H8000000E&
         Height          =   195
         Index           =   3
         Left            =   360
         TabIndex        =   39
         Top             =   3360
         Width           =   1095
      End
      Begin VB.CommandButton cmdLoad 
         Caption         =   "Load"
         Height          =   375
         Index           =   3
         Left            =   120
         TabIndex        =   32
         Top             =   120
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H0000FF00&
         Enabled         =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Index           =   3
         Left            =   120
         TabIndex        =   31
         Text            =   "Empty"
         Top             =   480
         Width           =   1455
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   ">"
         Height          =   375
         Index           =   3
         Left            =   120
         TabIndex        =   30
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "| |"
         Height          =   375
         Index           =   3
         Left            =   600
         TabIndex        =   29
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "[ ]"
         Height          =   375
         Index           =   3
         Left            =   1080
         TabIndex        =   28
         Top             =   960
         Width           =   495
      End
      Begin MSComctlLib.Slider sldVol 
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   45
         Top             =   2880
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         Min             =   -4000
         Max             =   0
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldPan 
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   48
         Top             =   2280
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   -5000
         Max             =   5000
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldFreq 
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   51
         Top             =   1680
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   100
         Max             =   100000
         SelStart        =   2000
         TickFrequency   =   20000
         Value           =   2000
      End
      Begin VB.Label Label1 
         BackStyle       =   0  'Transparent
         Caption         =   "Frequency"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   35
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label Label2 
         BackStyle       =   0  'Transparent
         Caption         =   "Pan"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   34
         Top             =   2040
         Width           =   855
      End
      Begin VB.Label Label3 
         BackStyle       =   0  'Transparent
         Caption         =   "Volume"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   33
         Top             =   2640
         Width           =   855
      End
   End
   Begin VB.PictureBox Picture1 
      Height          =   3735
      Index           =   2
      Left            =   3960
      ScaleHeight     =   3675
      ScaleWidth      =   1755
      TabIndex        =   18
      Top             =   120
      Width           =   1815
      Begin VB.CheckBox chkLoop 
         Caption         =   "Loop Play"
         ForeColor       =   &H8000000E&
         Height          =   195
         Index           =   2
         Left            =   360
         TabIndex        =   38
         Top             =   3360
         Width           =   1095
      End
      Begin VB.CommandButton cmdLoad 
         Caption         =   "Load"
         Height          =   375
         Index           =   2
         Left            =   120
         TabIndex        =   23
         Top             =   120
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H0000FF00&
         Enabled         =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Index           =   2
         Left            =   120
         TabIndex        =   22
         Text            =   "Empty"
         Top             =   480
         Width           =   1455
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   ">"
         Height          =   375
         Index           =   2
         Left            =   120
         TabIndex        =   21
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "| |"
         Height          =   375
         Index           =   2
         Left            =   600
         TabIndex        =   20
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "[ ]"
         Height          =   375
         Index           =   2
         Left            =   1080
         TabIndex        =   19
         Top             =   960
         Width           =   495
      End
      Begin MSComctlLib.Slider sldVol 
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   44
         Top             =   2880
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         Min             =   -4000
         Max             =   0
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldPan 
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   47
         Top             =   2280
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   -5000
         Max             =   5000
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldFreq 
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   50
         Top             =   1680
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   100
         Max             =   100000
         SelStart        =   2000
         TickFrequency   =   20000
         Value           =   2000
      End
      Begin VB.Label Label1 
         BackStyle       =   0  'Transparent
         Caption         =   "Frequency"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   26
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label Label2 
         BackStyle       =   0  'Transparent
         Caption         =   "Pan"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   25
         Top             =   2040
         Width           =   855
      End
      Begin VB.Label Label3 
         BackStyle       =   0  'Transparent
         Caption         =   "Volume"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   24
         Top             =   2640
         Width           =   855
      End
   End
   Begin VB.PictureBox Picture1 
      Height          =   3735
      Index           =   1
      Left            =   2040
      ScaleHeight     =   3675
      ScaleWidth      =   1755
      TabIndex        =   9
      Top             =   120
      Width           =   1815
      Begin VB.CheckBox chkLoop 
         Caption         =   "Loop Play"
         ForeColor       =   &H8000000E&
         Height          =   195
         Index           =   1
         Left            =   360
         TabIndex        =   37
         Top             =   3360
         Width           =   1095
      End
      Begin VB.CommandButton cmdLoad 
         Caption         =   "Load"
         Height          =   375
         Index           =   1
         Left            =   120
         TabIndex        =   14
         Top             =   120
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H0000FF00&
         Enabled         =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Index           =   1
         Left            =   120
         TabIndex        =   13
         Text            =   "Empty"
         Top             =   480
         Width           =   1455
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   ">"
         Height          =   375
         Index           =   1
         Left            =   120
         TabIndex        =   12
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "| |"
         Height          =   375
         Index           =   1
         Left            =   600
         TabIndex        =   11
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "[ ]"
         Height          =   375
         Index           =   1
         Left            =   1080
         TabIndex        =   10
         Top             =   960
         Width           =   495
      End
      Begin MSComctlLib.Slider sldVol 
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   43
         Top             =   2880
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         Min             =   -4000
         Max             =   0
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldPan 
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   46
         Top             =   2280
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   -5000
         Max             =   5000
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldFreq 
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   49
         Top             =   1680
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   100
         Max             =   100000
         SelStart        =   2000
         TickFrequency   =   20000
         Value           =   2000
      End
      Begin VB.Label Label1 
         BackStyle       =   0  'Transparent
         Caption         =   "Frequency"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   17
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label Label2 
         BackStyle       =   0  'Transparent
         Caption         =   "Pan"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   16
         Top             =   2040
         Width           =   855
      End
      Begin VB.Label Label3 
         BackStyle       =   0  'Transparent
         Caption         =   "Volume"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   15
         Top             =   2640
         Width           =   855
      End
   End
   Begin VB.PictureBox Picture1 
      Height          =   3735
      Index           =   0
      Left            =   120
      ScaleHeight     =   3675
      ScaleWidth      =   1755
      TabIndex        =   0
      Top             =   120
      Width           =   1815
      Begin MSComctlLib.Slider sldVol 
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   42
         Top             =   2880
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         Min             =   -4000
         Max             =   0
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldPan 
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   41
         Top             =   2280
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   -5000
         Max             =   5000
         TickFrequency   =   1000
      End
      Begin MSComctlLib.Slider sldFreq 
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   40
         Top             =   1680
         Width           =   1575
         _ExtentX        =   2778
         _ExtentY        =   450
         _Version        =   393216
         LargeChange     =   1000
         Min             =   100
         Max             =   100000
         SelStart        =   2000
         TickFrequency   =   20000
         Value           =   2000
      End
      Begin VB.CheckBox chkLoop 
         Caption         =   "Loop Play"
         ForeColor       =   &H8000000E&
         Height          =   195
         Index           =   0
         Left            =   360
         TabIndex        =   36
         Top             =   3360
         Width           =   1215
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "[ ]"
         Height          =   375
         Index           =   0
         Left            =   1080
         TabIndex        =   5
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "| |"
         Height          =   375
         Index           =   0
         Left            =   600
         TabIndex        =   4
         Top             =   960
         Width           =   495
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   ">"
         Height          =   375
         Index           =   0
         Left            =   120
         TabIndex        =   3
         Top             =   960
         Width           =   495
      End
      Begin VB.TextBox Text1 
         BackColor       =   &H0000FF00&
         Enabled         =   0   'False
         ForeColor       =   &H00FFFFFF&
         Height          =   285
         Index           =   0
         Left            =   120
         TabIndex        =   2
         Text            =   "Empty"
         Top             =   480
         Width           =   1455
      End
      Begin VB.CommandButton cmdLoad 
         Caption         =   "Load"
         Height          =   375
         Index           =   0
         Left            =   120
         TabIndex        =   1
         Top             =   120
         Width           =   1455
      End
      Begin VB.Label Label3 
         BackStyle       =   0  'Transparent
         Caption         =   "Volume"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   8
         Top             =   2640
         Width           =   855
      End
      Begin VB.Label Label2 
         BackStyle       =   0  'Transparent
         Caption         =   "Pan"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   7
         Top             =   2040
         Width           =   855
      End
      Begin VB.Label Label1 
         BackStyle       =   0  'Transparent
         Caption         =   "Frequency"
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   6
         Top             =   1440
         Width           =   855
      End
   End
End
Attribute VB_Name = "SoundMix"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'=====================================
'This tutorial will show how
'to mix wave files
'=====================================

Option Explicit
Dim m_ds As DirectSound
Dim m_dx As DirectX7
Dim m_dssb(4) As DirectSoundBuffer
Dim m_state(4) As String
Dim m_sFile(4) As String
Dim m_notify(4) As Long
Dim waveFormat(4) As WAVEFORMATEX
Dim bufferDesc(4) As DSBUFFERDESC
    
Implements DirectXEvent
Private Declare Function GetWindowsDirectory Lib "kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
    
Sub init()
    '============================
    'PART #1
   '============================
    On Local Error GoTo exitout
    
    'initalize the directx, and direct sound variables
    
    Set m_dx = New DirectX7
    Set m_ds = m_dx.DirectSoundCreate("")
    
    'set the cooperative level (DSSCL_PRIORITY) is the best
    'for sound quality.
    
    m_ds.SetCooperativeLevel Me.hWnd, DSSCL_PRIORITY
    
    Exit Sub
    
exitout:
    MsgBox ("Unable to initialize DirectSound - sound card may not be present")
    End
End Sub

            

Function LoadWave(i As Integer, sFile As String) As Boolean
    '================================
    'LOADING THE WAVE FILE
    '================================
    
    'set the buffer description
    bufferDesc(i).lFlags = DSBCAPS_CTRLVOLUME Or DSBCAPS_CTRLFREQUENCY Or DSBCAPS_CTRLPAN Or DSBCAPS_STATIC Or DSBCAPS_CTRLPOSITIONNOTIFY
    
    
    If (m_state(i) = "playing") Then
        m_dssb(i).Stop
    End If
    'clear buffer
    Set m_dssb(i) = Nothing
    
    waveFormat(i).nSize = LenB(waveFormat(i))
    waveFormat(i).nFormatTag = WAVE_FORMAT_PCM
    waveFormat(i).nChannels = 2
    waveFormat(i).lSamplesPerSec = 22050
    waveFormat(i).nBitsPerSample = 16
    waveFormat(i).nBlockAlign = waveFormat(i).nBitsPerSample / 8 * waveFormat(i).nChannels
    waveFormat(i).lAvgBytesPerSec = waveFormat(i).lSamplesPerSec * waveFormat(i).nBlockAlign
    'load a file into the buffer
    Set m_dssb(i) = m_ds.CreateSoundBufferFromFile(sFile, bufferDesc(i), waveFormat(i))
       
    
    Dim l As Long
    'get the frequency
    l = m_dssb(i).GetFrequency()
    sldFreq(i).Value = l
    
    If m_notify(i) <> 0 Then
      m_dx.DestroyEvent m_notify(i)
    End If
    
    m_notify(i) = m_dx.CreateEvent(Me)
    
    Dim psa(1) As DSBPOSITIONNOTIFY
    psa(0).hEventNotify = m_notify(i)
    psa(0).lOffset = bufferDesc(i).lBufferBytes - 1
    m_dssb(i).SetNotificationPositions 1, psa()
    
End Function


    


Private Sub chkLoop_Click(Index As Integer)
    If m_state(Index) = "playing" And chkLoop(Index).Value = 0 Then
        cmdStop_Click (Index)
    End If
End Sub

Private Sub cmdLoad_Click(Index As Integer)
    
    'common dialog for loading file
    
    Dim sFile As String
    Dim b As Boolean
    Static sCurdir As String
    
   ' On Local Error GoTo exitout
   
    CommonDialog1.filename = "*.Wav"
    CommonDialog1.DialogTitle = "Open Wave File "
    CommonDialog1.DefaultExt = ".wav"
    CommonDialog1.Filter = "*.wav"
    If sCurdir = vbNullString Then
        'Set the init folder to \windows\media if it exists.  If not, set it to the \windows folder
        Dim sWindir As String
        sWindir = Space$(255)
        If GetWindowsDirectory(sWindir, 255) <> 0 Then
            Dim sMedia As String
            sWindir = Left$(sWindir, InStr(sWindir, Chr$(0)) - 1)
            If Right$(sWindir, 1) = "\" Then
                sMedia = sWindir & "Media"
            Else
                sMedia = sWindir & "\Media"
            End If
            If Dir$(sMedia, vbDirectory) <> vbNullString Then
                CommonDialog1.InitDir = sMedia
            Else
                CommonDialog1.InitDir = sWindir
            End If
        End If
    Else
        CommonDialog1.InitDir = sCurdir
    End If
    CommonDialog1.ShowOpen
    
    sFile = CommonDialog1.filename
    If sFile = "" Or sFile = "*.Wav" Then Exit Sub
    sCurdir = GetFolder(CommonDialog1.filename)
    chkLoop(Index).Enabled = True
    Call LoadWave(Index, sFile)
    m_sFile(Index) = CommonDialog1.FileTitle
    Text1(Index).Text = m_sFile(Index)
    m_state(Index) = "loaded"
    Exit Sub
exitout:
    Call MsgBox("problem loading " + sFile)
    m_state(Index) = ""
    Text1(Index).Text = "empty"
End Sub

Private Function GetFolder(ByVal sFile As String) As String
    Dim lCount As Long
    
    For lCount = Len(sFile) To 1 Step -1
        If Mid$(sFile, lCount, 1) = "\" Then
            GetFolder = Left$(sFile, lCount)
            Exit Function
        End If
    Next
    GetFolder = vbNullString
End Function

Private Sub cmdPlay_Click(Index As Integer)
    '================================
    'PLAYING THE WAVE BUFFER
    '================================
    
    Dim loopit As Long
    
    If m_state(Index) = "" Then Exit Sub
    If m_state(Index) <> "pause" Then
        m_dssb(Index).SetCurrentPosition 0
    End If
    
    If chkLoop(Index).Value = 0 Then
        m_dssb(Index).Play DSBPLAY_DEFAULT ' Play one time
    Else
        m_dssb(Index).Play DSBPLAY_LOOPING ' Looping
    End If
    
    m_state(Index) = "playing"
    Text1(Index).Text = "playing"
    chkLoop(Index).Enabled = False
    
    
End Sub

Private Sub cmdStop_Click(Index As Integer)
        
    If m_state(Index) = "" Then Exit Sub
    '=====================================
    'STOPPING THE BUFFER
    '=====================================
    
    'When you stop the buffer the position
    'is not reset to the start (position 0)
    'so you have to do this if you want
    'a complete stop, and not a pause

    
    Call m_dssb(Index).Stop
    Call m_dssb(Index).SetCurrentPosition(0)

    m_state(Index) = "stop"
    Text1(Index).Text = m_sFile(Index)
    chkLoop(Index).Enabled = True
    
End Sub

Private Sub cmdPause_Click(Index As Integer)
    'same as stop, but do not set the position to
    '0 because when you play the buffer again
    'it will start from the position of which
    'it stopped

    
    If m_state(Index) = "" Then Exit Sub
    Call m_dssb(Index).Stop
    m_state(Index) = "pause"
    Text1(Index).Text = "paused"
    chkLoop(Index).Enabled = True
End Sub




Private Sub Form_Load()
    init
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Dim tmpCnt As Integer
    
    For tmpCnt = 0 To UBound(m_notify)
        If m_notify(tmpCnt) <> 0 Then
            m_dx.DestroyEvent m_notify(tmpCnt)
        End If
    Next
    
    DoEvents
    
    End
    
End Sub

Private Sub sldFreq_Scroll(Index As Integer)
        '=======================================
        'CHANGE THE FREQUENCY
        '=======================================
        'earlier we set the slider bar to
        'the frequency that was recorded in
        'the wave file.  Here we can manipluate
        'the wave file by setting the frequency
        'of the current buffer.
        
        If m_state(Index) = "" Then Exit Sub
        m_dssb(Index).SetFrequency sldFreq(Index).Value
End Sub

Private Sub sldVol_Scroll(Index As Integer)
        '======================================
        'CHANGE THE VOLUME
        '======================================
        
        If m_state(Index) = "" Then Exit Sub
        
        Dim v As Long
        Dim l As Long
        Dim fVal As Single
        
        '======================================
        'NOTE: the volume is not amplified.
        'the highest value is zero
        'Direct Sound does not amplify the
        'sound
        
        v = sldVol(Index).Value
        
        'db is on log scale
        '3       2     1     0
        '100db   10db  1db   0
        '10000   1000  100   0


        If v < -4000 Then v = -10000
        m_dssb(Index).SetVolume v
        
End Sub

Private Sub sldPan_Scroll(Index As Integer)
    '=======================================
    'SET PAN
    '=======================================
    'Here we grab the pan value from a slider
    'bar.  The same concept as volume, and
    'frequency.
    
    If m_state(Index) = "" Then Exit Sub
    m_dssb(Index).SetPan sldPan(Index).Value
End Sub


Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
    Dim i As Long
    
    'figure out which sound finished playing.
     For i = 0 To 3
        If ((eventid = m_notify(i)) And (chkLoop(i).Value = 0)) Then
            Text1(i) = "stopped"
            chkLoop(i).Enabled = True
        End If
     Next
     
End Sub

