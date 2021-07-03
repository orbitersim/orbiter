VERSION 5.00
Begin VB.Form DSPlayForm 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "DS Play Sound"
   ClientHeight    =   4290
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4755
   Icon            =   "dstut1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4290
   ScaleWidth      =   4755
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture1 
      Height          =   1935
      Index           =   1
      Left            =   120
      ScaleHeight     =   1875
      ScaleWidth      =   4395
      TabIndex        =   16
      TabStop         =   0   'False
      Top             =   2160
      Width           =   4455
      Begin VB.HScrollBar scrlPan 
         Height          =   255
         Index           =   1
         LargeChange     =   1000
         Left            =   1080
         Max             =   10000
         Min             =   -10000
         SmallChange     =   500
         TabIndex        =   12
         Top             =   720
         Width           =   2895
      End
      Begin VB.HScrollBar scrlVol 
         Height          =   255
         Index           =   1
         LargeChange     =   20
         Left            =   1080
         Max             =   0
         Min             =   -5000
         SmallChange     =   500
         TabIndex        =   11
         Top             =   360
         Width           =   2895
      End
      Begin VB.CheckBox chLoop 
         Caption         =   "Loop Play"
         Height          =   315
         Index           =   1
         Left            =   3000
         TabIndex        =   10
         Top             =   1200
         Width           =   1455
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "Stop"
         Height          =   375
         Index           =   1
         Left            =   2040
         TabIndex        =   9
         Top             =   1200
         Width           =   735
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "Pause"
         Height          =   375
         Index           =   1
         Left            =   1080
         TabIndex        =   8
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play"
         Height          =   375
         Index           =   1
         Left            =   120
         TabIndex        =   7
         Top             =   1200
         Width           =   855
      End
      Begin VB.Label Label2 
         Caption         =   "Pan"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   19
         Top             =   720
         Width           =   975
      End
      Begin VB.Label Label1 
         Caption         =   "Volume"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   18
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
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   17
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
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   120
      Width           =   4455
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play"
         Height          =   375
         Index           =   0
         Left            =   120
         TabIndex        =   1
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdPause 
         Caption         =   "Pause"
         Height          =   375
         Index           =   0
         Left            =   1080
         TabIndex        =   2
         Top             =   1200
         Width           =   855
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "Stop"
         Height          =   375
         Index           =   0
         Left            =   2040
         TabIndex        =   3
         Top             =   1200
         Width           =   735
      End
      Begin VB.CheckBox chLoop 
         Caption         =   "Loop Play"
         Height          =   315
         Index           =   0
         Left            =   3000
         TabIndex        =   4
         Top             =   1200
         Width           =   1455
      End
      Begin VB.HScrollBar scrlVol 
         Height          =   255
         Index           =   0
         LargeChange     =   20
         Left            =   1080
         Max             =   0
         Min             =   -5000
         SmallChange     =   500
         TabIndex        =   5
         Top             =   360
         Width           =   2895
      End
      Begin VB.HScrollBar scrlPan 
         Height          =   255
         Index           =   0
         LargeChange     =   1000
         Left            =   1080
         Max             =   10000
         Min             =   -10000
         SmallChange     =   500
         TabIndex        =   6
         Top             =   720
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
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   15
         Top             =   0
         Width           =   1695
      End
      Begin VB.Label Label1 
         Caption         =   "Volume"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   14
         Top             =   360
         Width           =   1095
      End
      Begin VB.Label Label2 
         Caption         =   "Pan"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   13
         Top             =   720
         Width           =   975
      End
   End
End
Attribute VB_Name = "DSPlayForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'===========================================================
'Direct Sound Tutorial 1
'This tutorial will show you how to load a wave file in to a
'buffer, and then play back the buffered file.
'1/27/1999
'===========================================================
option explicit

Dim m_dx As New DirectX7
Dim m_ds As DirectSound
Dim m_dsBuffer(2) As DirectSoundBuffer
Dim m_bLoaded(2) As Boolean
Dim MediaPath As String



Private Sub Form_Load()
    Me.Show
    
    FindMediaDir "tuta.wav"
    MediaPath = CurDir$
    
    
    '===========================================
    '- Step1 initialize the DirectX object.
    '  We pass in "" to indicate we want the
    '  default sound device.
    '===========================================
    
    On Local Error Resume Next
    
    Set m_ds = m_dx.DirectSoundCreate("")
    
    '========================================================
    '- Step2
    '  We can check for errors which would indicate
    '  a sound card is not present or DirectX is not
    '  installed. The 'On Local Error Resume Next'
    '  statement allows us to check error values immediately
    '  after execution. The error number 0 indicates no error.
    '========================================================

    
    If Err.Number <> 0 Then
        MsgBox "Unable to start DirectSound. Check to see that your sound card is properly installed"
        End
    End If

    '=======================================================
    '- Step3 set the cooperative level by associating
    '  our dsound object with a window.
    '  This tells windows if the sounds created with
    '  the object should be only heard when the window
    '  has focus, COOPERATING  with other sounds from
    '  other applications, or have EXLUSIVE ACCESS to
    '  the sound card which allows us to change the
    '  output wave format and not allow sounds from
    '  other applications to be heard.
    '
    '  For this application we will use DSSCL_COOPERATIVE
    '  NOTE: This has to be set before you create the
    '  sound buffer.
    '========================================================
    
    m_ds.SetCooperativeLevel Me.hWnd, DSSCL_PRIORITY
    
    
    
End Sub


Sub LoadWave(i As Integer, sfile As String)


    '========================================================================
    '- Step4 CREATE SOUND BUFFER FROM FILE.
    '  we use the DSBUFFERDESC type to indicate
    '  what features we want the sound to have.
    '  The lFlags member can be used to enable 3d support,
    '  frequency changes, and volume changes.
    '  The DSBCAPS flags indicates we will allow
    '  volume changes, frequency changes, and pan changes
    '  the DDSBCAPS_STATIC -(which is optional in this release
    '  since all  buffers loaded by this method are static) indicates
    '  that we want the entire file loaded into memory.
    '
    '  The function fills in the other members of bufferDesc which lets
    '  us know how large the buffer is.  It also fills in the wave Format
    '  type giving information about the waves quality and if it supports
    '  stereo the function returns an initialized SoundBuffer
    '=========================================================================
    
    Dim bufferDesc As DSBUFFERDESC
    Dim waveFormat As WAVEFORMATEX
    
    bufferDesc.lFlags = DSBCAPS_CTRLFREQUENCY or DSBCAPS_CTRLPAN or DSBCAPS_CTRLVOLUME Or DSBCAPS_STATIC
    
    waveFormat.nFormatTag = WAVE_FORMAT_PCM
    waveFormat.nChannels = 2
    waveFormat.lSamplesPerSec = 22050
    waveFormat.nBitsPerSample = 16
    waveFormat.nBlockAlign = waveFormat.nBitsPerSample / 8 * waveFormat.nChannels
    waveFormat.lAvgBytesPerSec = waveFormat.lSamplesPerSec * waveFormat.nBlockAlign
    Set m_dsBuffer(i) = m_ds.CreateSoundBufferFromFile(sfile, bufferDesc, waveFormat)
    
    '========================================
    '- Step 5 make sure we have no errors
    '========================================
    
    If Err.Number <> 0 Then
        MsgBox "unable to find " + sfile
        End
    End If

    scrlPan_Change i
    scrlVol_Change i
    
End Sub

'===============================
' Step 8 - PLAYING THE SOUNDS
'===============================

Private Sub cmdPlay_Click(i As Integer)

    '=========================================================
    ' Make sure we loaded our sound
    '=========================================================
    If m_bLoaded(i) = False Then
        m_bLoaded(i) = True
        LoadWave CLng(i), MediaPath + "\tut" + Chr(97 + i) + ".wav"
    End If
            
    '=========================================================
    ' For a sound buffer you can play it two ways.
    ' One is: m_dsBuffer(0).Play DSBPLAY_DEFAULT
    ' this will play the buffer one time with no
    ' looping.  Second is: m_dsBuffer(0).Play DSBPLAY_LOOPING
    ' and this one will continue to loop the buffer.
    '==========================================================
  
        
    '======================================
    'See if the loop check box is checked
    '======================================
    Dim flag As Long
    flag = 0
    If chLoop(i).Value <> 0 Then flag = 1
    
    '================================================
    'Play, plays the sound from the current position
    'if the sound was paused using the stop command
    'then play will begin where it last left off
    '================================================

    m_dsBuffer(i).Play flag
    
    '================================================
    'This format (i) is used because the buffer was
    'set to a array.  The syntax for normal buffer
    'would be the same except for the deletion of the
    'array setting.  I.E. if the buffer was set to
    'DSB then the format for playing would be
    'DSB.Play flag
    'values are:
    '   DSBPLAY_DEFAULT
    '   DSBPLAY_LOOPING
    '================================================


End Sub

'==================
'- Step 9 Add Stop
'==================

Private Sub cmdStop_Click(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub

    '================================================
    '- stop does not reset the position of the sound
    '================================================
    m_dsBuffer(i).Stop
    
    '=======================================================
    '- Here we explicitly reset the position to the beginning
    '  of the sound.
    '=======================================================
    m_dsBuffer(i).SetCurrentPosition 0
    
End Sub

Private Sub chLoop_Click(Index As Integer)
    If chLoop(Index).Value = 0 Then
        cmdStop_Click Index
    End If
End Sub

'============================
'- Step 10 Add Pause
'============================

Private Sub cmdPause_Click(i As Integer)
    '===================================================
    '- stop does not reset the position of the sound
    '  so play will resume in the middle of the  sound
    '===================================================
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).Stop
    
End Sub




'======================================================================
'- Step 11 Add Handler for setting the volume
'
'  Changing volume is enabled because we enabled it
'  when we created the buffer otherwise this call would fail.
'
'  volume is set in db and ranges from -10000 to 0
'  (direct sound doesn't amplify sounds just decreases their volume)
'  because db is a log scale -6000 is almost the same as
'  off and changes near zero have more effect on the volume
'  than those at -6000. we use a -5000 to 0
'======================================================================

Private Sub scrlVol_Change(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).SetVolume scrlVol(i).Value
End Sub
Private Sub scrlVol_Scroll(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).SetVolume scrlVol(i).Value
End Sub



'===============================================================
'- Step 9 Add Handler for Pan
'
'  changing pan is enabled because we enabled it
'  when we created the buffer otherwise this call would fail
'  volume is set in db and ranges from -10000 to 10000 where
'  -10000 is the left speaker and 10000 is the right speaker
'  like changing volume, values at the extremes aren't as audible
'  as those near 0
'===============================================================

Private Sub scrlPan_Change(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).SetPan scrlPan(i).Value
End Sub
Private Sub scrlPan_Scroll(i As Integer)
    If m_dsBuffer(i) Is Nothing Then Exit Sub
    m_dsBuffer(i).SetPan scrlPan(i).Value
End Sub


'============================
'This looks for the media
'path.
'============================

Sub FindMediaDir(sfile As String)
 On Local Error Resume Next
 
 If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
 End If
 ChDir App.Path
 If Dir$(sfile) = "" Then
     ChDir "..\media"
 End If
 If Dir$(sfile) = "" Then
     ChDir "..\..\media"
 End If
 Err.Number = 0
End Sub
