VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Capture and Save to File Sample"
   ClientHeight    =   2925
   ClientLeft      =   150
   ClientTop       =   720
   ClientWidth     =   4110
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2925
   ScaleWidth      =   4110
   StartUpPosition =   3  'Windows Default
   Begin MSComDlg.CommonDialog svFile 
      Left            =   240
      Top             =   840
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      Flags           =   4
   End
   Begin VB.CommandButton cmdStopPlaying 
      Caption         =   "Stop Playing"
      Height          =   375
      Left            =   2520
      TabIndex        =   4
      Top             =   1800
      Width           =   1455
   End
   Begin VB.CommandButton cmdSaveToFile 
      Caption         =   "Save to File"
      Height          =   375
      Left            =   2520
      TabIndex        =   3
      Top             =   2280
      Width           =   1455
   End
   Begin VB.CommandButton cmdPlayRec 
      Caption         =   "Play"
      Height          =   375
      Left            =   2520
      TabIndex        =   2
      Top             =   1320
      Width           =   1455
   End
   Begin VB.CommandButton cmdStopRec 
      Caption         =   "Stop Recording"
      Height          =   375
      Left            =   2520
      TabIndex        =   1
      Top             =   840
      Width           =   1455
   End
   Begin VB.Timer tmrCount 
      Left            =   840
      Top             =   840
   End
   Begin VB.CommandButton cmdStartRec 
      Caption         =   "Start Recording"
      Height          =   375
      Left            =   2520
      TabIndex        =   0
      Top             =   360
      Width           =   1455
   End
   Begin VB.Label lblLTime 
      Alignment       =   1  'Right Justify
      Caption         =   "Time:"
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   360
      Width           =   795
   End
   Begin VB.Label lblTIME 
      Caption         =   "Label1"
      Height          =   255
      Left            =   1140
      TabIndex        =   5
      Top             =   360
      Width           =   795
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''--------------------------------------------------------
''This sample will show how to use the "SaveToFile"
''--------------------------------------------------------


Dim dx As New DirectX7
Dim ds As DirectSound
Dim dsb As DirectSoundBuffer
Dim dsd As DSBUFFERDESC
Dim dsc As DirectSoundCapture
Dim dscb As DirectSoundCaptureBuffer
Dim dscd As DSCBUFFERDESC
Dim CaptureWave As WAVEFORMATEX
Dim capCURS As DSCURSORS
Dim ByteBuffer() As Integer
Dim CNT As Integer
Dim cCaps As DSCCAPS
Dim gfPlay As Boolean

Private Sub cmdPlayRec_Click()
    ''----------------------------------------
    '' convert the data from a capture buffer
    '' to a sound buffer
    ''----------------------------------------
    ConvertToSBuffer
    
    '' did the sound buffer get created?
    If dsb Is Nothing Then
        Exit Sub
    Else
        dsb.Play DSBPLAY_DEFAULT
        tmrCount.Enabled = True
        CNT = 0
        lblTIME.Caption = vbNullString
        If gfPlay Then cmdStopPlaying.Enabled = True
    End If
    
    
End Sub

Private Sub cmdSaveToFile_Click()
On Error Resume Next
    
    Dim FileLocal As String
    
    
    ConvertToSBuffer
    
    If dsb Is Nothing Then Exit Sub
    
    cmdStopPlaying.Enabled = False
    
    tmrCount.Enabled = False
    lblTIME.Caption = vbNullString
    CNT = 0
    
    If dsb Is Nothing Then
        MsgBox "Please record a sound first"
        Exit Sub
    End If
    
    'common dialog control
    svFile.Filter = "*.wav"
    svFile.DialogTitle = "Save Wave File"
    svFile.ShowSave
    
    
    If Right(svFile.filename, 4) <> ".wav" And svFile.filename <> vbNullString Then
        FileLocal = svFile.filename
        FileLocal = FileLocal & ".wav"
    Else
        FileLocal = svFile.filename
    End If
    
    'FileLocal = InputBox("Please enter the location, and file name you want the file saved as.", "SAVE", "c:\windows\temp\test.wav")
    
    If FileLocal = vbNullString Then Exit Sub
    
    If Mid(FileLocal, 2, 1) <> ":" Then Exit Sub
    
    If Right(FileLocal, 3) <> "wav" Then
        MsgBox "Please enter a correct name ie something.wav", vbApplicationModal
        Exit Sub
    End If
    
        
    dsb.SaveToFile FileLocal
    
End Sub

Private Sub cmdStartRec_Click()
    Set dscb = Nothing
    Call InitCapture
    
    dscb.start DSCBSTART_DEFAULT
    
    tmrCount.Interval = 1000
    tmrCount.Enabled = True
    cmdStopRec.Enabled = True
    cmdStartRec.Enabled = False
End Sub

Private Sub cmdStopPlaying_Click()
    
    If dsb Is Nothing Then Exit Sub
    
    Dim l_st As Long
    Dim l_soundStatus As Long
    
    ''--- see if the capture buffer is running
    l_st = dscb.GetStatus()
    If (l_st And DSCBSTATUS_CAPTURING) Then
        dscb.Stop
    End If
    
    ''-- see if the sound buffer is playing
    l_soundStatus = dsb.GetStatus()
    If (l_soundStatus And DSBSTATUS_PLAYING) Then
        dsb.Stop
        dsb.SetCurrentPosition 0
    End If
    
    tmrCount.Enabled = False
    
    CNT = 0
    lblTIME.Caption = vbNullString
    cmdStopPlaying.Enabled = False
End Sub

Private Sub cmdStopRec_Click()
    Dim l_bufferS As Long
    
    If dscb Is Nothing Then Exit Sub
    
    cmdSaveToFile.Enabled = True
    If gfPlay Then cmdPlayRec.Enabled = True
    ''cmdStopPlaying.Enabled = True
    
    '' is the buffer going?
    l_bufferS = dscb.GetStatus()
    If (l_bufferS And DSCBSTATUS_CAPTURING) Then
        dscb.Stop
    End If
    
    tmrCount.Enabled = False
    CNT = 0
    lblTIME.Caption = vbNullString
    cmdStartRec.Enabled = True
    cmdStopRec.Enabled = False
End Sub


Private Sub Form_Load()
    
    On Local Error GoTo errOut

    Set dsc = dx.DirectSoundCaptureCreate(vbNullString)
    On Error Resume Next
    Set ds = dx.DirectSoundCreate(vbNullString)
    If Err.Number = DSERR_ALLOCATED Then 'The card isn't supporting full duplex
        gfPlay = False
        MsgBox "This card does not support full duplex.  You may still record sound.", vbOKOnly Or vbInformation, "No full duplex"
    Else
        gfPlay = True
        ds.SetCooperativeLevel Me.hWnd, DSSCL_NORMAL
    End If
    On Local Error GoTo errOut
    
    InitCapture
    
    cmdSaveToFile.Enabled = False
    cmdPlayRec.Enabled = False
    cmdStopPlaying.Enabled = False
    cmdStopRec.Enabled = False
    
    lblTIME.Caption = vbNullString
    Exit Sub

errOut:
    MsgBox "Unable to initialize sound card for capture.  Exiting this application", vbOKOnly Or vbCritical
    End
End Sub

Private Sub ConvertToSBuffer()
    Dim l_captureS As Long
    
    '' are we running?
    l_captureS = dscb.GetStatus()
    If (l_captureS And DSCBSTATUS_CAPTURING) Then
        dscb.Stop
    End If
    
    '' get the capture info
    dscb.GetCurrentPosition capCURS
    dsd.lBufferBytes = capCURS.lWrite * dscd.fxFormat.nBlockAlign
    dsd.lFlags = DSBCAPS_CTRLVOLUME Or DSBCAPS_STATIC
    
    If capCURS.lWrite = 0 Then
        Exit Sub
    End If

    
    Set dsb = ds.CreateSoundBuffer(dsd, dscd.fxFormat)
    ReDim ByteBuffer(capCURS.lWrite * dscd.fxFormat.nBlockAlign + 1)
    dscb.ReadBuffer 0, capCURS.lWrite * dscd.fxFormat.nBlockAlign, ByteBuffer(0), DSCBLOCK_DEFAULT
    dsb.WriteBuffer 0, capCURS.lWrite * dscd.fxFormat.nBlockAlign, ByteBuffer(0), DSBLOCK_DEFAULT
    ''Set dscb = Nothing
End Sub

Private Function WaveEx(Hz As Long, Channels As Integer, BITS As Integer) As WAVEFORMATEX

    WaveEx.nFormatTag = WAVE_FORMAT_PCM
    WaveEx.nChannels = Channels
    WaveEx.lSamplesPerSec = Hz
    WaveEx.nBitsPerSample = BITS
    WaveEx.nBlockAlign = Channels * BITS / 8
    WaveEx.lAvgBytesPerSec = WaveEx.lSamplesPerSec * WaveEx.nBlockAlign
    WaveEx.nSize = 0

End Function

Private Sub InitCapture()
    'set the capture buffer
    dsc.GetCaps cCaps
    
    If cCaps.lFormats And WAVE_FORMAT_2M08 Then
        CaptureWave = WaveEx(22050, 1, 8)
    ElseIf cCaps.lFormats And WAVE_FORMAT_1M08 Then
        CaptureWave = WaveEx(11025, 1, 8)
    Else
        MsgBox "Capture is not supported with your sound card!", vbApplicationModal
        End
    End If
    
    
    dscd.fxFormat = CaptureWave
    dscd.lBufferBytes = CaptureWave.lAvgBytesPerSec * 20
    dscd.lFlags = DSCBCAPS_WAVEMAPPED
    
    
    Set dscb = dsc.CreateCaptureBuffer(dscd)

    
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Call CleanUp
    End
End Sub

Private Sub CleanUp()
    ''Clean up all the stuff
    Set dx = Nothing
    Set ds = Nothing
    Set dsb = Nothing
    Set dsc = Nothing
    Set dscb = Nothing
    Erase ByteBuffer
End Sub

Private Sub mnuExit_Click()
    Unload Me
End Sub

Private Sub tmrCount_Timer()
On Error Resume Next
    
    CNT = CNT + 1
    
    If CNT = 19 Then
        dscb.Stop
        lblTIME.Caption = "Full"
        frmMain.Refresh
        tmrCount.Enabled = False
                
        cmdSaveToFile.Enabled = True
        If gfPlay Then cmdPlayRec.Enabled = True
        If gfPlay Then cmdStopPlaying.Enabled = True
        
        Exit Sub
    End If
    
    lblTIME.Caption = CNT
    
    ''check the status of the sound buffer
    Dim l_sBs As Long
    If Not (dsb Is Nothing) Then
        l_sBs = dsb.GetStatus()
        If (l_sBs And DSBSTATUS_PLAYING) Then
        Else
            If cmdStartRec.Enabled = True Then
                tmrCount.Enabled = False
                CNT = 1
                lblTIME.Caption = vbNullString
                cmdStopPlaying.Enabled = False
            End If
        End If
    End If
    
        
End Sub
