VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "PlaySound"
   ClientHeight    =   4035
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5550
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4035
   ScaleWidth      =   5550
   StartUpPosition =   3  'Windows Default
   Begin MSComctlLib.Slider sldVol 
      Height          =   255
      Left            =   2640
      TabIndex        =   16
      Top             =   2640
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   450
      _Version        =   393216
   End
   Begin MSComctlLib.Slider sldPan 
      Height          =   255
      Left            =   2640
      TabIndex        =   15
      Top             =   2280
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   450
      _Version        =   393216
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      Height          =   375
      Left            =   4080
      TabIndex        =   14
      Top             =   3480
      Width           =   1095
   End
   Begin VB.CommandButton cmdStop 
      Caption         =   "&Stop"
      Height          =   375
      Left            =   1320
      TabIndex        =   13
      Top             =   3480
      Width           =   1095
   End
   Begin VB.CommandButton cmdPlay 
      Caption         =   "&Play"
      Height          =   375
      Left            =   120
      TabIndex        =   12
      Top             =   3480
      Width           =   1095
   End
   Begin VB.CheckBox chkLoop 
      Caption         =   "Loop Sound"
      Height          =   255
      Left            =   120
      TabIndex        =   11
      Top             =   3120
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog comDLG 
      Left            =   5040
      Top             =   3360
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton cmdLoadFile 
      Caption         =   "SoundFile..."
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   600
      Width           =   1095
   End
   Begin VB.ComboBox cboDrivers 
      Height          =   315
      ItemData        =   "frmMain.frx":0442
      Left            =   1320
      List            =   "frmMain.frx":0444
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   240
      Width           =   3855
   End
   Begin MSComctlLib.Slider sldFreq 
      Height          =   255
      Left            =   2640
      TabIndex        =   18
      Top             =   1920
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   450
      _Version        =   393216
   End
   Begin VB.Label lblHigh 
      Caption         =   "High"
      Height          =   255
      Left            =   4920
      TabIndex        =   24
      Top             =   2640
      Width           =   495
   End
   Begin VB.Label lblRight 
      Caption         =   "Right"
      Height          =   255
      Left            =   4920
      TabIndex        =   23
      Top             =   2280
      Width           =   495
   End
   Begin VB.Label lblMaxHz 
      Caption         =   "100 Kz"
      Height          =   255
      Left            =   4920
      TabIndex        =   22
      Top             =   1920
      Width           =   615
   End
   Begin VB.Label lblLow 
      Caption         =   "Low"
      Height          =   255
      Left            =   2280
      TabIndex        =   21
      Top             =   2640
      Width           =   375
   End
   Begin VB.Label lblLeft 
      Caption         =   "Left"
      Height          =   255
      Left            =   2280
      TabIndex        =   20
      Top             =   2280
      Width           =   375
   End
   Begin VB.Label lblNOKLZ 
      Caption         =   "100 Hz"
      Height          =   255
      Left            =   2040
      TabIndex        =   19
      Top             =   1920
      Width           =   615
   End
   Begin VB.Label lblFreqDesc 
      Caption         =   "Label1"
      Height          =   255
      Left            =   1320
      TabIndex        =   17
      Top             =   1920
      Width           =   735
   End
   Begin VB.Label lblVol 
      Caption         =   "Volume"
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   2640
      Width           =   975
   End
   Begin VB.Label lblPan 
      Caption         =   "Pan"
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   2280
      Width           =   975
   End
   Begin VB.Label lblFreq 
      Caption         =   "Frequency"
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   1920
      Width           =   975
   End
   Begin VB.Label lblVolDesc 
      Caption         =   "Label3"
      Height          =   255
      Left            =   1320
      TabIndex        =   7
      Top             =   2640
      Width           =   855
   End
   Begin VB.Label lblPanDesc 
      Caption         =   "Label2"
      Height          =   255
      Left            =   1320
      TabIndex        =   6
      Top             =   2280
      Width           =   855
   End
   Begin VB.Label lblStatusDesc 
      Caption         =   "Label1"
      Height          =   375
      Left            =   1320
      TabIndex        =   5
      Top             =   1200
      Width           =   3855
   End
   Begin VB.Label lblStatus 
      AutoSize        =   -1  'True
      Caption         =   "Status"
      Height          =   195
      Left            =   120
      TabIndex        =   4
      Top             =   1200
      Width           =   450
   End
   Begin VB.Label lblFileLabel 
      Caption         =   "Label1"
      Height          =   375
      Left            =   1320
      TabIndex        =   3
      Top             =   720
      Width           =   3855
   End
   Begin VB.Label lblAudioDriver 
      AutoSize        =   -1  'True
      Caption         =   "Audio Driver"
      Height          =   195
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   870
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim gDX As New DirectX7
Dim gDS As DirectSound
Dim gDSB As DirectSoundBuffer
Dim gD As DSBUFFERDESC
Dim gW As WAVEFORMATEX
Dim gFileName As String
Dim dsE As DirectSoundEnum
Dim POS(0) As DSBPOSITIONNOTIFY

Dim endEvent As Long
Implements DirectXEvent

Private Declare Function GetWindowsDirectory Lib "kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long

Private Sub cboDrivers_Click()
    
    On Local Error GoTo ErrOut
    Set gDS = Nothing
    Set gDSB = Nothing
    Set gDS = gDX.DirectSoundCreate(dsE.GetGuid(cboDrivers.ListIndex + 1))
    DisableButtons True
    
    lblFileLabel.Caption = vbNullString
    lblStatusDesc.Caption = vbNullString
    SetSettings
    chkLoop = False

    'If endEvent Then gDX.DestroyEvent endEvent
    Exit Sub

ErrOut:
    MsgBox "Could not create the sound device.  Exiting application", vbOKOnly Or vbCritical, "Cannot create"
    End
End Sub

Private Sub cmdClose_Click()
    End
End Sub

Function GetWinDir() As String
    Dim s As String * 255
    Dim i%
    
    i% = GetWindowsDirectory(s, Len(s))
    
    GetWinDir = Left(s, i%)
End Function


Private Sub cmdLoadFile_Click()
    
    
    '''''''''''''''''''''''''''''''''
    'Load sound from file
    '''''''''''''''''''''''''''''''''
    
    
    Dim nReturnVal As Integer
    Dim s$
    
    chkLoop = False
    lblStatusDesc.Caption = "Loading File..."
    
    s$ = GetWinDir()
    
    
    comDLG.DialogTitle = "Open Wave File"
    comDLG.Filter = "*.wav"
    comDLG.filename = "*.wav"
    comDLG.InitDir = s$ & "\media\"
    comDLG.ShowOpen
    
        
    With comDLG
        If .filename = vbNullString Or .filename = "*.wav" Or Right(.filename, 4) <> ".wav" Then
            'MsgBox "Please select a valid wav file", vbApplicationModal
            Exit Sub
        End If
    
        nReturnVal = LoadSoundBufferFromFile(.filename)
        If nReturnVal = 1 Then
            frmMain.lblStatusDesc = vbNullString
            Exit Sub
        End If
    End With
    
    
    If nReturnVal <> 1 Then
        DisableButtons False
    End If
    
    ''''''''''''' set up the info
    lblFileLabel.Caption = comDLG.filename
    lblStatusDesc.Caption = "File Loaded"
    
    
    
End Sub

Private Sub CreateEvent()
     endEvent = gDX.CreateEvent(Me)
End Sub
    

Private Function LoadSoundBufferFromFile(sFile As String) As Integer
    '''''''''''''''''''''''''''''''''''
    'Create the sound buffer
    '''''''''''''''''''''''''''''''''''
    On Error GoTo err_out
    
    If gDSB Is Nothing Then InitDirectSound
    
    If sFile <> vbNullString Then
        
        With gD
            .lFlags = DSBCAPS_CTRLVOLUME Or DSBCAPS_CTRLPAN Or DSBCAPS_CTRLFREQUENCY Or DSBCAPS_CTRLPOSITIONNOTIFY
            .lReserved = 0
        End With
        
        Set gDSB = gDS.CreateSoundBufferFromFile(sFile, gD, gW)
        
        With POS(0)
            .hEventNotify = endEvent
            .lOffset = -1
        End With
        gDX.SetEvent endEvent
        gDSB.SetNotificationPositions 1, POS()
        
        sldFreq.Value = gDSB.GetFrequency
        sldPan.Value = gDSB.GetPan
        sldVol.Value = gDSB.GetVolume
        
        LoadSoundBufferFromFile = 0
    Else
        LoadSoundBufferFromFile = 1
    End If
    
    Exit Function
        
err_out:
    'MsgBox Err.Number & " : " & Err.Description
    MsgBox "Error creating sound buffer", vbApplicationModal
    LoadSoundBufferFromFile = 1
    
    
End Function

Private Sub InitDirectSound()
    
    ''''''''''''''''''''''''''''''''''''''''''''''''
    'Init Directsound
    ''''''''''''''''''''''''''''''''''''''''''''''''
    
    Set gDS = gDX.DirectSoundCreate(dsE.GetGuid(cboDrivers.ListIndex + 1))
    gDS.SetCooperativeLevel hWnd, DSSCL_PRIORITY
    
End Sub


Private Sub cmdPlay_Click()
    '''''''''''''''''''''''''''
    'Play buffer
    '''''''''''''''''''''''''''
    
    Dim nVal As Integer
    
    nVal = chkLoop.Value
    
        
    lblStatusDesc.Caption = "Sound Playing..."
    Select Case nVal
        Case 0
            gDSB.Play DSBPLAY_DEFAULT
        Case Else
            gDSB.Play DSBPLAY_LOOPING
    End Select
    
    chkLoop.Enabled = False
    
End Sub

Private Sub cmdStop_Click()
    gDSB.Stop
    gDSB.SetCurrentPosition 0
    'lblStatusDesc.Caption = "Sound Stopped..."
    chkLoop.Enabled = True
End Sub

Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
    lblStatusDesc.Caption = "Sound Stopped..."
    chkLoop.Enabled = True
End Sub

Private Sub Form_Load()
    
    
    CreateEvent
    
    lblFileLabel.Caption = vbNullString
    lblStatusDesc.Caption = vbNullString
    
    DisableButtons True
    SetSettings
    LoadAudioDrivers

    
End Sub

Private Sub LoadAudioDrivers()

    Dim i As Integer
        
    
    
    Set dsE = gDX.GetDSEnum
    
    
    For i = 1 To dsE.GetCount
        cboDrivers.AddItem dsE.GetDescription(i)
    Next i

    cboDrivers.ListIndex = 0


End Sub


Private Sub DisableButtons(bWhAT As Boolean)
    
    Select Case bWhAT
    Case False
        chkLoop.Enabled = True
        cmdPlay.Enabled = True
        cmdStop.Enabled = True
    Case True
        chkLoop.Enabled = False
        cmdPlay.Enabled = False
        cmdStop.Enabled = False
    End Select
    
End Sub

Private Sub ChangeLableValue(lbl As Label, vl As Long)
    lbl.Caption = vl
End Sub

Private Sub SetSettings()
''''''''''''''''''''''''''''''
'Set the setting of the slider
'bars
''''''''''''''''''''''''''''''

sldFreq.Max = 100000
sldFreq.Min = 100
sldFreq.TickStyle = sldNoTicks
sldFreq.LargeChange = 1000

sldPan.Min = -10000
sldPan.Max = 10000
sldPan.TickStyle = sldNoTicks
sldPan.LargeChange = 500

sldVol.Min = -10000
sldVol.Max = 0
sldVol.TickStyle = sldNoTicks
sldVol.LargeChange = 1000


'''''''''''''''''''''''''''''''
'set the location of the
'slider value
'''''''''''''''''''''''''''''''
sldFreq.Value = 0
sldPan.Value = 0
sldVol.Value = 0


'''''''''''''''''''''''''''''
'set the labels for the
'slider bars
'''''''''''''''''''''''''''''

lblFreqDesc.Caption = sldFreq.Value
lblPanDesc.Caption = sldPan.Value
lblVolDesc.Caption = sldVol.Value



End Sub


Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If endEvent Then
        gDX.DestroyEvent endEvent
    End If
End Sub

Private Sub sldFreq_Change()
    ChangeLableValue lblFreqDesc, sldFreq.Value
End Sub

Private Sub sldFreq_Scroll()
    ChangeLableValue lblFreqDesc, sldFreq.Value
    Dim i As Long
    i = sldFreq.Value
    If i <= 100 Then i = 101
    If Not (gDSB Is Nothing) Then gDSB.SetFrequency i
End Sub


Private Sub sldPan_Scroll()
    ChangeLableValue lblPanDesc, sldPan.Value
    If Not (gDSB Is Nothing) Then gDSB.SetPan sldPan.Value
End Sub


Private Sub sldVol_Scroll()
    ChangeLableValue lblVolDesc, sldVol.Value
    If Not (gDSB Is Nothing) Then gDSB.SetVolume sldVol.Value
End Sub
