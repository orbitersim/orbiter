VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Streaming To File"
   ClientHeight    =   1590
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   2460
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1590
   ScaleWidth      =   2460
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdEnd 
      Caption         =   "End Streaming"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   1080
      Width           =   1935
   End
   Begin VB.CommandButton cmdStart 
      Caption         =   "Start Streaming"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   600
      Width           =   1935
   End
   Begin VB.Label lblLSize 
      Caption         =   "File Size:"
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
      Left            =   240
      TabIndex        =   3
      Top             =   120
      Width           =   855
   End
   Begin VB.Label lblSize 
      Caption         =   "Label1"
      Height          =   255
      Left            =   1200
      TabIndex        =   2
      Top             =   120
      Width           =   1095
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Implements DirectXEvent

Private Sub cmdCloseFile_Click()
    gDSCB.Stop
    Close #f
End Sub

''-------------------------------
''Stop the streaming
''-------------------------------
Private Sub cmdEnd_Click()
    Dim fName As String
    gDSCB.Stop
    
    On Local Error Resume Next
    Kill App.Path + "\new.wav"
    fName = InputBox("Enter File Name", "FileName", "New.Wav")
    
    If fName <> vbNullString Then
        On Local Error Resume Next
        Kill App.Path + "\fname"
        SaveToFileAsStream fName
    Else
        MsgBox "Please enter a valid name", vbApplicationModal
    End If
End Sub

''--------------------------------
''Start Button
''--------------------------------
Private Sub cmdStart_Click()
    StartCapture
End Sub

''--------------------------------
''CallBack information
''triggered by the events
''--------------------------------
Private Sub DirectXEvent_DXCallback(ByVal EventID As Long)
    
    Select Case EventID
        Case EVNT(1).hEventNotify
            basStream.CopyBuffer 2
    End Select

End Sub


''-----------------------------------------
''Set the events to the capture buffer
''-----------------------------------------

Sub SetEvents()

    EventID(0) = gDX.CreateEvent(Me)
    EventID(1) = gDX.CreateEvent(Me)
    
    If gDSCB Is Nothing Then Call Init(hWnd)
    
    EVNT(0).hEventNotify = EventID(0)
    EVNT(0).lOffset = 0
    
    EVNT(1).hEventNotify = EventID(1)
    EVNT(1).lOffset = (gDSCBD.lBufferBytes \ 2)
    
    gDSCB.SetNotificationPositions 2, EVNT()
    
End Sub

''-----------------------------------------
''Start the capture buffer rolling
''-----------------------------------------
Sub StartCapture()
    gDSCB.start DSCBSTART_LOOPING
End Sub

''-----------------------------------------
''Clean the labels
''Initalize the buffers
''set the events
''-----------------------------------------
Private Sub Form_Load()
    lblSize.Caption = vbNullString
    Init hWnd
    SetEvents
End Sub

''-----------------------------------------
''Close everything down
''-----------------------------------------
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Dim i As Integer
    
    For i = 0 To UBound(EventID)
        DoEvents
        If EventID(i) Then gDX.DestroyEvent EventID(i)
    Next
    
    Call CloseFiles
    KillTempFile
End Sub

