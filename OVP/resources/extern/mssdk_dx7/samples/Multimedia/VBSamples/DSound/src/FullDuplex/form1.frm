VERSION 5.00
Begin VB.Form frmFullDuplex 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Full Duplex"
   ClientHeight    =   2130
   ClientLeft      =   150
   ClientTop       =   435
   ClientWidth     =   3150
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2130
   ScaleWidth      =   3150
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame fraInfo 
      Caption         =   "Full Duplex"
      Height          =   1695
      Left            =   240
      TabIndex        =   0
      Top             =   120
      Width           =   2775
      Begin VB.CommandButton cmdEnd 
         Caption         =   "S&top"
         Height          =   495
         Left            =   1320
         TabIndex        =   2
         Top             =   960
         Width           =   1215
      End
      Begin VB.CommandButton cmdStart 
         Caption         =   "&Start"
         Height          =   495
         Left            =   1320
         TabIndex        =   1
         Top             =   360
         Width           =   1215
      End
   End
End
Attribute VB_Name = "frmFullDuplex"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'==========================================================
' This program is a sample of using DirectSound for full
' duplex activities.  The only issue is a small latency
' Dominic Riccetti 1/28/1999
' This also shows how to use the DirectXEvent
'==========================================================
Dim dxs As New DirectX7
Dim MultiLoop As Boolean ' for the looping of the sounds


Implements DirectXEvent


Private Sub cmdEnd_Click()
    fraInfo.Caption = "Full Duplex"
    cmdStart.Enabled = True
    Call StopAll
End Sub

Private Sub cmdStart_Click()
On Error GoTo err_out

fraInfo.Caption = "Full Duplex : Running"
cmdStart.Enabled = False

Call record

AllTogetherNow
RunBuffers

Do While MultiLoop <> True
    CopyBuffers
    
    DoEvents ' By passing the doevents command we wave passage to other processes
             ' however if you were to do one process over and over you might get
             ' skipping until the process is released.  Also this
             ' allows the user to kill the program when finished.

    
    
    
Loop
Exit Sub

err_out:
    MsgBox Err.Description, vbApplicationModal
    End
    
End Sub

Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
On Error GoTo err_out

    Select Case eventid
        Case Is = lid1
            Part_one
            num = 0
        Case Is = lid2
            part_two
            num = 1
        Case Is = lid3
            num = 2
    End Select
Exit Sub

err_out:
    MsgBox Err.Description, vbApplicationModal
    End
End Sub

Private Sub form_load()
On Error GoTo err_out

    'set all the events active
    Me.Show
    
    lid1 = DX.CreateEvent(Me)
    lid2 = DX.CreateEvent(Me)
    lid3 = DX.CreateEvent(Me)
    
    DoEvents

    InitSoundDevices Me.hWnd ' initiate the direct sound
                             ' and the direct sound capture
                             ' devices

Exit Sub

err_out:
    MsgBox Err.Description, vbApplicationModal
    End
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
On Error GoTo err_out

    ' when the form is closed kill the process
    MultiLoop = False
    
    'end the program
    
    End
Exit Sub

err_out:
    MsgBox Err.Description, vbApplicationModal
    End
End Sub

