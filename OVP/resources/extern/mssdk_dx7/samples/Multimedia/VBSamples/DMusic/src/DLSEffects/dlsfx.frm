VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "DLS Sound Effects"
   ClientHeight    =   4920
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5670
   Icon            =   "DLSFX.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4920
   ScaleWidth      =   5670
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame1 
      Caption         =   "Heartbeat"
      Height          =   3255
      Left            =   120
      TabIndex        =   17
      Top             =   1560
      Width           =   2895
      Begin VB.CommandButton cmdB7 
         Caption         =   "&On"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   480
         TabIndex        =   8
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton cmdOff 
         Caption         =   "O&ff"
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   1800
         TabIndex        =   9
         Top             =   360
         Width           =   735
      End
      Begin MSComctlLib.Slider sliderB7 
         Height          =   375
         Left            =   360
         TabIndex        =   11
         Top             =   1440
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   661
         _Version        =   393216
         LargeChange     =   3
         Min             =   1
         Max             =   13
         SelStart        =   1
         Value           =   1
      End
      Begin MSComctlLib.Slider sliderPitch 
         Height          =   375
         Left            =   360
         TabIndex        =   13
         Top             =   2400
         Width           =   2295
         _ExtentX        =   4048
         _ExtentY        =   661
         _Version        =   393216
         LargeChange     =   1365
         SmallChange     =   128
         Max             =   16383
         SelStart        =   8065
         TickFrequency   =   1365
         Value           =   8065
      End
      Begin VB.Label Label2 
         Caption         =   "&Note (B7-B8)"
         Height          =   255
         Left            =   480
         TabIndex        =   10
         Top             =   1080
         Width           =   1695
      End
      Begin VB.Label lblPitch 
         Caption         =   "&Pitch Bend"
         Height          =   255
         Left            =   480
         TabIndex        =   12
         Top             =   2040
         Width           =   1815
      End
   End
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "E&xit"
      Height          =   495
      Left            =   3720
      TabIndex        =   16
      Top             =   3120
      Width           =   1335
   End
   Begin VB.CommandButton cmdC10 
      BackColor       =   &H00FFFFFF&
      Caption         =   "C&10"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   4688
      TabIndex        =   6
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC9 
      Caption         =   "C&9"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   3968
      TabIndex        =   5
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC5 
      Caption         =   "C&5"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1808
      TabIndex        =   2
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC4 
      Caption         =   "C&4"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1088
      TabIndex        =   1
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC6 
      Caption         =   "C&6"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   2528
      TabIndex        =   3
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC3 
      Caption         =   "C&3"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   368
      TabIndex        =   0
      Top             =   720
      Width           =   615
   End
   Begin VB.CommandButton cmdC7 
      Caption         =   "C&7"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   3248
      TabIndex        =   4
      Top             =   720
      Width           =   615
   End
   Begin MSComctlLib.Slider sliderVelocity 
      Height          =   375
      Left            =   3360
      TabIndex        =   15
      Top             =   1920
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   661
      _Version        =   393216
      LargeChange     =   16
      Max             =   127
      SelStart        =   127
      TickFrequency   =   16
      Value           =   127
   End
   Begin VB.Label Label3 
      Caption         =   "&Velocity of New Notes"
      Height          =   255
      Left            =   3480
      TabIndex        =   14
      Top             =   1560
      Width           =   1935
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Boids.dls - ""Vocals"" Instrument Regions"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   428
      TabIndex        =   7
      Top             =   240
      Width           =   4815
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' DLS Effects Sample
' Copyright (c) 1999 Microsoft Corporation


' This application demonstrates the use of Downloadable Sounds for sound
' effects, and how to send MIDI notes.

' The DLS instruments are taken from Boids.dls. That collection actually
' contains only a single instrument, called Vocals. However, the instrument
' is based on different wave samples for different "regions" or ranges
' of notes. For example, the first speech sound is used when any note
' between C3 and B3 is sent. The speech sounds are played at the correct
' pitch only when the note is the lowest one in the region.

' One of the samples, called Heartbeat, is valid for the range B7-B8.
' Because this is not a vocal sample, we can reasonably vary the pitch
' by playing various notes within that range, as determined by the
' slider setting.

' Heartbeat is also the only sample in the DLS collection that is based
' on a looped wave. Hence it can be played continuously for up to the
' maximum duration of a note. The other samples will play only once
' regardless of the duration of the note sent.

Option Explicit

Const patch = 127 ' Assigned to "Vocals" instrument in Boids.dls
Const channel = 1
Const hbchannel = 32

' NoteDur is the duration of any of the non-repeating samples. It should
' be long enough to accommodate all the sound effects but not so long
' that notes continue using up resources (voices) after the sample has
' finished playing. Note that if you send the same note before the last
' one has finished playing, it might not play properly.

Const NoteDur = 6000  ' milliseconds

Dim B7Freq As Byte
Dim B7Playing As Boolean
Dim gVelocity As Byte
Dim mediapath As String

Dim dx As New DirectX7
Dim perf As DirectMusicPerformance
Dim coll As DirectMusicCollection
Dim seg As DirectMusicSegment

Private Sub SendNote(chan As Integer, pitch As Byte, dur As Long)
  
  Dim noteMsg As DMUS_NOTE_PMSG
  noteMsg.velocity = gVelocity
  noteMsg.flags = DMUS_NOTEF_NOTEON
  noteMsg.midiValue = pitch
  noteMsg.mtDuration = dur
  Call perf.SendNotePMSG(0, DMUS_PMSGF_REFTIME, chan, noteMsg)
    
 End Sub
 
 Private Sub B7NoteOff()
 
 ' To turn off a note, we send a note-off message on the same
 ' channel and at the same pitch.
 
  Dim noteMsg As DMUS_NOTE_PMSG
  noteMsg.flags = 0
  noteMsg.midiValue = B7Freq
  Call perf.SendNotePMSG(0, DMUS_PMSGF_REFTIME, hbchannel, noteMsg)
 End Sub
 

Private Sub cmdB7_Click()
  ' For the hearbeat we'll send the note using a standard MIDI message.
  ' That way we don't have to worry about the duration of the note;
  ' it will play till we stop it.
  Call perf.SendMIDIPMSG(0, DMUS_PMSGF_REFTIME, hbchannel, &H90, B7Freq, gVelocity)
  B7Playing = True
  cmdB7.Enabled = False
  cmdOff.Enabled = True
End Sub

Private Sub cmdExit_Click()
  Unload Me
End Sub

Private Sub cmdOff_Click()
  B7NoteOff
  B7Playing = False
  cmdB7.Enabled = True
  cmdOff.Enabled = False
End Sub

Private Sub cmdC3_Click()
  SendNote channel, 36, NoteDur
End Sub

Private Sub cmdC4_Click()
  SendNote channel, 48, NoteDur
End Sub

Private Sub cmdC5_Click()
  SendNote channel, 60, NoteDur
End Sub

Private Sub cmdC6_Click()
  SendNote channel, 72, NoteDur
End Sub

Private Sub cmdC7_Click()
  SendNote channel, 84, NoteDur
End Sub

Private Sub cmdC9_Click()
  SendNote channel, 108, NoteDur
End Sub

Private Sub cmdC10_Click()
  SendNote channel, 120, NoteDur
End Sub

Private Sub Form_Load()

  On Error GoTo FAILEDINIT
  Set perf = dx.DirectMusicPerformanceCreate
  Call perf.Init(Nothing, 0)
  Call perf.SetPort(-1, 16)
  On Error GoTo 0
  
  On Error GoTo FAILEDLOAD
  mediapath = FindMediaDir("sample.sgt", "dmusic")
  If mediapath <> vbNullString Then ChDir mediapath
  
  Dim loader As DirectMusicLoader
  Set loader = dx.DirectMusicLoaderCreate
  Set coll = loader.LoadCollection(mediapath & "boids.dls")
  
  ' Load any segment. We're not actually going to play it,
  ' but we need a valid segment object so we can download the DLS.
  
  Set seg = loader.LoadSegment(mediapath & "sample.sgt")
  Call seg.ConnectToCollection(coll)
  Call seg.Download(perf)
  On Error GoTo 0
  
  ' Assign the Vocals instrument to two channels
  ' One will be used only for the heartbeat so we can pitch bend
  Call perf.SendPatchPMSG(0, DMUS_PMSGF_REFTIME, channel, patch, 0, 0)
  Call perf.SendPatchPMSG(0, DMUS_PMSGF_REFTIME, hbchannel, patch, 0, 0)
  
  ' Initialize heartbeat note. B7 is MIDI note 95.
  B7Freq = sliderB7.Value + 94
  gVelocity = sliderVelocity.Value
  
  Exit Sub
  
FAILEDINIT:
  MsgBox "Failed to initialize DirectMusic."
  Unload Me
  Exit Sub
FAILEDLOAD:
  MsgBox "Failed to load file."
  Unload Me
  Exit Sub
End Sub


Private Sub Form_Unload(Cancel As Integer)
    If Not (perf Is Nothing) Then perf.CloseDown
    End
End Sub

Private Sub sliderB7_Change()
  B7NoteOff
  B7Freq = 94 + sliderB7.Value
  If B7Playing Then
    Call perf.SendMIDIPMSG(0, DMUS_PMSGF_REFTIME, hbchannel, &H90, B7Freq, gVelocity)
  End If
 
End Sub

Private Sub sliderPitch_Change()
    Dim hi As Byte, lo As Byte
    
    ' Split value into 7-bit bytes
    hi = Fix(sliderPitch.Value / 128)
    lo = CByte(sliderPitch.Value And 127)
    
    ' Send pitch bend message
    Call perf.SendMIDIPMSG(0, DMUS_PMSGF_REFTIME, hbchannel, &HE0, _
        lo, hi)
    
End Sub

Private Sub sliderVelocity_Change()
  gVelocity = sliderVelocity.Value
End Sub

