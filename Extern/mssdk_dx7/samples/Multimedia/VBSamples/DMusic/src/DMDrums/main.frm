VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomct2.ocx"
Begin VB.Form main 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "DMDrums"
   ClientHeight    =   5505
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6255
   Icon            =   "main.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5505
   ScaleWidth      =   6255
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "Exit"
      Height          =   495
      Left            =   5340
      TabIndex        =   31
      Top             =   4980
      Width           =   855
   End
   Begin MSComCtl2.UpDown UpDown_Volume 
      Height          =   375
      Left            =   1740
      TabIndex        =   36
      TabStop         =   0   'False
      Top             =   960
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   661
      _Version        =   393216
      Value           =   50
      Max             =   100
      Enabled         =   -1  'True
   End
   Begin MSComCtl2.UpDown UpDown_Tempo 
      Height          =   375
      Left            =   1740
      TabIndex        =   35
      TabStop         =   0   'False
      Top             =   360
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   661
      _Version        =   393216
      Value           =   120
      Max             =   1000
      Min             =   1
      Enabled         =   -1  'True
   End
   Begin VB.TextBox EDIT_Tempo 
      Height          =   375
      Left            =   1200
      MaxLength       =   4
      TabIndex        =   0
      Text            =   "120"
      Top             =   360
      Width           =   495
   End
   Begin VB.TextBox EDIT_Volume 
      Height          =   375
      Left            =   1200
      TabIndex        =   1
      Text            =   "50"
      Top             =   960
      Width           =   495
   End
   Begin VB.CommandButton Stop 
      Height          =   495
      Left            =   3720
      Picture         =   "main.frx":0442
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   840
      Width           =   495
   End
   Begin VB.CommandButton Play 
      Height          =   495
      Left            =   3120
      Picture         =   "main.frx":08F8
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   840
      Width           =   495
   End
   Begin VB.PictureBox Picture1 
      BorderStyle     =   0  'None
      Height          =   1695
      Left            =   120
      Picture         =   "main.frx":0E8A
      ScaleHeight     =   1695
      ScaleWidth      =   855
      TabIndex        =   32
      TabStop         =   0   'False
      Top             =   120
      Width           =   855
   End
   Begin VB.ListBox LIST_Grooves 
      Height          =   2400
      Left            =   4320
      TabIndex        =   29
      Top             =   840
      Width           =   1815
   End
   Begin VB.ListBox LIST_Bands 
      Height          =   1425
      Left            =   4320
      TabIndex        =   30
      Top             =   3480
      Width           =   1815
   End
   Begin VB.CommandButton Drum 
      Caption         =   "High Q"
      Height          =   495
      Index           =   24
      Left            =   3480
      TabIndex        =   28
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Scratch"
      Height          =   495
      Index           =   23
      Left            =   2640
      TabIndex        =   27
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Sticks"
      Height          =   495
      Index           =   22
      Left            =   1800
      TabIndex        =   26
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Hand Clap"
      Height          =   495
      Index           =   21
      Left            =   960
      TabIndex        =   25
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Tamb- ourine"
      Height          =   495
      Index           =   20
      Left            =   120
      TabIndex        =   24
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Jingle Bells"
      Height          =   495
      Index           =   19
      Left            =   3480
      TabIndex        =   23
      Top             =   3840
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Cast- anets"
      Height          =   495
      Index           =   18
      Left            =   2640
      TabIndex        =   22
      Top             =   3840
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Shaker"
      Height          =   495
      Index           =   17
      Left            =   1800
      TabIndex        =   21
      Top             =   3840
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Triangle"
      Height          =   495
      Index           =   16
      Left            =   960
      TabIndex        =   20
      Top             =   3840
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Cuica"
      Height          =   495
      Index           =   15
      Left            =   120
      TabIndex        =   19
      Top             =   3840
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "High Block"
      Height          =   495
      Index           =   14
      Left            =   3480
      TabIndex        =   18
      Top             =   3240
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Low Block"
      Height          =   495
      Index           =   13
      Left            =   2640
      TabIndex        =   17
      Top             =   3240
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Guiro"
      Height          =   495
      Index           =   12
      Left            =   1800
      TabIndex        =   16
      Top             =   3240
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Agogo"
      Height          =   495
      Index           =   11
      Left            =   960
      TabIndex        =   15
      Top             =   3240
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Timbale"
      Height          =   495
      Index           =   10
      Left            =   120
      TabIndex        =   14
      Top             =   3240
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "High Conga"
      Height          =   495
      Index           =   9
      Left            =   3480
      TabIndex        =   13
      Top             =   2640
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Low Conga"
      Height          =   495
      Index           =   8
      Left            =   2640
      TabIndex        =   12
      Top             =   2640
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Crash"
      Height          =   495
      Index           =   7
      Left            =   1800
      TabIndex        =   11
      Top             =   2640
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Splash"
      Height          =   495
      Index           =   6
      Left            =   960
      TabIndex        =   10
      Top             =   2640
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Ride"
      Height          =   495
      Index           =   5
      Left            =   120
      TabIndex        =   9
      Top             =   2640
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "High Tom"
      Height          =   495
      Index           =   4
      Left            =   3480
      TabIndex        =   8
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Mid Tom"
      Height          =   495
      Index           =   3
      Left            =   2640
      TabIndex        =   7
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Low Tom"
      Height          =   495
      Index           =   2
      Left            =   1800
      TabIndex        =   6
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Snare"
      Height          =   495
      Index           =   1
      Left            =   960
      TabIndex        =   5
      Top             =   2040
      Width           =   735
   End
   Begin VB.CommandButton Drum 
      Caption         =   "Kick"
      Height          =   495
      Index           =   0
      Left            =   120
      TabIndex        =   4
      Top             =   2040
      Width           =   735
   End
   Begin VB.Label lblInfo 
      BackStyle       =   0  'Transparent
      Caption         =   "Drum Sets"
      Height          =   255
      Index           =   1
      Left            =   4320
      TabIndex        =   38
      Top             =   3240
      Width           =   1755
   End
   Begin VB.Label lblInfo 
      BackStyle       =   0  'Transparent
      Caption         =   "Styles"
      Height          =   255
      Index           =   0
      Left            =   4320
      TabIndex        =   37
      Top             =   600
      Width           =   1755
   End
   Begin VB.Label Label2 
      Caption         =   "Tempo:"
      Height          =   255
      Left            =   1200
      TabIndex        =   34
      Top             =   120
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Volume:"
      Height          =   255
      Left            =   1200
      TabIndex        =   33
      Top             =   720
      Width           =   615
   End
End
Attribute VB_Name = "main"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim dx As New DirectX7
Dim perf As DirectMusicPerformance
Dim loader As DirectMusicLoader
Dim style As DirectMusicStyle
Dim band As DirectMusicBand
Dim composer As DirectMusicComposer
Dim seg As DirectMusicSegment
Dim segBand As DirectMusicSegment
Dim segMotif(100) As DirectMusicSegment
Dim mediapath As String



Dim mtTime As Long



Private Sub cmdExit_Click()
    Unload Me
End Sub

Private Sub Drum_KeyDown(Index As Integer, KeyCode As Integer, Shift As Integer)
    Call perf.PlaySegment(segMotif(Index), DMUS_SEGF_SECONDARY, 0)
End Sub

Private Sub Drum_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    Call perf.PlaySegment(segMotif(Index), DMUS_SEGF_SECONDARY, 0)
End Sub

Private Sub EDIT_Tempo_KeyPress(KeyAscii As Integer)
    If KeyAscii = vbKeyReturn Then
        If Val(EDIT_Tempo.Text) > 0 And Val(EDIT_Tempo.Text) < 1001 And IsNumeric(EDIT_Tempo.Text) Then
            UpDown_Tempo.Value = EDIT_Tempo.Text
            ChangeTempo (UpDown_Tempo.Value)
        Else
            EDIT_Tempo.Text = UpDown_Tempo.Value
        End If
    End If
End Sub

Private Sub EDIT_Tempo_LostFocus()
    If Val(EDIT_Tempo.Text) > 0 And Val(EDIT_Tempo.Text) < 1001 And IsNumeric(EDIT_Tempo.Text) Then
        UpDown_Tempo.Value = EDIT_Tempo.Text
        ChangeTempo (UpDown_Tempo.Value)
    Else
        EDIT_Tempo.Text = UpDown_Tempo.Value
    End If
End Sub

Private Sub EDIT_Volume_KeyPress(KeyAscii As Integer)
    If KeyAscii = vbKeyReturn Then
        If IsNumeric(EDIT_Volume.Text) And Val(EDIT_Volume.Text) >= 0 And Val(EDIT_Volume.Text) < 101 Then
            UpDown_Volume.Value = EDIT_Volume.Text
            Call ChangeVolume(UpDown_Volume.Value)
            'Call perf.SetMasterVolume((UpDown_Volume.Value * 60) - 3000)
        Else
            EDIT_Volume.Text = UpDown_Volume.Value
        End If
    End If
End Sub

Private Sub EDIT_Volume_LostFocus()
    If IsNumeric(EDIT_Volume.Text) And Val(EDIT_Volume.Text) >= 0 And Val(EDIT_Volume.Text) < 101 Then
        UpDown_Volume.Value = EDIT_Volume.Text
        Call perf.SetMasterVolume((UpDown_Volume.Value * 60) - 3000)
    Else
        EDIT_Volume.Text = UpDown_Volume.Value
    End If

End Sub

Private Sub Form_Load()

    FindMediaDir "Drums!.sgt"
    mediapath = CurDir$
    
    Set perf = dx.DirectMusicPerformanceCreate()
    Set loader = dx.DirectMusicLoaderCreate()
    Set composer = dx.DirectMusicComposerCreate()
    
    ' Initialize performance object to use its own DirectSound object
    Call perf.Init(Nothing, 0)
    
    ' SetPort allows us to select the music synthesizor
    ' passing -1 for the index will have the performance object
    ' select one for you (usually the Microsoft Software Synth)
    ' the number of groups indicate how many voices you need in
    ' increments of 16 (passing in 4 will give us a total of 64)
    Call perf.SetPort(-1, 4)
    
    ' SetMasterAutoDownload indicates we the perofmance object
    ' to attempt to auto download DLS collections when reference in
    ' sgt and sty files
    Call perf.SetMasterAutoDownload(True)


    Set style = loader.LoadStyle(mediapath + "\drums!.sty")

    Set seg = loader.LoadSegment(mediapath + "\drums!.sgt")
    
    
    Get_Bands

    LIST_Grooves.AddItem ("Alternative")
    LIST_Grooves.AddItem ("Blues")
    LIST_Grooves.AddItem ("Country")
    LIST_Grooves.AddItem ("Dance - Pop")
    LIST_Grooves.AddItem ("Hard Rock")
    LIST_Grooves.AddItem ("Hip Hop")
    LIST_Grooves.AddItem ("Jazz")
    LIST_Grooves.AddItem ("Latin")
    LIST_Grooves.AddItem ("R & B")
    LIST_Grooves.AddItem ("Rap")
    LIST_Grooves.AddItem ("Soft Rock")
    LIST_Grooves.AddItem ("World")
    
    ' Download the default band so that we can play the drum pads immediately
    ChangeBands
    
    
    For i = 0 To style.GetMotifCount() - 1
        MotifName = style.GetMotifName(i)
        Set segMotif(i) = style.GetMotif(MotifName)
    Next
    
    LIST_Grooves.ListIndex = 0
    LIST_Bands.ListIndex = 0
    
End Sub

Private Sub Form_Unload(Cancel As Integer)

    If Not (band Is Nothing) Then
        Call band.Unload(perf)
    End If

    If Not (perf Is Nothing) Then
        Call perf.CloseDown
    End If

End Sub

Private Sub Get_Bands()
    Dim BandCount As Integer
    Dim counter As Integer
    BandCount = style.GetBandCount()
    For counter = 0 To (BandCount - 1)
        LIST_Bands.AddItem (style.GetBandName(BandCount - counter - 1))
    Next counter
End Sub

Private Sub LIST_Bands_Click()
    ChangeBands
End Sub

Private Sub LIST_Grooves_Click()
    perf.SetMasterGrooveLevel ((LIST_Grooves.ListIndex * 8) + 1)
End Sub

Private Sub Play_Click()
    PlaySeg
    ChangeBands
End Sub

Private Sub Stop_Click()
    Call perf.Stop(seg, Nothing, 0, 0)
End Sub

Private Sub UPDOWN_Tempo_Change()
    EDIT_Tempo.Text = UpDown_Tempo.Value
    ChangeTempo (UpDown_Tempo.Value)
End Sub

Private Sub UPDOWN_Volume_Change()
    EDIT_Volume.Text = UpDown_Volume.Value
    Call ChangeVolume(UpDown_Volume.Value)
End Sub

Private Sub ChangeBands()

    If Not (band Is Nothing) Then
        Call band.Unload(perf)
    End If

    If LIST_Bands = "" Then
        Set band = style.GetBand("Standard")
    Else
        Set band = style.GetBand(LIST_Bands)
    End If
    Call band.Download(perf)
    Set segBand = band.CreateSegment()
    Call perf.PlaySegment(segBand, DMUS_SEGF_SECONDARY, 0)
End Sub

Private Sub PlaySeg()
    Call perf.PlaySegment(seg, 0, 0)
End Sub

Private Sub ChangeTempo(tempo As Integer)
    
    'Get the time
    mtTime = perf.GetMusicTime()
    Call perf.SendTempoPMSG(mtTime, 10, tempo)

End Sub


Sub ChangeVolume(ByVal n As Long)
    If n = 0 Then
        n = -10000
    Else
        n = (-50 * (50 - n))
    End If
    
    perf.SetMasterVolume n
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

