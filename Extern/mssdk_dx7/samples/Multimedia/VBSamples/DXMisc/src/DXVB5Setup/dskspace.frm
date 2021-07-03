VERSION 5.00
Begin VB.Form frmDskSpace 
   AutoRedraw      =   -1  'True
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "#"
   ClientHeight    =   2550
   ClientLeft      =   870
   ClientTop       =   1530
   ClientWidth     =   5355
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "dskspace.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2550
   ScaleWidth      =   5355
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   3570
      MaskColor       =   &H00000000&
      TabIndex        =   2
      Top             =   1965
      Width           =   1560
   End
   Begin VB.CommandButton cmdInstall 
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   1875
      MaskColor       =   &H00000000&
      TabIndex        =   1
      Top             =   1965
      Width           =   1560
   End
   Begin VB.CommandButton cmdChgDrv 
      Caption         =   "#"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   192
      MaskColor       =   &H00000000&
      TabIndex        =   0
      Top             =   1968
      Width           =   1560
   End
   Begin VB.Label lblDisk 
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   216
      Index           =   0
      Left            =   240
      TabIndex        =   4
      Top             =   1308
      Visible         =   0   'False
      Width           =   504
   End
   Begin VB.Label lblAvail 
      Alignment       =   1  'Right Justify
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   216
      Index           =   0
      Left            =   2340
      TabIndex        =   5
      Top             =   1308
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label lblNeed 
      Alignment       =   1  'Right Justify
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   216
      Index           =   0
      Left            =   3888
      TabIndex        =   6
      Top             =   1308
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label lblReq 
      Alignment       =   1  'Right Justify
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   216
      Index           =   0
      Left            =   804
      TabIndex        =   7
      Top             =   1308
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Shape shpSpace 
      BorderColor     =   &H00000000&
      Height          =   396
      Left            =   192
      Top             =   1224
      Width           =   4980
   End
   Begin VB.Label lblDiskH 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   240
      TabIndex        =   8
      Top             =   1005
      Width           =   105
   End
   Begin VB.Label lblAvailH 
      Alignment       =   1  'Right Justify
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   408
      Left            =   2352
      TabIndex        =   11
      Top             =   804
      Width           =   1260
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblNeedH 
      Alignment       =   1  'Right Justify
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   408
      Left            =   3888
      TabIndex        =   10
      Top             =   804
      Width           =   1260
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblReqH 
      Alignment       =   1  'Right Justify
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   408
      Left            =   804
      TabIndex        =   9
      Top             =   804
      Width           =   1260
      WordWrap        =   -1  'True
   End
   Begin VB.Shape shpHeading 
      BorderColor     =   &H00000000&
      Height          =   480
      Left            =   192
      Top             =   744
      Width           =   4980
   End
   Begin VB.Label lblNoSpace 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   195
      TabIndex        =   3
      Top             =   150
      Width           =   105
   End
End
Attribute VB_Name = "frmDskSpace"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_TemplateDerived = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'
' Form/Module Constants
'
Const strFMT$ = "######0 K"

Private Sub cmdChgDrv_Click()
    gfRetVal = gintRET_CANCEL
    Unload Me
End Sub

Private Sub cmdExit_Click()
    ExitSetup Me, gintRET_EXIT
End Sub

Private Sub cmdInstall_Click()
    gfRetVal = gintRET_CONT
    Unload Me
End Sub

Private Sub Form_Load()
    Const ONE_K& = 1024

    Dim intIdx As Integer
    Dim lAvail As Long
    Dim lReq As Long
    Dim intHeight As Integer
    Dim intTop As Integer

    SetFormFont Me
    cmdExit.Caption = ResolveResString(resBTNEXIT)
    cmdInstall.Caption = ResolveResString(resBTNINSTALLNOW)
    cmdChgDrv.Caption = ResolveResString(resBTNCHGDRV)
    lblDiskH.Caption = ResolveResString(resLBLDRIVE)
    lblAvailH.Caption = ResolveResString(resLBLAVAIL)
    lblNeedH.Caption = ResolveResString(resLBLNEEDED)
    lblReqH.Caption = ResolveResString(resLBLREQUIRED)
    lblNoSpace.Caption = ResolveResString(resLBLNOSPACE)
    frmDskSpace.Caption = gstrTitle

    intHeight = lblDisk(0).Height * 1.6
    intTop = lblDisk(0).Top

    '
    'borders are for design mode only...
    '
    lblDisk(0).BorderStyle = 0
    lblReq(0).BorderStyle = 0
    lblAvail(0).BorderStyle = 0
    lblNeed(0).BorderStyle = 0

    For intIdx = 1 To Len(gstrDrivesUsed)
        Load lblDisk(intIdx)
        Load lblReq(intIdx)
        Load lblAvail(intIdx)
        Load lblNeed(intIdx)

        lAvail = gsDiskSpace(intIdx).lAvail
        lReq = gsDiskSpace(intIdx).lReq

        lblDisk(intIdx).Caption = Mid$(gstrDrivesUsed, intIdx, 1) & gstrCOLON
        lblReq(intIdx).Caption = Format$(lReq / ONE_K, strFMT)
        lblAvail(intIdx).Caption = Format$(lAvail / ONE_K, strFMT)
        lblNeed(intIdx).Caption = Format$(IIf(lReq > lAvail, lReq - lAvail, 0) / ONE_K, strFMT)

        lblDisk(intIdx).Top = intTop
        lblReq(intIdx).Top = intTop
        lblAvail(intIdx).Top = intTop
        lblNeed(intIdx).Top = intTop

        intTop = intTop + intHeight

        lblDisk(intIdx).Visible = True
        lblReq(intIdx).Visible = True
        lblAvail(intIdx).Visible = True
        lblNeed(intIdx).Visible = True
    Next

    shpSpace.Height = intHeight * (intIdx - 1)

    cmdChgDrv.Top = shpSpace.Top + shpSpace.Height + cmdChgDrv.Height
    cmdInstall.Top = cmdChgDrv.Top
    cmdExit.Top = cmdChgDrv.Top

    frmDskSpace.Height = cmdChgDrv.Top + cmdChgDrv.Height * 2.5

    EtchedLine Me, 100, cmdChgDrv.Top - cmdChgDrv.Height * 0.5, Me.ScaleWidth - 200

    CenterForm Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode <> 1 Then
        ExitSetup Me, gintRET_EXIT
        Cancel = 1
    End If
End Sub

