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
   HasDC           =   0   'False
   Icon            =   "dskspace.frx":0000
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   ScaleHeight     =   2550
   ScaleWidth      =   5355
   ShowInTaskbar   =   0   'False
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
      Left            =   195
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
      TabIndex        =   0
      Top             =   1965
      Width           =   1560
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
      TabIndex        =   11
      Top             =   150
      Width           =   105
   End
   Begin VB.Shape shpHeading 
      BorderColor     =   &H00000000&
      Height          =   480
      Left            =   195
      Top             =   750
      Width           =   4980
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
      Height          =   405
      Left            =   810
      TabIndex        =   10
      Top             =   810
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
      Height          =   405
      Left            =   3885
      TabIndex        =   9
      Top             =   810
      Width           =   1260
      WordWrap        =   -1  'True
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
      Height          =   405
      Left            =   2355
      TabIndex        =   8
      Top             =   810
      Width           =   1260
      WordWrap        =   -1  'True
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
      TabIndex        =   7
      Top             =   1005
      Width           =   105
   End
   Begin VB.Shape shpSpace 
      BorderColor     =   &H00000000&
      Height          =   390
      Left            =   195
      Top             =   1230
      Width           =   4980
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
      Height          =   210
      Index           =   0
      Left            =   810
      TabIndex        =   6
      Top             =   1305
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
      Height          =   210
      Index           =   0
      Left            =   3885
      TabIndex        =   5
      Top             =   1305
      Visible         =   0   'False
      Width           =   1260
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
      Height          =   210
      Index           =   0
      Left            =   2340
      TabIndex        =   4
      Top             =   1305
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label lblDisk 
      AutoSize        =   -1  'True
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
      Height          =   210
      Index           =   0
      Left            =   240
      TabIndex        =   3
      Top             =   1305
      Visible         =   0   'False
      Width           =   510
   End
End
Attribute VB_Name = "frmDskSpace"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Const mstrFMT$ = "######0 K"

Private Sub cmdChgDrv_Click()
    gintRetVal = gintRET_CANCEL
    Unload Me
End Sub

Private Sub cmdExit_Click()
    ExitSetup Me, gintRET_EXIT
End Sub

Private Sub cmdInstall_Click()
    gintRetVal = gintRET_CONT
    Unload Me
End Sub

Private Sub Form_Load()
    Const ONE_K& = 1024

    Dim intIdx As Integer
    Dim lAvail As Long
    Dim lReq As Long
    Dim lTmp As Long
    Dim intHeight As Integer
    Dim intTop As Integer
    Dim sDrive As String

    Dim nCurrentWidth As Single
    Dim nMaxWidth As Single

    nCurrentWidth = lblDisk(0).Width
    nMaxWidth = nCurrentWidth

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
    lblDisk(0).BorderStyle = vbTransparent
    lblReq(0).BorderStyle = vbTransparent
    lblAvail(0).BorderStyle = vbTransparent
    lblNeed(0).BorderStyle = vbTransparent

    For intIdx = 1 To DriveCount
        Load lblDisk(intIdx)
        Load lblReq(intIdx)
        Load lblAvail(intIdx)
        Load lblNeed(intIdx)

        lAvail = gsDiskSpace(intIdx).lAvail
        lReq = gsDiskSpace(intIdx).lReq

        sDrive = DriveFromDriveIndex(intIdx)
        RemoveDirSep sDrive
        lblDisk(intIdx).Caption = sDrive
        If lblDisk(intIdx).Width > nMaxWidth Then
            nMaxWidth = lblDisk(intIdx).Width
        End If
        lblReq(intIdx).Caption = Format$(lReq / ONE_K, mstrFMT)
        lblAvail(intIdx).Caption = Format$(lAvail / ONE_K, mstrFMT)
        If lReq > lAvail Then
            lTmp = lReq - lAvail
        Else
            lTmp = 0
        End If
        lblNeed(intIdx).Caption = Format$(lTmp / ONE_K, mstrFMT)

        lblDisk(intIdx).Top = intTop
        lblReq(intIdx).Top = intTop
        lblAvail(intIdx).Top = intTop
        lblNeed(intIdx).Top = intTop

        intTop = intTop + intHeight

        lblDisk(intIdx).Visible = True
        lblReq(intIdx).Visible = True
        lblAvail(intIdx).Visible = True
        lblNeed(intIdx).Visible = True
    Next intIdx
    If nMaxWidth <> nCurrentWidth Then
        nMaxWidth = nMaxWidth - nCurrentWidth
        For intIdx = 1 To DriveCount
            lblReq(intIdx).Left = lblReq(intIdx).Left + nMaxWidth
            lblAvail(intIdx).Left = lblAvail(intIdx).Left + nMaxWidth
            lblNeed(intIdx).Left = lblNeed(intIdx).Left + nMaxWidth
        Next intIdx
        lblReqH.Left = lblReqH.Left + nMaxWidth
        lblAvailH.Left = lblAvailH.Left + nMaxWidth
        lblNeedH.Left = lblNeedH.Left + nMaxWidth
        shpHeading.Width = shpHeading.Width + nMaxWidth
        shpSpace.Width = shpSpace.Width + nMaxWidth
        Width = Width + nMaxWidth
        nMaxWidth = nMaxWidth / 3
        cmdChgDrv.Width = cmdChgDrv.Width + nMaxWidth
        cmdInstall.Left = cmdInstall.Left + nMaxWidth
        cmdInstall.Width = cmdInstall.Width + nMaxWidth
        cmdExit.Left = cmdExit.Left + (2 * nMaxWidth)
        cmdExit.Width = cmdExit.Width + nMaxWidth
    End If
    
    shpSpace.Height = intHeight * (intIdx - 1)

    cmdChgDrv.Top = shpSpace.Top + shpSpace.Height + cmdChgDrv.Height
    cmdInstall.Top = cmdChgDrv.Top
    cmdExit.Top = cmdChgDrv.Top

    frmDskSpace.Height = cmdChgDrv.Top + cmdChgDrv.Height * 2.5

    EtchedLine Me, 100, cmdChgDrv.Top - cmdChgDrv.Height * 0.5, ScaleWidth - 200

    CenterForm Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    HandleFormQueryUnload UnloadMode, Cancel, Me
End Sub
