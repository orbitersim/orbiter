VERSION 5.00
Begin VB.Form frmOverwrite 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "frmOverwrite"
   ClientHeight    =   3990
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5160
   ClipControls    =   0   'False
   HasDC           =   0   'False
   Icon            =   "Overwrit.frx":0000
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   ScaleHeight     =   3990
   ScaleWidth      =   5160
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton cmdNoAll 
      Caption         =   "cmdNoAll"
      Height          =   375
      Left            =   3720
      TabIndex        =   2
      Top             =   3480
      Width           =   1315
   End
   Begin VB.CommandButton cmdNo 
      Caption         =   "cmdNo"
      Height          =   375
      Left            =   2280
      TabIndex        =   1
      Top             =   3480
      Width           =   1315
   End
   Begin VB.CommandButton cmdYes 
      Caption         =   "cmdYes"
      Default         =   -1  'True
      Height          =   375
      Left            =   840
      TabIndex        =   0
      Top             =   3480
      Width           =   1315
   End
   Begin VB.Label lblVersion 
      BackStyle       =   0  'Transparent
      Caption         =   "lblVersion"
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   2280
      Width           =   4935
   End
   Begin VB.Label lblDescription 
      BackStyle       =   0  'Transparent
      Caption         =   "lblDescription"
      Height          =   615
      Left            =   120
      TabIndex        =   6
      Top             =   1560
      Width           =   4935
   End
   Begin VB.Label lblFileName 
      BackStyle       =   0  'Transparent
      Caption         =   "lblFileName"
      Height          =   615
      Left            =   120
      TabIndex        =   5
      Top             =   840
      Width           =   4935
   End
   Begin VB.Label lblCopy 
      BackStyle       =   0  'Transparent
      Caption         =   "lblCopy"
      Height          =   615
      Left            =   120
      TabIndex        =   4
      Top             =   2640
      Width           =   4935
   End
   Begin VB.Label lblTopInfo 
      BackStyle       =   0  'Transparent
      Caption         =   "lblTopInfo"
      Height          =   615
      Left            =   120
      TabIndex        =   3
      Top             =   120
      Width           =   4935
   End
End
Attribute VB_Name = "frmOverwrite"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private msFile As String
Private msDesc As String
Private msVer As String
Private mfFormLoaded As Boolean
Private mfYes As Boolean
Private mfNo As Boolean
Private mfNoToAll As Boolean

Private Sub cmdNo_Click()
    mfNo = True
    Unload Me
End Sub

Private Sub cmdNoAll_Click()
    mfNoToAll = True
    Unload Me
End Sub

Private Sub cmdYes_Click()
    mfYes = True
    Unload Me
End Sub

Private Sub Form_Load()
    mfFormLoaded = True
    SetFormFont Me
    'Load the strings for this form.
    Caption = ResolveResString(resOVERWRITEFORM)
    lblTopInfo.Caption = ResolveResString(resOVERWRITEINFO)
    lblCopy.Caption = ResolveResString(resOVERWRITEKEEP)
    cmdYes.Caption = ResolveResString(resOVERYES)
    cmdNo.Caption = ResolveResString(resOVERNO)
    cmdNoAll.Caption = ResolveResString(resOVERNOTOALL)
    lblFileName.Caption = ResolveResString(resOVERWRITEFILE, gstrPIPE1, msFile)
    lblDescription.Caption = ResolveResString(resOVERWRITEDESC, gstrPIPE1, msDesc)
    lblVersion.Caption = ResolveResString(resOVERWRITEVER, gstrPIPE1, msVer)
    SetMousePtr vbNormal
End Sub

'public access to local vars
Public Property Let FileName(sName As String)
    msFile = sName
    If mfFormLoaded Then lblFileName.Caption = ResolveResString(resOVERWRITEFILE, gstrPIPE1, msFile)
End Property
Public Property Let Description(sDesc As String)
    msDesc = sDesc
    If mfFormLoaded Then lblDescription.Caption = ResolveResString(resOVERWRITEDESC, gstrPIPE1, msDesc)
End Property
Public Property Let Version(sVer As String)
    msVer = sVer
    If mfFormLoaded Then lblVersion.Caption = ResolveResString(resOVERWRITEVER, gstrPIPE1, msVer)
End Property

Private Sub Form_Unload(Cancel As Integer)
    SetMousePtr vbHourglass
End Sub

Public Property Get ReturnVal() As Byte
    'Return 0 for yes
    'return 1 for no
    'return 2 for no to all
    
    If mfYes Then ReturnVal = owYes
    If mfNo Then ReturnVal = owNo
    If mfNoToAll Then ReturnVal = owNoToAll
End Property
