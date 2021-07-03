VERSION 5.00
Begin VB.Form frmSelectRes 
   ClientHeight    =   4215
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   Icon            =   "frmSelectRes.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   4215
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.ComboBox cmbResolution 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   7
      Top             =   1320
      Width           =   4455
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Cancel"
      Height          =   495
      Left            =   2400
      TabIndex        =   6
      Top             =   3480
      Width           =   975
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   495
      Left            =   3480
      TabIndex        =   5
      Top             =   3480
      Width           =   975
   End
   Begin VB.CheckBox chFPS 
      Caption         =   " Show Frames Per Second"
      Height          =   195
      Left            =   120
      TabIndex        =   4
      Top             =   2760
      Width           =   3015
   End
   Begin VB.ComboBox cmbRasterizer 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   2160
      Width           =   4455
   End
   Begin VB.ComboBox cmbHardware 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   480
      Width           =   4455
   End
   Begin VB.Label Label3 
      Caption         =   "FullScreen Resolution"
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   960
      Width           =   2415
   End
   Begin VB.Label Label2 
      Caption         =   "Rasterizers"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   1800
      Width           =   2895
   End
   Begin VB.Label Label1 
      Caption         =   "Display Hardware"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2415
   End
End
Attribute VB_Name = "frmSelectRes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim dx As New DirectX7


Dim m_dden As DirectDrawEnum
Dim m_modes  As DirectDrawEnumModes
Dim m_rast As Direct3DEnumDevices

Dim m_sDDrawGuid As String
Dim m_sRasterizerGuid As String
Dim m_w As Long
Dim m_h As Long
Dim m_bpp As Long
Dim m_bShowFps As Boolean
Dim m_bFullscreen As Boolean
Dim m_bCommit As Boolean

Public Function ChangeConfig(ByRef sDDrawGuid As String, ByRef sRasterizerGuid As String, ByRef w As Long, ByRef h As Long, ByRef bpp As Long, ByRef bFullscreen As Boolean, ByRef bShowfps As Boolean) As Boolean
    
    
    m_bFullscreen = bFullscreen
    m_bShowFps = bShowfps
    m_bpp = bpp
    m_w = w
    m_h = h
    m_sDDrawGuid = sDDrawGuid
    m_sRasterizerGuid = sRasterizerGuid
        
    FillDDrawEnum
    
    Dim i As Long
    
    'Find the DDraw Guid in the combo list
    For i = 1 To m_dden.GetCount()
        If UCase(m_dden.GetGuid(i)) = UCase(sDDrawGuid) Then
            cmbHardware.ListIndex = i - 1
            Exit For
        End If
    Next
    
    'if we did not find one- select the first one in the list
    If i = m_dden.GetCount + 1 Then cmbHardware.ListIndex = 0
    
    'Find rasterizer in the combo list
    For i = 1 To m_rast.GetCount()
        If UCase(m_rast.GetGuid(i)) = UCase(sRasterizerGuid) Then
            cmbRasterizer.ListIndex = i - 1
            Exit For
        End If
    Next
    
    'if we did not find one- select the first one in the list
    If i = m_rast.GetCount + 1 Then cmbRasterizer.ListIndex = 0
    
    
    'Find the resolution
    Dim info As DDSURFACEDESC2
    If Not bFullscreen Then
        cmbResolution.ListIndex = 0
    Else
        
        For i = 1 To m_modes.GetCount()
            m_modes.GetItem i, info
            If info.ddpfPixelFormat.lRGBBitCount = bpp And _
                info.lWidth = w And _
                info.lHeight = h Then
                cmbResolution.ListIndex = i
                Exit For
            End If
        Next
                
    End If
    
    Me.Show 1
    
    If m_bCommit Then



        If cmbResolution.ListIndex = 0 Then
            bFullscreen = False
        Else
        
            m_modes.GetItem cmbResolution.ListIndex, info
        
            If (info.ddpfPixelFormat.lRGBBitCount <= 8) Then
                MsgBox "D3DIM application must run in 16 bpp color or better"
                Exit Function
            End If
        
            bFullscreen = True
               
        End If


        bpp = info.ddpfPixelFormat.lRGBBitCount
        w = info.lWidth
        h = info.lHeight

           
        bShowfps = chFPS.Value
        
    
                                
        sDDrawGuid = m_dden.GetGuid(cmbHardware.ListIndex + 1)
        sRasterizerGuid = m_rast.GetGuid(cmbRasterizer.ListIndex + 1)
        ChangeConfig = True
    End If
    
    Set m_dden = Nothing
    Set m_modes = Nothing
    Set m_rast = Nothing

End Function


Private Sub cmbHardware_Click()
    SelectDDraw cmbHardware.ListIndex
End Sub


Sub FillDDrawEnum()
    Dim i As Long
    Set m_dden = dx.GetDDEnum()
   
    cmbHardware.Clear
    For i = 1 To m_dden.GetCount()
        cmbHardware.AddItem m_dden.GetDescription(i)
    Next
    SelectDDraw 0
End Sub

Sub SelectDDraw(i As Integer)
    Dim dd As DirectDraw7
    Dim desc As DDSURFACEDESC2
    Dim j As Long
    
    Set dd = dx.DirectDrawCreate(m_dden.GetGuid(i + 1))
    
    Set m_modes = dd.GetDisplayModesEnum(DDEDM_DEFAULT, desc)
    
    cmbResolution.Clear
    cmbResolution.AddItem "Window Size with Desktop Resolution"
    For j = 1 To m_modes.GetCount()
        m_modes.GetItem j, desc
        cmbResolution.AddItem Str(desc.lWidth) + " x" + Str(desc.lHeight) + " x" + Str(desc.ddpfPixelFormat.lRGBBitCount)
    Next
    
    Dim d3d As Direct3D7
    Set d3d = dd.GetDirect3D()
    
    cmbRasterizer.Clear
    Set m_rast = d3d.GetDevicesEnum()
    For j = 1 To m_rast.GetCount()
        cmbRasterizer.AddItem m_rast.GetDescription(j)
    Next
    
    cmbResolution.ListIndex = 0
    
    cmbRasterizer.ListIndex = 0
    
End Sub

Private Sub Command1_Click()
    m_bCommit = True
    Me.Visible = False
End Sub

Private Sub Command2_Click()
    m_bCommit = False
    Me.Visible = False

End Sub


