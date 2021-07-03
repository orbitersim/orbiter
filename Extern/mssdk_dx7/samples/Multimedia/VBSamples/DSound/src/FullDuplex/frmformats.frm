VERSION 5.00
Begin VB.Form frmFormats 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Select Formats"
   ClientHeight    =   2925
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6330
   Icon            =   "frmFormats.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2925
   ScaleWidth      =   6330
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3480
      TabIndex        =   6
      Top             =   2280
      Width           =   1215
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   375
      Left            =   1560
      TabIndex        =   5
      Top             =   2280
      Width           =   1335
   End
   Begin VB.ListBox lstInput 
      Height          =   1425
      Left            =   3960
      TabIndex        =   1
      Top             =   600
      Width           =   2175
   End
   Begin VB.ListBox lstOutput 
      Height          =   1425
      Left            =   240
      TabIndex        =   0
      Top             =   600
      Width           =   2175
   End
   Begin VB.Label lblToDO 
      Caption         =   "Choose an output format, then select from the available input formats.  Click OK to accept the combination."
      Height          =   1335
      Left            =   2520
      TabIndex        =   4
      Top             =   600
      Width           =   1335
   End
   Begin VB.Label lblInputFormat 
      Caption         =   "Input Format:"
      Height          =   375
      Left            =   3960
      TabIndex        =   3
      Top             =   240
      Width           =   2175
   End
   Begin VB.Label lblOutPut 
      Caption         =   "Output Format:"
      Height          =   255
      Left            =   240
      TabIndex        =   2
      Top             =   240
      Width           =   2175
   End
End
Attribute VB_Name = "frmFormats"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private DX As New DirectX7
Private DS As DirectSound
Private DSC As DirectSoundCapture
Private DSCD As DSCBUFFERDESC
Private sCaps As DSCAPS
Private cCaps As DSCCAPS
Private dsb_ As DirectSoundBuffer
Private dsbd_ As DSBUFFERDESC
Private tWFX As WAVEFORMATEX
Sub INIT_SND()
    
    On Local Error GoTo errOut
    Set DSC = DX.DirectSoundCaptureCreate(Full.CaptureGUID)
    DSC.GetCaps cCaps
    
    Set DS = DX.DirectSoundCreate(Full.SoundGUID)
    DS.SetCooperativeLevel hWnd, DSSCL_PRIORITY
    DS.GetCaps sCaps
    Exit Sub
    
errOut:
    MsgBox "Could not initialize this device", vbOKOnly Or vbCritical, "Could not init"
    End
End Sub

Private Sub Term()
    Set DX = Nothing
    Set DS = Nothing
    Set DSC = Nothing
    Set dsb_ = Nothing
    Set gDSCB = Nothing
    Set gDSB = Nothing
End Sub

Private Sub cmdCancel_Click()
    End
End Sub

Private Sub cmdOK_Click()
    Unload Me
    frmMain.Show
End Sub

Private Sub form_load()
    GetFormatInfo True
    INIT_SND
        
    ScanAvailableDSFormats
    
    Dim i As Integer
    
    
    For i = 0 To UBound(aOutputFormats)
        If aOutputFormats(i).bEnabled = True Then
            lstOutput.AddItem aOutputFormats(i).sDesc
        End If
    Next
    
    cmdOK.Enabled = False
    
End Sub


Sub ScanAvailableDSFormats()
    On Error Resume Next
    
    Dim dsrval As Long
    Dim i As Integer

    For i = 0 To NUM_FORMATCODES - 1
        aOutputFormats(i).bEnabled = False
    Next
    
    ' This might take a second or two, so throw up the hourglass
    Screen.MousePointer = vbHourglass


    

    For i = 0 To NUM_FORMATCODES - 1
        Dim wfx As WAVEFORMATEX
        wfx.nFormatTag = WAVE_FORMAT_PCM
        FormatCodeToWFX aOutputFormats(i).lCode, wfx
    
        Dim HLD As Long
        
        
        Set dsb_ = Nothing
        
        With dsbd_
            .lFlags = DSBCAPS_PRIMARYBUFFER
            .lReserved = 0
        End With
        
        Set dsb_ = DS.CreateSoundBuffer(dsbd_, tWFX)
        
        
        
        
        dsb_.SetFormat wfx
        
        
        Dim kwEmpty As WAVEFORMATEX
        Dim kW As WAVEFORMATEX
        kW = kwEmpty
        dsb_.GetFormat kW
        
        Debug.Print kW.lSamplesPerSec
        
        
        If kW.lAvgBytesPerSec = wfx.lAvgBytesPerSec Then
            If kW.lSamplesPerSec = wfx.lSamplesPerSec Then
                If kW.nChannels = wfx.nChannels Then
                    aOutputFormats(i).bEnabled = True
                End If
            End If
        Else
            aOutputFormats(i).bEnabled = False
        End If
    
        'Set wfx = Nothing
        
        
    Next
    
    Screen.MousePointer = vbNormal
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Term
End Sub

Private Sub lstInput_Click()
    Dim FMT As WAVEFORMATEX, lCode As Long, cnt As Integer
    Dim InputCnt As Integer
    
    While (cnt <> UBound(aInputFormats) + 1)
        If aInputFormats(cnt).sDesc = lstInput.Text Then
            FormatCodeToWFX aInputFormats(cnt).lCode, FMT
        End If
        cnt = cnt + 1
    Wend
        
    Full.CaptureFMT = FMT
    Full.CaptureFormatDesc = lstInput.Text
    
    cmdOK.Enabled = True
End Sub

Private Sub lstOutput_Click()
    Dim FMT As WAVEFORMATEX, lCode As Long, cnt As Integer
    Dim InputCnt As Integer
    
    lstInput.Clear
    
    While (cnt <> UBound(aOutputFormats) + 1)
        If aOutputFormats(cnt).sDesc = lstOutput.Text Then
            FormatCodeToWFX aOutputFormats(cnt).lCode, FMT
        End If
        cnt = cnt + 1
    Wend
        
    
    Full.SoundFMT = FMT
    Full.SoundFormatDesc = lstOutput.Text

    GetFormatInfo False
    ScanAvailableDSCFormats

    While (InputCnt <> UBound(aInputFormats) + 1)
        If aInputFormats(InputCnt).bEnabled = True Then
            lstInput.AddItem aInputFormats(InputCnt).sDesc
        End If
        InputCnt = InputCnt + 1
    Wend

End Sub

Sub ScanAvailableDSCFormats()
    On Error Resume Next
    
    Dim dsrval As Long
    Dim i As Integer

    For i = 0 To NUM_FORMATCODES - 1
        aInputFormats(i).bEnabled = False
    Next
    
    ' This might take a second or two, so throw up the hourglass
    Screen.MousePointer = vbHourglass
    

    For i = 0 To NUM_FORMATCODES - 1
        Dim wfx As WAVEFORMATEX
        wfx.nFormatTag = WAVE_FORMAT_PCM
        FormatCodeToWFX aInputFormats(i).lCode, wfx
    
        Dim HLD As Long
        
        Set gDSCB = Nothing
        
        
        With DSCD
            .lFlags = DSCBCAPS_WAVEMAPPED
            .fxFormat = wfx
            .lBufferBytes = wfx.lAvgBytesPerSec
        End With
        
        Set gDSCB = DSC.CreateCaptureBuffer(DSCD)
        
        Debug.Print Err.Number
        
        Dim fw As WAVEFORMATEX
        Dim fwEmpty As WAVEFORMATEX
        fw = fwEmpty
        
        gDSCB.GetFormat fw
        
        If fw.lAvgBytesPerSec = wfx.lAvgBytesPerSec Then
            If fw.lSamplesPerSec = wfx.lSamplesPerSec Then
                If fw.nChannels = wfx.nChannels Then
                    aInputFormats(i).bEnabled = True
                End If
            End If
        Else
            aInputFormats(i).bEnabled = False
        End If
    
        
    Next
    
    Screen.MousePointer = vbNormal
End Sub
