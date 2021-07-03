Attribute VB_Name = "basStream"
''----------------------------------------
''Variables
''----------------------------------------

Global gDX As New DirectX7
Global gDSC As DirectSoundCapture
Global gDSCB As DirectSoundCaptureBuffer
Global gDSCBD As DSCBUFFERDESC
Public EventID(1) As Long
Public EVNT(1) As DSBPOSITIONNOTIFY
Public MEM() As Byte
Global f%
Public WaveF1 As WAVEFORMATEX
Public Buffer() As Byte
Public cur As DSCURSORS


''----------------------------------------
''Wave header info
''----------------------------------------
Private Type FileHeader
    dwRiff As Long
    dwFileSize As Long
    dwWave As Long
    dwFormat As Long
    dwFormatLength As Long
    wFormatTag As Integer
    nChannels As Integer
    nSamplesPerSec As Long
    nAvgBytesPerSec As Long
    nBlockAlign As Integer
    wBitsPerSample As Integer
    dwData As Long
    dwDataLength As Long
End Type


''-------------------------------------
''init the capture buffer
''-------------------------------------
Public Sub Init(hWnd As Long)
    
    On Local Error GoTo ErrOut
    With WaveF1
        .nChannels = 1
        .nFormatTag = WAVE_FORMAT_PCM
        .lSamplesPerSec = 22050
        .lAvgBytesPerSec = 22050
        .nBlockAlign = 1
        .nBitsPerSample = 8
    End With
    
    'capture buffer
    Set gDSC = gDX.DirectSoundCaptureCreate(vbNullString)
    
    With gDSCBD
        .lFlags = DSCBCAPS_WAVEMAPPED
        .lBufferBytes = WaveF1.lAvgBytesPerSec * 2
        .fxFormat = WaveF1
    End With
    
    Set gDSCB = gDSC.CreateCaptureBuffer(gDSCBD)
    
    ''---------------------------------
    ''Send the info to the file
    ''---------------------------------
    Call StreamToTempFile
    Exit Sub

ErrOut:
    MsgBox "Could not create the default sound device", vbOKOnly Or vbCritical, "Cannot create"
    End
        
End Sub

''--------------------------------------
''Close the open files
''--------------------------------------
Public Sub CloseFiles()
    On Local Error Resume Next
    Close #fFile_1
    Close #fFile_2
    Close #f
End Sub

''--------------------------------------
''Delete the temp file
''--------------------------------------
Public Sub KillTempFile()
    On Local Error Resume Next
    Close #f
    Kill App.Path + "\tmp.tmp"
End Sub


''--------------------------------------
''Open a temp file for output
''--------------------------------------
Public Sub StreamToTempFile()
    'open a temp file for streaming input
    f% = FreeFile
    Open App.Path + "\tmp.tmp" For Binary Access Write As #f
End Sub


''--------------------------------------
''send the buffer data to a file
''--------------------------------------
Public Sub CopyBuffer(part As Integer)
    Static sz As Double
    
    gDSCB.GetCurrentPosition cur
    
    ReDim Buffer(cur.lWrite - 1)
    
    gDSCB.ReadBuffer 0, cur.lWrite, Buffer(0), DSCBLOCK_DEFAULT
    
    
    Put #f, , Buffer
    
    Erase Buffer
    
    sz = sz + cur.lWrite
    frmMain.lblSize = Str$(sz) ' send size info to the form label

End Sub



''--------------------------------------------------
''Save the whole thing to a file
''--------------------------------------------------

Public Sub SaveToFileAsStream(sName As String)
    
    Dim fh As FileHeader, Status As Long
    Dim WF As WAVEFORMATEX
    Dim fFile_1%, fFile_2%, File_1Holder() As Byte
    
    Status = gDSCB.GetStatus

    If (Status And DSCBSTATUS_CAPTURING) Then gDSCB.Stop
    
    'get the wave data from the tmp file
    fFile_1% = FreeFile
    
    Open App.Path + "\tmp.tmp" For Binary Access Read As #fFile_1
    ReDim File_1Holder(LOF(fFile_1%) + 1)
    
    Get #fFile_1, , File_1Holder
    
    Close #fFile_1
    
    
    WF = WaveF1
        
    
    fFile_2% = FreeFile
    
    Open App.Path + "\" + sName For Binary Access Write As #fFile_2
    
    fh.dwRiff = &H46464952            '                // RIFF
    fh.dwWave = &H45564157            '                // WAVE
    fh.dwFormat = &H20746D66          '                // fmt_chnk
    fh.dwFormatLength = 16
    fh.wFormatTag = WAVE_FORMAT_PCM
    fh.nChannels = WF.nChannels
    fh.nSamplesPerSec = WF.lSamplesPerSec
    fh.wBitsPerSample = WF.nBitsPerSample
    fh.nBlockAlign = fh.wBitsPerSample / 8 * fh.nChannels
    fh.nAvgBytesPerSec = fh.nSamplesPerSec * fh.nBlockAlign
    fh.dwData = &H61746164            '                 // data_chnk
    fh.dwDataLength = UBound(File_1Holder)
    fh.dwFileSize = UBound(File_1Holder) + Len(fh)
    
    'add the info to the second file
    Put #fFile_2, , fh
    Put #fFile_2, , File_1Holder
    
    Close #fFile_2
    
    Call KillTempFile
    
End Sub

