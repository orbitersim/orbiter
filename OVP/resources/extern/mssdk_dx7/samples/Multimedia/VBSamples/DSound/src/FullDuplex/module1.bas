Attribute VB_Name = "basUtils"

Private Sub SetTheSoundBuffer(CDesc As DSCBUFFERDESC)
Dim tempDSD As DSBUFFERDESC, tmpWF As WAVEFORMATEX

    tmpWF = WaveEx(Full.SoundFMT.lSamplesPerSec, Full.SoundFMT.nChannels, Full.SoundFMT.nBitsPerSample)

    tempDSD.lBufferBytes = CDesc.lBufferBytes
    tempDSD.lFlags = DSBCAPS_CTRLPOSITIONNOTIFY Or DSBCAPS_GLOBALFOCUS Or DSBCAPS_GETCURRENTPOSITION2

    Set gDSB = gDS.CreateSoundBuffer(tempDSD, tmpWF)

    gDSB.Play DSBPLAY_LOOPING
    
End Sub

Public Sub InitDirectSound(FormHwnd As Long, GUID As String)


'set up the direct sound for playback
Set gDS = gDX.DirectSoundCreate(GUID)

'set the cooperative level
gDS.SetCooperativeLevel FormHwnd, DSSCL_PRIORITY



End Sub

Public Sub InitDirectSoundCapture()


Dim tempformat As WAVEFORMATEX
Dim Notifications(1) As DSBPOSITIONNOTIFY
Dim CaptureDesc As DSCBUFFERDESC

' set up the direct sound capture for recording
Set gDSC = gDX.DirectSoundCaptureCreate(Full.CaptureGUID)

'''''''''''''''''''''
'Grab the capture
'format
'''''''''''''''''''''
tempformat = WaveEx(Full.CaptureFMT.lSamplesPerSec, Full.CaptureFMT.nChannels, Full.CaptureFMT.nBitsPerSample)
gDSCFW = tempformat
'tempformat = WaveEx(22050, 2, HBITS)

' Set up the capture description for the capture buffer
CaptureDesc.fxFormat = tempformat
CaptureDesc.lReserved = 0
CaptureDesc.lBufferBytes = tempformat.lAvgBytesPerSec
CaptureDesc.lFlags = DSCBCAPS_WAVEMAPPED


'create the capture buffer
Set gDSCB = gDSC.CreateCaptureBuffer(CaptureDesc)
 
Call SetTheSoundBuffer(CaptureDesc)

End Sub

Public Sub record(DSCB As DirectSoundCaptureBuffer)
On Error GoTo err_out

    If DSCB Is Nothing Then
        Call InitDirectSoundCapture
    End If
    
    'start the recording loop
    DSCB.start DSCBSTART_LOOPING
Exit Sub

err_out:
    MsgBox Err.Description, vbApplicationModal
    End
End Sub


Private Function WaveEx(Hz As Long, Channels As Integer, BITS As Integer) As WAVEFORMATEX

    WaveEx.nFormatTag = WAVE_FORMAT_PCM
    WaveEx.nChannels = Channels
    WaveEx.lSamplesPerSec = Hz
    WaveEx.nBitsPerSample = BITS
    WaveEx.nBlockAlign = Channels * BITS / 8
    WaveEx.lAvgBytesPerSec = WaveEx.lSamplesPerSec * WaveEx.nBlockAlign
    WaveEx.nSize = 0

End Function
Public Sub CopyBuffers(DSOUNDB As DirectSoundBuffer, DSCB As DirectSoundCaptureBuffer, Effect As Integer)
    Dim CURS As DSCURSORS, cnt As Double, tmpB() As Integer
    Dim tCnt As Integer
    
    DSCB.GetCurrentPosition CURS
    
    ReDim gBuffer(CURS.lWrite + 1)
    
    Select Case Effect
    Case 0
        DSCB.ReadBuffer 0, CURS.lWrite + 1, gBuffer(0), DSCBLOCK_DEFAULT
        
        DSOUNDB.WriteBuffer 0, CURS.lWrite + 1, gBuffer(0), DSBLOCK_DEFAULT
        Erase gBuffer
    Case 1
        ReDim tmpB(CURS.lWrite + 1)
        DSCB.ReadBuffer 0, CURS.lWrite + 1, gBuffer(0), DSCBLOCK_DEFAULT
        

'        For cnt = 0 To UBound(gBuffer)
'        '''''''''''''''''''''''
'        'Do the effect
'        '''''''''''''''''''''''
'            i = i + 1: If i > 5 Then i = 1
'            If gBuffer(cnt) <> 0 Then tmpB(cnt) = Sin(cnt / i) * i / (gBuffer(cnt))
'            gBuffer(cnt) = tmpB(cnt)
'
'        Next
'
'        For tCnt = CURS.lWrite + 2 To UBound(gBuffer)
'            gBuffer(tCnt) = 0
'        Next
        
        
        DSOUNDB.WriteBuffer 0, CURS.lWrite + 1, tmpB(0), DSBLOCK_DEFAULT
        Erase gBuffer: Erase tmpB
        
    End Select
    
End Sub


Public Sub StopAll(DSB As DirectSoundBuffer, DSCB As DirectSoundCaptureBuffer)
    On Error Resume Next
    DSB.Stop
    DSCB.Stop
    running = False
End Sub
    
Public Sub PlayBack(DSB As DirectSoundBuffer)
    DSB.Play DSBPLAY_LOOPING
End Sub



