Attribute VB_Name = "DSOUND"
Public Sub initSound(hw As Long)
    Set ds = dx.DirectSoundCreate("")
    ds.SetCooperativeLevel hw, DSSCL_NORMAL
End Sub


Public Sub record_memo(hw As Long)
    On Error Resume Next
    Dim ST As Long
    
    If ds Is Nothing Then initSound hw
    Set dscb = Nothing
    
    ST = dscb.GetStatus
    
    If Err.Number = 91 Then Resume Next
    
    If (ST And DSCBSTATUS_CAPTURING) Then
        MsgBox "Already Capturing......", vbApplicationModal
        Exit Sub
    End If
    
    Set dsc = dx.DirectSoundCaptureCreate("")
    
    wc = MakeWaveEX(11025, 1, 8)
    dc.fxFormat = wc
    dc.lBufferBytes = wc.lAvgBytesPerSec * 30
    dc.lFlags = DSCBCAPS_WAVEMAPPED
    Set dscb = dsc.CreateCaptureBuffer(dc)
    
    dscb.start DSCBSTART_DEFAULT
    MEMO_RECORD = True
End Sub


Public Sub Play_Sound()
    Dim cur As DSCURSORS
    Dim dsBPR As DirectSoundBuffer
    Dim PD As DSBUFFERDESC
    Dim wD As WAVEFORMATEX
    Dim ST As Long
    Dim WV() As Byte
    
    If Not MEMO_RECORD Then Exit Sub
    
    ST = dscb.GetStatus
    If (ST And DSCBSTATUS_CAPTURING) Then
        dscb.Stop
    End If
    
    dscb.GetCurrentPosition cur
    
    PD.lFlags = DSBCAPS_CTRLVOLUME Or DSBCAPS_STATIC
    wD = wc
    PD.lBufferBytes = cur.lWrite + 1 '* wD.nBlockAlign
    
    ReDim WV(cur.lWrite + 1) ' * wD.nBlockAlign + 1)
    
    dscb.ReadBuffer 0, cur.lWrite + 1, WV(0), DSCBLOCK_DEFAULT
    
    
    
    Set dsb = ds.CreateSoundBuffer(PD, wD)
    
    dsb.WriteBuffer 0, cur.lWrite + 1, WV(0), DSBLOCK_DEFAULT
    
    dsb.SetCurrentPosition 0
    dsb.Play DSBPLAY_DEFAULT
    
    'Set dscb = Nothing
End Sub

Public Sub Play_Sound_From_db(nm As String, hw As Long)
    
    Dim SZ As Long
    Dim WAVE_DATA() As Byte
    Dim bFound As Boolean
    
    If ds Is Nothing Then Call initSound(hw)
    
    If rs.RecordCount = 0 Then Exit Sub
    rs.MoveFirst
    
    
    
    Do While Not rs.EOF
        With rs
            If !Name = nm Then
                SZ = !wl
                ReDim WAVE_DATA(SZ)
                WAVE_DATA() = !wav
                bFound = True
                Exit Do
            End If
            .MoveNext
        End With
        bFound = False
    Loop
        

    If bFound Then
        Set dsb2 = Nothing
        
        DSBD.lBufferBytes = SZ
        DSBD.lFlags = DSBCAPS_CTRLVOLUME Or DSBCAPS_STATIC
        
        w = MakeWaveEX(11025, 1, 8)
        
        
        Set dsb2 = ds.CreateSoundBuffer(DSBD, w)
        
        dsb2.WriteBuffer 0, 0, WAVE_DATA(0), DSBLOCK_ENTIREBUFFER
        
        dsb2.Play DSBPLAY_DEFAULT
    Else
        MsgBox "File not found in database", vbApplicationModal
        Exit Sub
    End If

End Sub
Public Sub CLEANUPSND()
    Set dsb = Nothing
    Set ds = Nothing
    Set dsb2 = Nothing
    Set dsc = Nothing
End Sub
Private Function MakeWaveEX(Hz As Long, Channels As Integer, Bits As Integer) As WAVEFORMATEX
    MakeWaveEX.lSamplesPerSec = Hz
    MakeWaveEX.lExtra = 0
    MakeWaveEX.nSize = 0
    MakeWaveEX.nBitsPerSample = Bits
    MakeWaveEX.nChannels = Channels
    MakeWaveEX.nFormatTag = WAVE_FORMAT_PCM
    MakeWaveEX.nBlockAlign = Channels * Bits \ 8
    MakeWaveEX.lAvgBytesPerSec = Hz * (Channels * Bits \ 8)
End Function
