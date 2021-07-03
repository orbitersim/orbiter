Attribute VB_Name = "basHelper"
Public Type FD
    CaptureGUID As String
    SoundGUID As String
    SoundDesc As String
    CaptureDesc As String
    SoundFormatDesc As String
    CaptureFormatDesc As String
    CaptureFMT As WAVEFORMATEX
    SoundFMT As WAVEFORMATEX
End Type

Global Full As FD

Public Const NUM_FORMATCODES = 16

Public Type Formats
    lCode As Long
    bEnabled  As Boolean
    sDesc As String
End Type


Public aOutputFormats(NUM_FORMATCODES - 1) As Formats
Public aInputFormats(NUM_FORMATCODES - 1) As Formats
Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)





'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'FormatCodeToWFX()
'
' This function reads format codes and fills most of the fields of a
' WAVEFORMATEX structure based on the values read.  It does not fill the
' wFormatTag or cbSize members.
'


Public Function FormatCodeToWFX(lFormat As Long, wf As WAVEFORMATEX) As Boolean


    Dim lFreq As Long

'    If wf Is Nothing Then
'        FormatCodeToWFX = False
'        Exit Function
'    End If
'

    ' Extract the sample rate
    lFreq = FC_GETFREQCODE(lFormat)
    
    If lFreq = 8 Then
        wf.lSamplesPerSec = 8000
    Else
        wf.lSamplesPerSec = (lFreq / 11) * 11025
    End If
    
    wf.nBitsPerSample = FC_GETBITS(lFormat)
    wf.nChannels = FC_GETCHANNELS(lFormat)

    ' The nBlockAlign calculation below only works for whole-byte samples
     If wf.nBitsPerSample Mod 8 = 0 Then
        wf.nBlockAlign = wf.nChannels * (wf.nBitsPerSample / 8)
        wf.lAvgBytesPerSec = wf.nBlockAlign * wf.lSamplesPerSec
    End If
    
    FormatCodeToWFX = True

End Function

Private Function FC_GETFREQCODE(fc) As Long
    FC_GETFREQCODE = ((fc) / 1000)
End Function


Private Function FC_GETBITS(fc) As Long
    FC_GETBITS = ((fc) Mod 100)
End Function

Private Function FC_GETCHANNELS(fc) As Long
    FC_GETCHANNELS = (((fc) Mod 1000) / 100)
End Function


Public Sub GetFormatInfo(InputOutPut As Boolean)

Dim ConstForm(NUM_FORMATCODES - 1) As Formats
Dim cnt As Integer

ConstForm(0).bEnabled = True
ConstForm(0).lCode = 8108
ConstForm(0).sDesc = "8000 Hz 8-bit Mono"

ConstForm(1).bEnabled = True
ConstForm(1).lCode = 8208
ConstForm(1).sDesc = "8000 Hz 8-bit Stereo"

ConstForm(2).bEnabled = True
ConstForm(2).lCode = 8116
ConstForm(2).sDesc = "8000 Hz 16-bit Mono"

ConstForm(3).bEnabled = True
ConstForm(3).lCode = 8216
ConstForm(3).sDesc = "8000 Hz 16-bit Stereo"

ConstForm(4).bEnabled = True
ConstForm(4).lCode = 11108
ConstForm(4).sDesc = "11025 Hz 8-bit Mono"

ConstForm(5).bEnabled = True
ConstForm(5).lCode = 11208
ConstForm(5).sDesc = "11025 Hz 8-bit Stereo"

ConstForm(6).bEnabled = True
ConstForm(6).lCode = 11116
ConstForm(6).sDesc = "11025 Hz 16-bit Mono"

ConstForm(7).bEnabled = True
ConstForm(7).lCode = 11216
ConstForm(7).sDesc = "11025 Hz 16-bit Stereo"

ConstForm(8).bEnabled = True
ConstForm(8).lCode = 22108
ConstForm(8).sDesc = "22050 Hz 8-bit Mono"

ConstForm(9).bEnabled = True
ConstForm(9).lCode = 22208
ConstForm(9).sDesc = "22050 Hz 8-bit Stereo"

ConstForm(10).bEnabled = True
ConstForm(10).lCode = 22116
ConstForm(10).sDesc = "22050 Hz 16-bit Mono"

ConstForm(11).bEnabled = True
ConstForm(11).lCode = 22216
ConstForm(11).sDesc = "22050 Hz 16-bit Stereo"

ConstForm(12).bEnabled = True
ConstForm(12).lCode = 44108
ConstForm(12).sDesc = "44100 Hz 8-bit Mono"

ConstForm(13).bEnabled = True
ConstForm(13).lCode = 44208
ConstForm(13).sDesc = "44100 Hz 8-bit Stereo"

ConstForm(14).bEnabled = True
ConstForm(14).lCode = 44116
ConstForm(14).sDesc = "44100 Hz 16-bit Mono"

ConstForm(15).bEnabled = True
ConstForm(15).lCode = 44216
ConstForm(15).sDesc = "44100 Hz 16-bit Stereo"

Select Case InputOutPut
    Case True
        While (cnt <> UBound(ConstForm) + 1)
            aOutputFormats(cnt).bEnabled = ConstForm(cnt).bEnabled
            aOutputFormats(cnt).lCode = ConstForm(cnt).lCode
            aOutputFormats(cnt).sDesc = ConstForm(cnt).sDesc
            cnt = cnt + 1
        Wend
    Case False
        While (cnt <> UBound(ConstForm) + 1)
            aInputFormats(cnt).bEnabled = ConstForm(cnt).bEnabled
            aInputFormats(cnt).lCode = ConstForm(cnt).lCode
            aInputFormats(cnt).sDesc = ConstForm(cnt).sDesc
            cnt = cnt + 1
        Wend
End Select
End Sub

