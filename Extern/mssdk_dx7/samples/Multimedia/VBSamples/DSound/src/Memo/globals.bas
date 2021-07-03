Attribute VB_Name = "Globals"
Public dx As New DirectX7

'dsound
Public ds As DirectSound
Public dsb As DirectSoundBuffer
Public dsb2 As DirectSoundBuffer
Public DSBD As DSBUFFERDESC
Public w As WAVEFORMATEX
Public hlDBUFF() As Integer

'dscapture
Public dsc As DirectSoundCapture
Public dscb As DirectSoundCaptureBuffer
Public dc As DSCBUFFERDESC
Public wc As WAVEFORMATEX

Public HLDFROMDB As String
Public hold() As Integer
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (destination As Any, source As Any, ByVal Length As Long)
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)


Public D As Integer
Public M As String
Public Y As Integer

Global MEMO_RECORD As Boolean



