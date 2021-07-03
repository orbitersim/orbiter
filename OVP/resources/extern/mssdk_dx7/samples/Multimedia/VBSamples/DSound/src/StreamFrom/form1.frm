VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Form1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Stream From File"
   ClientHeight    =   2430
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   2430
   Icon            =   "form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2430
   ScaleWidth      =   2430
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdStop 
      Caption         =   "Stop"
      Enabled         =   0   'False
      Height          =   420
      Left            =   600
      TabIndex        =   2
      Top             =   1740
      Width           =   1185
   End
   Begin MSComDlg.CommonDialog cdlgLoad 
      Left            =   1920
      Top             =   1200
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton cmdPlay 
      Caption         =   "Play File"
      Enabled         =   0   'False
      Height          =   420
      Left            =   600
      TabIndex        =   1
      Top             =   1200
      Width           =   1185
   End
   Begin VB.CommandButton cmdLoad 
      Caption         =   "Load File"
      Height          =   420
      Left            =   600
      TabIndex        =   0
      Top             =   660
      Width           =   1185
   End
   Begin VB.Label lblTitle 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFC0&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "None"
      ForeColor       =   &H00800000&
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   240
      Width           =   2175
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'''''''''''''''''''''''''''''''''''''''''''''
'This sample shows how to stream from a file.
'''''''''''''''''''''''''''''''''''''''''''''

'Initialize variables, constants, and user-defined types.

Implements DirectXEvent                         'This enables the form to receive events from DirectX.
Const NUM_POSITIONS = 16                        'Used for making 16 (0 based) notification positions.
Private Declare Sub RtlZeroMemory Lib "kernel32" (dest As Any, ByVal cbSize As Long)

Private Type FileHeader                         'File header structure for wave files.
    dwRiff As Long
    dwFileSize As Long
    dwWave As Long
    dwFormat As Long
    dwFormatLength As Long
End Type

Private Type FileFormat                         'File format structure for wave files
        wFormatTag As Integer
        nChannels As Integer
        nSamplesPerSec As Long
        nAvgBytesPerSec As Long
        nBlockAlign As Integer
        wBitsPerSample As Integer
End Type
     
Private Type HeaderChunk                        'Header chunk format for wave files
    dwType As Long
    dwLen As Long
End Type

Dim dx As New DirectX7                          'DirectX object.
Dim ds As DirectSound                           'Direct Sound object.
Dim dsb As DirectSoundBuffer                    'Direct sound buffer object.
Dim dsbPrimary As DirectSoundBuffer             'Primary direct sound buffer object.
Dim dsbd As DSBUFFERDESC                        'Direct sound buffer description.
Dim Format As WAVEFORMATEX                      'Wave format EX structure.
Dim Header As FileHeader                        'Wave file header variable.
Dim HdrFormat  As FileFormat
Dim hEvent(1) As Long                           'Array to hold the event handle.
Dim psa(1) As DSBPOSITIONNOTIFY                 'Notify position array.
Dim FileFree As Long                            'Holds the handle to the file.
Dim Buffer() As Byte                            'Dynamic byte array for the wave data buffer.
Dim lngNotificationSize As Long
Dim lngLastBit As Long
Dim fEnd As Long
Dim dwDataLength As Long
Dim m_bLoop As Boolean

Private Sub cmdPlay_Click()

    'This is where the buffers are initialized for playback.
    
    If CreateStreamingBuffer Then               'Call the function that creates the streaming buffer. If it succeeds, continue.
        fEnd = 0
        dsb.SetCurrentPosition 0
        If m_bLoop Then
            dsb.Play DSBPLAY_LOOPING                'Start the secondary buffer, and keep it looping as well.
            cmdLoad.Enabled = False                 'Disable the load button during playback.
            cmdPlay.Enabled = False                 'Disable the play button during playback.
            cmdStop.Enabled = True                  'Enable the stop button.
        Else
            dsb.Play DSBPLAY_DEFAULT                'Start the secondary buffer
        End If
    End If
    
End Sub

Private Sub cmdStop_Click()
        
    dsb.Stop                                    'Stop the direct sound buffer.
    cmdLoad.Enabled = True                      'Enable the load button.
    cmdStop.Enabled = False                     'Disable the stop button.
    cmdPlay.Enabled = True                      'Enable the play button.
    
End Sub

Private Sub Form_Load()
    
    'Sets up the primary buffer & DX events.
    
    On Local Error GoTo ErrOut
    Dim dsbdPrimary As DSBUFFERDESC             'Used to initialize the primary buffer.
    Dim WavFormat As WAVEFORMATEX               'Also used to init the primary buffer.
        
    Me.Show                                     'Make sure that the loading of the form is complete.
    
    cmdLoad.Enabled = True                      'Enable the load button.
    cmdPlay.Enabled = False                     'Disable the play button.
    cmdStop.Enabled = False                     'Disable the stop button.
    
    hEvent(0) = dx.CreateEvent(Me)              'Create an event handle, and attach it to this form.
    hEvent(1) = dx.CreateEvent(Me)              'Create an event handle, and attach it to this form.
    
    Set ds = dx.DirectSoundCreate(vbNullString) 'Create the direct sound object using the default driver.
    ds.SetCooperativeLevel Me.hWnd, DSSCL_PRIORITY
                                                'Set the cooperative level to the forms window handle.
                                                'Create the primary buffer.
    Exit Sub
ErrOut:
    MsgBox "Cannot create the primary sound device.  Exiting this application.", vbOKOnly Or vbCritical, "Cannot create"
    End
    
End Sub

Private Sub cmdLoad_Click()
    
    'This begins the loading process for the wave file to be played back.
    
    On Local Error GoTo ErrorHandler            'Make sure to handle if cancel is pressed.
    With cdlgLoad                               'Set the flags for the common dialog box.
        .CancelError = True                     'Make sure canel will be detected if it is clicked.
        .Filter = "(*.WAV)|*.WAV"               'Set the filters for the dialog box.
        .flags = cdlOFNHideReadOnly Or cdlOFNFileMustExist
                                                'Hide the read only checkbox, and the user has to enter a file that already exists.
        .ShowOpen                               'Show the common dialog box.
    End With
    cmdPlay.Enabled = True                      'Enable the play button.
    
    
                                                'Display the selected wave file.
        
    Dim l_d As Long
    l_d = 1
    Do While InStr(l_d, cdlgLoad.FileName, "\", vbBinaryCompare) <> 0
        l_d = l_d + 1                           'Loop until the last \ is found
    Loop
        
    lblTitle = Right(cdlgLoad.FileName, Len(cdlgLoad.FileName) - (l_d - 1))
        
    Exit Sub                                    'Exit the subroutine.

ErrorHandler:                                   'Set up error handling for a cancel error.
    If Err.Number = cdlCancel Then              'If cancel was selected,
        Exit Sub                                'Exit the sub.
    End If
    
End Sub

Private Function CreateStreamingBuffer() As Boolean
    
    'This sub sets up the streaming buffer.
    
    Dim lngCount As Long                        'Standard count variable.
    Close #FileFree                             'Close the file in case it is open.
    Set dsb = Nothing                           'Set the secondary buffer to nothing.
    Format = FillFormat()                       'Fill the format structure by calling the FillFormat function.
    If Format.nFormatTag <> WAVE_FORMAT_PCM Then
                                                'If an unsupported format is attempting to load,
        MsgBox "Unsupported format"             'display this message.
        Close #FileFree                         'Close the open file.
        Exit Function                           'Exit the sub.
    End If

    lngNotificationSize = (Format.lSamplesPerSec * 2) \ 2
    dsbd.lBufferBytes = lngNotificationSize * 2
    lngLastBit = (dwDataLength \ dsbd.lBufferBytes) * dsbd.lBufferBytes
                                                'Create a half second buffer.
    dsbd.lFlags = DSBCAPS_GETCURRENTPOSITION2 Or DSBCAPS_CTRLPOSITIONNOTIFY
                                                'Set the flags for the buffer. Flags needed are DSBCAPS_GLOBALFOCUS,
                                                'DSBCAPS_GETCURRENTPOSITION2 for accurate notification position tracking,
                                                'and DSBCAPS_CTRLPOSITIONNOTIFY to let Direct Sound know we are keeping
                                                'track of the position during playback.
    Set dsb = ds.CreateSoundBuffer(dsbd, Format)
                                                'Create the buffer with the above structures.
    If dwDataLength >= dsbd.lBufferBytes Then
        psa(0).lOffset = (dsbd.lBufferBytes) \ 2
        psa(0).hEventNotify = hEvent(0)
    
        psa(1).lOffset = (dsbd.lBufferBytes - 1)
        psa(1).hEventNotify = hEvent(1)
            
        dsb.SetNotificationPositions 2, psa()       'Set the notification positions for the buffer.
                                                    'Set the playback position to the middle of the buffer to trigger the first event.
    End If
    
    ReDim Buffer(dsbd.lBufferBytes - 1)         'Resize the wave data buffer to the size of the direct sound buffer
    cmdPlay.Enabled = True                      'Enable the play button.
    CreateStreamingBuffer = True                'The function succeeded.
    
    m_bLoop = True
    If dwDataLength < dsbd.lBufferBytes Then
        ReDim Buffer(dwDataLength - 1)
        m_bLoop = False
    End If
    
    'get our first chunk of data
    Get #FileFree, , Buffer             'Read the wave data into the buffer array.
    
    dsb.WriteBuffer 0, UBound(Buffer), Buffer(0), DSBLOCK_DEFAULT
    
End Function

Private Function FillFormat() As WAVEFORMATEX
        
    Dim chunk As HeaderChunk
    Dim by As Byte
    Dim i As Long
    
    'This reads the header info from a wave file, and returns a filled WAVEFORMATEX structure from this info.
    Close #FileFree
    FileFree = FreeFile                         'Get a free file handle.
    Open cdlgLoad.FileName For Binary Access Read As #FileFree
                                                'Open the selected wave file for binary input.
    Get #FileFree, , Header                     'Get the wave header data, and fill the header structure with the info.
    If Header.dwRiff <> &H46464952 Then         'This is not a valid Riff
        Exit Function
    End If
    If Header.dwWave <> &H45564157 Then         'This is not a valid Wave
        Exit Function
    End If
    Dim lCount As Long
    
    If Header.dwFormatLength < 16 Then          'We will only handle formats that are 16 bytes or greater
        Exit Function
    End If
    
    Get #FileFree, , HdrFormat                  'Get the wave format data
    
                    
    'get rid of extra format bytes
    For i = 1 To Header.dwFormatLength - 16
        Get #FileFree, , by
    Next
    
    Get #FileFree, , chunk
    Do While chunk.dwType <> &H61746164 'DATA chunck
        For i = 1 To chunk.dwLen
            Get #FileFree, , by
        Next
        Get #FileFree, , chunk
    Loop
    
    dwDataLength = chunk.dwLen
    
    With FillFormat                             'Fill the WAVEFORMATEX structure with the info from the file header.
        .lAvgBytesPerSec = HdrFormat.nAvgBytesPerSec
        .lExtra = 0
        .lSamplesPerSec = HdrFormat.nSamplesPerSec
        .nBitsPerSample = HdrFormat.wBitsPerSample
        .nBlockAlign = HdrFormat.nBlockAlign
        .nChannels = HdrFormat.nChannels
        .nFormatTag = HdrFormat.wFormatTag
    End With
    
    'The file is left open to keep the file read position at the start of the wave file data.
    
End Function
Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
          
    'This is the callback sub for the DirectX event. The buffer data is written to the direct sound buffer here.
    
    Select Case eventid
        Case hEvent(0)                          'Event 0 has fired.
        
            If Loc(FileFree) > lngLastBit Then 'This is the last section of the buffer
                fEnd = fEnd + 1
                Get #FileFree, , Buffer 'Read in the buffer
                Dim dwStartSilence As Long
                Dim dwLenSilence As Long
                dwStartSilence = dwDataLength - lngLastBit
                dwLenSilence = dsbd.lBufferBytes - dwStartSilence
                Call RtlZeroMemory(Buffer(dwStartSilence), dwLenSilence) 'Zero the buffer out
            Else
                Get #FileFree, , Buffer             'Read the wave data into the buffer array.
            End If
            Dim j As Long
            j = ((UBound(Buffer) + 1) \ 2)
            j = j + j Mod 2
            dsb.WriteBuffer 0, j, Buffer(0), DSBLOCK_DEFAULT
            'Write to the buffer, using half of the data contained
            'in the wave data buffer, give it the starting element of the buffer,
            'and use the default flag for the buffer.
            
        Case hEvent(1)

            'Event 1 has fired.
                Dim h As Long
                h = ((UBound(Buffer) + 1) \ 2)
                dsb.WriteBuffer h, h, Buffer(0), DSBLOCK_DEFAULT
                If fEnd = 2 Then
                    cmdPlay.Enabled = True
                    cmdLoad.Enabled = True
                    cmdStop.Enabled = False
                    dsb.Stop
                End If
            
    End Select
    
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    
    'Make sure that everything is stopped and reset before exiting.
    
    Set dsb = Nothing                           'Set the secondary buffer to nothing.
    Set dsbPrimary = Nothing                    'Set the primary buffer object to nothing.
    If hEvent(0) <> 0 Then                      'If event handle zero exists,
        dx.DestroyEvent hEvent(0)               'destroy it.
    End If
    If hEvent(1) <> 0 Then                      'If event handle one exists,
        dx.DestroyEvent hEvent(1)               'destroy it.
    End If
    Set dx = Nothing                            'Set the DirectX object to nothing.

End Sub
