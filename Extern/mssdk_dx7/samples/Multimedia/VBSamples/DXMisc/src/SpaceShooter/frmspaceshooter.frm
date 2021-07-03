VERSION 5.00
Begin VB.Form frmSpaceShooter 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Space Shooter 2000 - by Gollum"
   ClientHeight    =   7200
   ClientLeft      =   1620
   ClientTop       =   15840
   ClientWidth     =   9600
   ControlBox      =   0   'False
   FillColor       =   &H0080FFFF&
   BeginProperty Font 
      Name            =   "Comic Sans MS"
      Size            =   12
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSpaceShooter.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   99  'Custom
   Palette         =   "frmSpaceShooter.frx":08CA
   ScaleHeight     =   480
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   ShowInTaskbar   =   1  'True
   StartUpPosition =   2  'CenterScreen
   WindowState     =   2  'Maximized
End
Attribute VB_Name = "frmSpaceShooter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)

    If KeyCode = vbKeyTab Then EndGame
    
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)

    'The key up event parses a high score name, and toggles some game options
    
    If boolGettingInput Then                        'If the game is getting high score input then
        If Shift = 0 And KeyCode > 64 Then KeyCode = KeyCode + 32
                                                    'if the shift key is down, then caps are being entered
        If KeyCode > 64 And KeyCode < 123 Then      'if the keys are alpha keys then
            strBuffer = Chr$(KeyCode)               'add this key to the buffer
        ElseIf KeyCode = vbKeySpace Then            'if a space has been entered
            strBuffer = KeyCode                     'add it to the buffer
        ElseIf KeyCode = 13 Then                    'if enter has been pressed
            boolEnterPressed = True                 'toggle the enter pressed flag to on
        ElseIf KeyCode = 8 Then                     'if backspace was pressed
            If Len(strName) > 0 Then strName = Left$(strName, Len(strName) - 1)
                                                    'make the buffer is not empty, and delete any existing character
        End If
    ElseIf KeyCode = vbKeyF Then                    'if the F key is pressed
        If boolFrameRate Then                       'if the frame rate display is toggled
            boolFrameRate = False                   'turn it off
        Else                                        'otherwise
            boolFrameRate = True                    'turn it on
        End If
    ElseIf KeyCode = vbKeyJ Then                    'if the J key is pressed
        If blnJoystickEnabled Then                  'if the joystick is enabled
            blnJoystickEnabled = False              'turn it off
        Else                                        'otherwise
            blnJoystickEnabled = True               'turn it on
        End If
    ElseIf KeyCode = vbKeyM And Not boolStarted Then
                                                    'if the M key is pressed, and the game has not started
        If blnMidiEnabled Then                      'if midi is enabled
            blnMidiEnabled = False                  'toggle it off
            PlayMidi ""                             'stop playing any midi
        Else                                        'otherwise
            blnMidiEnabled = True                   'turn the midi on
            PlayMidi "title.mid"                    'play the title midi
        End If
    ElseIf KeyCode = vbKeyX Then                    'if the X key has been pressed
        If boolMaxFrameRate Then                    'if the maximum frame rate is toggled
            boolMaxFrameRate = False                'toggle it off
        Else                                        'otherwise
            boolMaxFrameRate = True                 'toggle it on
        End If
    End If
        
End Sub

