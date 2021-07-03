Attribute VB_Name = "Palettes"
Option Explicit

'Initialize structures needed for parsing the bmp file information

Type BITMAPFILEHEADER
        bfType As Integer
        bfSize As Long
        bfReserved1 As Integer
        bfReserved2 As Integer
        bfOffBits As Long
End Type
Type BITMAPINFOHEADER '40 bytes
        biSize As Long
        biWidth As Long
        biHeight As Long
        biPlanes As Integer
        biBitCount As Integer
        biCompression As Long
        biSizeImage As Long
        biXPelsPerMeter As Long
        biYPelsPerMeter As Long
        biClrUsed As Long
        biClrImportant As Long
End Type

Sub CreatePaletteFromBitmap(BmpFile As String)
    
    'This sub takes a string to a bitmap, and creates a palette out of it
    
    Dim PalEntries(255) As PALETTEENTRY                             'structure to hold all palette indexes
    Dim BFHEADER As BITMAPFILEHEADER                                'bitmap file header structure
    Dim BINFOHEADER As BITMAPINFOHEADER                             'bitmap information structure
    Dim Dummy As Byte                                               'dummy variable used as a fill
    Dim intCount As Integer                                         'count variable
    
    Open BmpFile For Binary Access Read As #1                       'open the bitmap file for reading
    Get #1, , BFHEADER                                              'fill the file header
    Get #1, , BINFOHEADER                                           'fill the info header
    If BINFOHEADER.biClrUsed = 0 Then BINFOHEADER.biClrUsed = 256   'make sure that we grab a valid number of palette entries
    For intCount = 0 To BINFOHEADER.biClrUsed - 1                   'loop through all indexes
        Get #1, , PalEntries(intCount).blue                         'get the blue color of this index
        Get #1, , PalEntries(intCount).green                        'get the green color of this index
        Get #1, , PalEntries(intCount).red                          'get the red color of this index
        Get #1, , Dummy                                             'this value isn't used, but is need to increment the record count
    Next
    Close #1                                                        'close the file

    Set DDPalette = dd.CreatePalette(DDPCAPS_8BIT Or DDPCAPS_ALLOW256, PalEntries())
                                                                    'create the 256 color palette
    
End Sub

