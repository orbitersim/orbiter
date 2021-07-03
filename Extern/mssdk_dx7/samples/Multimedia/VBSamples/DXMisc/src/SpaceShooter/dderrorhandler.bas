Attribute VB_Name = "ErrorHandler"

Public Sub DisplayError(ErrorNumber As Long, Optional strLocation As String)
    
    'This sub is left over from a previous version, but might prove useful to some people. It accepts an
    'error number which is translated to a string, and an optional location string that can be passed to
    'display where in the code the error occurred.
    
    Dim strMsg As String
     
    Select Case ErrorNumber
        Case -2005532667
            strMsg = "DDERR_ALREADYINITIALIZED"
        Case -2005532662
            strMsg = "DDERR_CANNOTATTACHSURFACE"
        Case -2005532632
            strMsg = "DDERR_CURRENTLYNOTAVAIL"
        Case -2005532617
            strMsg = "DDERR_EXCEPTION"
        Case -2005532582
            strMsg = "DDERR_HEIGHTALIGN"
        Case -2005532577
            strMsg = "DDERR_INCOMPATIBLEPRIMARY"
        Case -2005532572
            strMsg = "DDERR_INVALIDCAPS"
        Case -2005532562
            strMsg = "DDERR_INVALIDCLIPLIST"
        Case -2005532552
            strMsg = "DDERR_INVALIDMODE"
        Case -2005532542
            strMsg = "DDERR_INVALIDOBJECT"
        Case -2005532527
            strMsg = "DDERR_INVALIDPIXELFORMAT"
        Case -2005532522
            strMsg = "DDERR_INVALIDRECT"
        Case -2005532512
            strMsg = "DDERR_LOCKEDSURFACES"
        Case -2005532462
            strMsg = "DDERR_NOCOLORCONVHW"
        Case -2005532460
            strMsg = "DDERR_NOCOOPERATIVELEVELSET"
        Case -2005532457
            strMsg = "DDERR_NOCOLORKEY"
        Case -2005532452
            strMsg = "DDERR_NOCOLORKEYHW"
        Case -2005532450
            strMsg = "DDERR_NODIRECTDRAWSUPPORT"
        Case -2005532447
            strMsg = "DDERR_NOEXCLUSIVEMODE"
        Case -2005532442
            strMsg = "DDERR_NOFLIPHW"
        Case -2005532422
            strMsg = "DDERR_NOMIRRORHW"
        Case -2005532417
            strMsg = "DDERR_NOTFOUND"
        Case -2005532412
            strMsg = "DDERR_NOOVERLAYHW"
        Case -2005532392
            strMsg = "DDERR_NORASTEROPHW"
        Case -2005532352
            strMsg = "DDERR_NOT8BITCOLOR"
        Case -2005532342
            strMsg = "DDERR_NOTEXTUREHW"
        Case -2005532337
            strMsg = "DDERR_NOVSYNCHW"
        Case -2005532332
            strMsg = "DDERR_NOZBUFFERHW"
        Case -2005532322
            strMsg = "DDERR_NOZOVERLAYHW"
        Case -2005532312
            strMsg = "DDERR_OUTOFCAPS"
        Case -2005532292
            strMsg = "DDERR_OUTOFVIDEOMEMORY"
        Case -2005532285
            strMsg = "DDERR_PALETTEBUSY"
        Case -2005532272
            strMsg = "DDERR_COLORKEYNOTSET"
        Case -2005532262
            strMsg = "DDERR_SURFACEALREADYATTACHED"
        Case -2005532252
            strMsg = "DDERR_SURFACEALREADYDEPENDENT"
        Case -2005532242
            strMsg = "DDERR_SURFACEBUSY"
        Case -2005532237
            strMsg = "DDERR_CANTLOCKSURFACE"
        Case -2005532232
            strMsg = "DDERR_SURFACEISOBSCURED"
        Case -2005532222
            strMsg = "DDERR_SURFACELOST"
        Case -2005532212
            strMsg = "DDERR_SURFACENOTATTACHED"
        Case -2005532202
            strMsg = "DDERR_TOOBIGHEIGHT"
        Case -2005532192
            strMsg = "DDERR_TOOBIGSIZE"
        Case -2005532182
            strMsg = "DDERR_TOOBIGWIDTH"
        Case -2005532162
            strMsg = "DDERR_UNSUPPORTEDFORMAT"
        Case -2005532135
            strMsg = "DDERR_VERTICALBLANKINPROGRESS"
        Case -2005532132
            strMsg = "DDERR_WASSTILLDRAWING"
        Case -2005532112
            strMsg = "DDERR_XALIGN"
        Case -2005532111
            strMsg = "DDERR_INVALIDDIRECTDRAWGUID"
        Case -2005532110
            strMsg = "DDERR_DIRECTDRAWALREADYCREATED"
        Case -2005532109
            strMsg = "DDERR_NODIRECTDRAWHW"
        Case -2005532108
            strMsg = "DDERR_PRIMARYSURFACEALREADYEXISTS"
        Case -2005532107
            strMsg = "DDERR_NOEMULATION"
        Case -2005532106
            strMsg = "DDERR_REGIONTOOSMALL"
        Case -2005532103
            strMsg = "DDERR_NOHWND"
        Case -2005532102
            strMsg = "DDERR_HWNDSUBCLASSED"
        Case -2005532101
            strMsg = "DDERR_HWNDALREADYSET"
        Case -2005532100
            strMsg = "DDERR_NOPALETTEATTACHED"
        Case -2005532099
            strMsg = "DDERR_NOPALETTEHW"
        Case -2005532098
            strMsg = "DDERR_BLTFASTCANTCLIP"
        Case -2005532097
            strMsg = "DDERR_NOBLTHW"
        Case -2005532096
            strMsg = "DDERR_NODDROPSHW"
        Case -2005532095
            strMsg = "DDERR_OVERLAYNOTVISIBLE"
        Case -2005532094
            strMsg = "DDERR_NOOVERLAYDEST"
        Case -2005532093
            strMsg = "DDERR_INVALIDPOSITION"
        Case -2005532092
            strMsg = "DDERR_NOTAOVERLAYSURFACE"
        Case -2005532091
            strMsg = "DDERR_EXCLUSIVEMODEALREADYSET"
        Case -2005532090
            strMsg = "DDERR_NOTFLIPPABLE"
        Case -2005532089
            strMsg = "DDERR_CANTDUPLICATE"
        Case -2005532088
            strMsg = "DDERR_NOTLOCKED"
        Case -2005532087
            strMsg = "DDERR_CANTCREATEDC"
        Case -2005532086
            strMsg = "DDERR_NODC"
        Case -2005532085
            strMsg = "DDERR_WRONGMODE"
        Case -2005532084
            strMsg = "DERR_IMPLICITYCREATED"
        Case -2005532083
            strMsg = "DDERR_NOTPALETTIZED"
        Case -2005532082
            strMsg = "DDERR_UNSUPPORTEDMODE"
        Case -2005532081
            strMsg = "DDERR_NOMIPMAPHW"
        Case -2005532080
            strMsg = "DDERR_INVALIDSURFACETYPE"
        Case -2005532072
            strMsg = "DDERR_NOOPTIMIZEHW"
        Case -2005532071
            strMsg = "DDERR_NOTLOADED"
        Case -2005532052
            strMsg = "DDERR_DCALREADYCREATED"
        Case -2005532042
            strMsg = "DDERR_NONONLOCALVIDMEM"
        Case -2005532032
            strMsg = "DDERR_CANTPAGELOCK"
        Case -2005532012
            strMsg = "DDERR_CANTPAGEUNLOCK"
        Case -2005531992
            strMsg = "DDERR_NOTPAGELOCKED"
        Case -2005531982
            strMsg = "DDERR_MOREDATA"
        Case -2005531977
            strMsg = "DDERR_VIDEONOTACTIVE"
        Case -2005531973
            strMsg = "DDERR_DEVICEDOESNTOWNSURFACE"
        Case Else
            If Err.Description <> "" Then
                    strMsg = "Non-DirectX error." & vbCrLf & "Error description: " & Err.Description
            Else
                    strMsg = "Non-DirectX error." & vbCrLf & "No error description is available for this error." & Err.Description & vbCrLf & "Error number: " & ErrorNumber
            End If
    End Select
    
    If strLocation <> "" Then strMsg = strMsg & vbCrLf & "Procedure where error occurred, and additional info: " & strLocation
    MsgBox "Error during program execution: " & strMsg
    
    End
    
End Sub

