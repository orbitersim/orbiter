Attribute VB_Name = "basSetupRes"
Option Explicit
Option Compare Text

'
' Global vars
'
Global Const resSETMSG% = 1

'
'frmBegin
'
Global Const resSPECDEST% = 100
Global Const resSPECNODEST% = 101

'
'frmWelcome
'
Global Const resWELCOME% = 200

'
'frmPath
'
Global Const resSRCPROMPT% = 300
Global Const resDESTPROMPT% = 301
Global Const resDESTDIR% = 302
Global Const resCREATE% = 303
Global Const resCHANGEDIR% = 304
Global Const resINSTFROM% = 305

'
'Setup1.Bas
'
Global Const resCALCSPACE% = 400
Global Const resDRVREAD% = 401
Global Const resDRVCHK% = 402
Global Const resCANTOPEN% = 403
Global Const resCANTREAD% = 404
Global Const resCANTWRITE% = 405
Global Const resCHKSPACE% = 406
Global Const resMAKEDIR% = 407
Global Const resASKEXIT% = 411
Global Const resINTERRUPTED% = 412
Global Const resCANRUN% = 413
Global Const resINCOMPLETE% = 414
Global Const resQUITNOW% = 415
Global Const resQUITSETUP% = 416
Global Const resSUCCESS% = 417
Global Const resERROR% = 418
Global Const resALLOCUNIT% = 419
Global Const resSAMEASSRC% = 420
Global Const resNOCREATE% = 421
Global Const resNOTPROTECT% = 422
Global Const resINSERT% = 423
Global Const resDISK% = 424
Global Const resINTO% = 425
Global Const resSECTNAME% = 426
Global Const resINVLINE% = 427
Global Const resDIRSPECIFIED% = 428
Global Const resDIRINVALID% = 429
Global Const resNOTEXIST% = 430
Global Const resDIRINVNAME% = 431
Global Const resWRITEPROT% = 432
Global Const resINUSE% = 433
Global Const resOUTOFSPACE% = 434
Global Const resACCESSVIOLATION% = 435
Global Const resSHARINGVIOLATION% = 436
Global Const resOUTOFMEMORY% = 437
Global Const resCANNOTCREATE% = 438
Global Const resCANNOTDELETE% = 439
Global Const resCANNOTRENAME% = 440
Global Const resCANNOTREADSRC% = 441
Global Const resCANNOTREADDST% = 442
Global Const resBUFFTOOSMALL% = 443
Global Const resNOINSTALL% = 444
Global Const resCHKCONNECT% = 445
Global Const resWARNIGNORE% = 446
Global Const resTEMPDRIVE% = 447
Global Const resERR_VSHARE% = 448
Global Const resCANTREADUNC% = 449
Global Const resCHECKUNC% = 450
Global Const resCANTFINDAPPREMOVALLOG = 451
Global Const resBADCOMMANDLINE = 452
Global Const resCANTFINDAPPREMOVALEXE = 453
Global Const resAPPREMOVALICONNAME = 454
Global Const resCANTREGISTERAPPREMOVER = 455
Global Const resCANTCREATEAPPREMOVALICON = 456
Global Const resOVERWRITEPRIVATE = 457
Global Const resOVERWRITEPRIVATE2 = 458
Global Const resCANCELSETUP = 459
Global Const resCHOOSENEWDEST = 460
Global Const resVERIFYCONTINUE = 461
Global Const resERR_REG = 462
Global Const resMICROSOFTSHARED = 463
Global Const resCANTCREATEPROGRAMGROUP = 465
Global Const resCANTCREATEPROGRAMICON = 466
Global Const resCANTCOPYLOG = 467
Global Const resCANTFINDREGFILE = 468
Global Const resCANTRUNPROGRAM = 469
Global Const resREMOTELINENOTFOUND = 470
Global Const resPROGRAMMANAGER = 471
Global Const resUNEXPECTEDRPCREGDAT = 472
Global Const resCANTCOPYPATHTOOLONG% = 473
Global Const resCANTCREATEICONPATHTOOLONG% = 474
Global Const resICONMISSING = 475
'
'Setup1.Frm
'
Global Const resBADDEFDIR% = 500
Global Const resDISKSPACE% = 501
Global Const resPROGMAN% = 502
Global Const resUPDATING% = 503
Global Const resNOEXE% = 504
Global Const resSETUP% = 505
Global Const resNOSETUPLST% = 506
Global Const resUNEXPECTED% = 507
Global Const resNOFOLDERFORICON = 508
Global Const resSTILLWITHINACTION = 509
Global Const resREMAUTGROUPNAME = 510
Global Const resAUTMGR32ICON = 511
Global Const resRACMGR32ICON = 512
Global Const resNT4WithoutSP2 = 513

'
'Common.Bas
'
Global Const resDISKSPCERR% = 600

'
'Forms/Controls
'
Global Const resFRMDIRECTORY% = 701
Global Const resBTNCHGDIR% = 702
Global Const resBTNEXIT% = 703
Global Const resLBLBEGIN = 704
Global Const resBTNCANCEL% = 705
Global Const resLBLDESTFILE% = 706
Global Const resBTNINSTALLNOW% = 707
Global Const resBTNCHGDRV% = 708
Global Const resLBLDRIVE% = 709
Global Const resLBLAVAIL% = 710
Global Const resLBLNEEDED% = 711
Global Const resLBLREQUIRED% = 712
Global Const resLBLNOSPACE% = 713
Global Const resBTNOK% = 714
Global Const resLBLDRIVES% = 715
Global Const resLBLDIRS% = 716
Global Const resLBLPATH% = 717
Global Const resLBLRUNNING% = 718
Public Const resBTNTOOLTIPBEGIN = 719

'
' Group.Frm
'
Global Const resGROUPFRM = 800
Global Const resGROUPLBLMAIN = 801
Global Const resGROUPLBLGROUP = 802
Global Const resGROUPLBLGROUPS = 803
Global Const resGROUPBTNCONTINUE = 804
Global Const resGROUPINVALIDGROUPNAME = 805
Global Const resGROUPINVALIDCHARS = 806
'
'ServerDt.Frm
'
Global Const resNETWORKADDRESS = 900
Global Const resNETWORKPROTOCOL = 901
Global Const resOK = 902
Global Const resCANCEL = 903
Global Const resREMOTESERVERDETAILSTITLE = 904
Global Const resREMOTESERVERDETAILSLBL = 905
Global Const resNOTEPROTOSEQNOTSUPPORTED = 906
Global Const resNOTEPROTOSEQINVALID = 907
Global Const resPROTOSEQUNEXPECTEDERR = 908
Global Const resNOPROTOCOLSINSETUPLST = 909
Global Const resNOPROTOCOLSSUPPORTED1 = 910
Global Const resNOPROTOCOLSSUPPORTED2 = 911
Global Const resSELECTEDPROTONOTSUPPORTED = 912

'
' Global Font settings for Setup1 project.  To override,
' set font properties in code after Form_Load.
'
Public Const resFONTNAME = 950
Public Const resFONTSIZE = 951
Public Const resFONTBOLD = 952
Public Const resFONTNAMEBACKUP1 = 953
Public Const resFONTNAMEBACKUP2 = 954
'
'Logging (common with bootstrapper)
'
Global Const resLOG_FILEUPTODATE = 2000
Global Const resLOG_FILECOPIED = 2001
Global Const resLOG_ERROR = 2002
Global Const resLOG_WARNING = 2003
Global Const resLOG_DURINGACTION = 2004
Global Const resLOG_CANNOTWRITE = 2005
Global Const resLOG_CANNOTCREATE = 2006
Global Const resLOG_DONOTMODIFY = 2007
Global Const resLOG_FILECONTENTS = 2008
Global Const resLOG_FILEUSEDFOR = 2009
Global Const resLOG_USERRESPONDEDWITH = 2012
Global Const resLOG_CANTRUNAPPREMOVER = 2013
Global Const resLOG_ABOUTTOREMOVEAPP = 2014

Global Const resLOG_IDOK = 2100
Global Const resLOG_IDCANCEL = 2101
Global Const resLOG_IDABORT = 2102
Global Const resLOG_IDRETRY = 2103
Global Const resLOG_IDIGNORE = 2104
Global Const resLOG_IDYES = 2105
Global Const resLOG_IDNO = 2106
Global Const resLOG_IDUNKNOWN = 2107


'Other resources possibly common with the bootstrapper
Global Const resCOMMON_CANTREG = 2200
Global Const resCOMMON_CANTREGUNEXPECTED = 2201
Global Const resCOMMON_CANTREGOLE = 2202
Global Const resCOMMON_CANTREGLOAD = 2203
Global Const resCOMMON_CANTREGENTRY = 2204
Global Const resCOMMON_CANTREGREG = 2205
Global Const resCOMMON_CANTREGAUTPRXRPC1 = 2206
Global Const resCOMMON_CANTREGAUTPRXRPC2 = 2207
Global Const resCOMMON_CTL3D32NOTCOPIED = 2208
Global Const resCOMMON_INVALIDFILECHARS = 2209
Global Const resCOMMON_MULTDIRBASENAME = 2210
Global Const resCOMMON_CANTFINDSRCFILE = 2211
Global Const resCOMMON_CANTREGTLB = 2212
Public Const resCOMMON_RICHED32NOTCOPIED = 2213
Public Const resCOMMON_AXDISTNOTCOPIED = 2214
Public Const resCOMMON_WINT351NOTCOPIED = 2215
