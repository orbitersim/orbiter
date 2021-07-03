Attribute VB_Name = "basSetupRes"
Option Explicit

'
' Public vars
'
Public Const resSETMSG% = 1

'
'frmBegin
'
Public Const resSPECDEST% = 100
Public Const resSPECNODEST% = 101

'
'frmWelcome
'
Public Const resWELCOME% = 200

'
'frmPath
'
Public Const resDESTPROMPT% = 301
Public Const resDESTDIR% = 302
Public Const resCREATE% = 303
Public Const resCHANGEDIR% = 304

'
'Setup1.Bas
'
Public Const resCALCSPACE% = 400
Public Const resDRVREAD% = 401
Public Const resDRVCHK% = 402
Public Const resCANTOPEN% = 403

Public Const resMAKEDIR% = 407

Public Const resASKEXIT% = 411
Public Const resINTERRUPTED% = 412
Public Const resCANRUN% = 413
Public Const resINCOMPLETE% = 414
Public Const resQUITNOW% = 415
Public Const resQUITSETUP% = 416
Public Const resSUCCESS% = 417
Public Const resERROR% = 418
Public Const resALLOCUNIT% = 419
Public Const resSAMEASSRC% = 420

Public Const resINSERT% = 423
Public Const resDISK% = 424
Public Const resINTO% = 425
Public Const resSECTNAME% = 426
Public Const resINVLINE% = 427
Public Const resDIRSPECIFIED% = 428
Public Const resDIRINVALID% = 429
Public Const resNOTEXIST% = 430
Public Const resDIRINVNAME% = 431
Public Const resWRITEPROT% = 432
Public Const resINUSE% = 433
Public Const resOUTOFSPACE% = 434
Public Const resACCESSVIOLATION% = 435
Public Const resSHARINGVIOLATION% = 436
Public Const resOUTOFMEMORY% = 437
Public Const resCANNOTCREATE% = 438
Public Const resCANNOTDELETE% = 439
Public Const resCANNOTRENAME% = 440
Public Const resCANNOTREADSRC% = 441
Public Const resCANNOTREADDST% = 442
Public Const resBUFFTOOSMALL% = 443
Public Const resNOINSTALL% = 444
Public Const resCHKCONNECT% = 445
Public Const resWARNIGNORE% = 446

Public Const resCANTREADUNC% = 449
Public Const resCHECKUNC% = 450
Public Const resCANTFINDAPPREMOVALLOG = 451
Public Const resBADCOMMANDLINE = 452
Public Const resCANTFINDAPPREMOVALEXE = 453

Public Const resCANTREGISTERAPPREMOVER = 455

Public Const resCHOOSENEWDEST = 460

Public Const resERR_REG = 462
Public Const resMICROSOFTSHARED = 463

Public Const resCANTCREATEPROGRAMGROUP = 465
Public Const resCANTCREATEPROGRAMICON = 466
Public Const resCANTCOPYLOG = 467
Public Const resCANTFINDREGFILE = 468
Public Const resCANTRUNPROGRAM = 469
Public Const resREMOTELINENOTFOUND = 470

Public Const resCANTCOPYPATHTOOLONG% = 473
Public Const resCANTCREATEICONPATHTOOLONG% = 474
Public Const resICONMISSING = 475
'
'Setup1.Frm
'
Public Const resBADDEFDIR% = 500
Public Const resDISKSPACE% = 501
Public Const resPROGMAN% = 502
Public Const resUPDATING% = 503

Public Const resSETUP% = 505
Public Const resNOSETUPLST% = 506
Public Const resUNEXPECTED% = 507

Public Const resSTILLWITHINACTION = 509
Public Const resREMAUTGROUPNAME = 510
Public Const resAUTMGR32ICON = 511
Public Const resRACMGR32ICON = 512

Public Const resINSTALLADO = 514
Public Const resREBOOT = 515
Public Const resREBOOTNO = 516
'
'Common.Bas
'
Public Const resDISKSPCERR% = 600

'
'Forms/Controls
'
Public Const resFRMDIRECTORY% = 701
Public Const resBTNCHGDIR% = 702
Public Const resBTNEXIT% = 703
Public Const resLBLBEGIN = 704
Public Const resBTNCANCEL% = 705
Public Const resLBLDESTFILE% = 706
Public Const resBTNINSTALLNOW% = 707
Public Const resBTNCHGDRV% = 708
Public Const resLBLDRIVE% = 709
Public Const resLBLAVAIL% = 710
Public Const resLBLNEEDED% = 711
Public Const resLBLREQUIRED% = 712
Public Const resLBLNOSPACE% = 713
Public Const resBTNOK% = 714
Public Const resLBLDRIVES% = 715
Public Const resLBLDIRS% = 716
Public Const resLBLPATH% = 717
Public Const resLBLRUNNING% = 718
Public Const resBTNTOOLTIPBEGIN = 719

'
' Group.Frm
'
Public Const resGROUPFRM = 800
Public Const resGROUPLBLMAIN = 801
Public Const resGROUPLBLGROUP = 802
Public Const resGROUPLBLGROUPS = 803
Public Const resGROUPBTNCONTINUE = 804
Public Const resGROUPINVALIDGROUPNAME = 805
'
'ServerDt.Frm
'
Public Const resNETWORKADDRESS = 900
Public Const resNETWORKPROTOCOL = 901
Public Const resOK = 902
Public Const resCANCEL = 903
Public Const resREMOTESERVERDETAILSTITLE = 904
Public Const resREMOTESERVERDETAILSLBL = 905
Public Const resNOTEPROTOSEQNOTSUPPORTED = 906
Public Const resNOTEPROTOSEQINVALID = 907
Public Const resPROTOSEQUNEXPECTEDERR = 908
Public Const resNOPROTOCOLSINSETUPLST = 909
Public Const resNOPROTOCOLSSUPPORTED1 = 910
Public Const resNOPROTOCOLSSUPPORTED2 = 911
Public Const resSELECTEDPROTONOTSUPPORTED = 912


' Overwrite forms information
Public Const resOVERWRITEFORM = 1000
Public Const resOVERWRITEINFO = 1001
Public Const resOVERWRITEFILE = 1002
Public Const resOVERWRITEDESC = 1003
Public Const resOVERWRITEVER = 1004
Public Const resOVERWRITEKEEP = 1005
Public Const resOVERNOTOALL = 1006
Public Const resOVERYES = 1007
Public Const resOVERNO = 1008
'
'Logging (common with bootstrapper)
'
Public Const resLOG_FILEUPTODATE = 2000
Public Const resLOG_FILECOPIED = 2001
Public Const resLOG_ERROR = 2002
Public Const resLOG_WARNING = 2003
Public Const resLOG_DURINGACTION = 2004

Public Const resLOG_USERRESPONDEDWITH = 2012
Public Const resLOG_CANTRUNAPPREMOVER = 2013
Public Const resLOG_ABOUTTOREMOVEAPP = 2014

Public Const resLOG_vbok = 2100
Public Const resLOG_vbCancel = 2101
Public Const resLOG_vbabort = 2102
Public Const resLOG_vbretry = 2103
Public Const resLOG_vbignore = 2104
Public Const resLOG_vbyes = 2105
Public Const resLOG_vbno = 2106
Public Const resLOG_IDUNKNOWN = 2107


'Other resources possibly common with the bootstrapper
Public Const resCOMMON_CANTREG = 2200
Public Const resCOMMON_CANTREGUNEXPECTED = 2201
Public Const resCOMMON_CANTREGOLE = 2202
Public Const resCOMMON_CANTREGLOAD = 2203
Public Const resCOMMON_CANTREGENTRY = 2204
Public Const resCOMMON_CANTREGREG = 2205

Public Const resCOMMON_INVALIDFILECHARS = 2209
Public Const resCOMMON_MULTDIRBASENAME = 2210
Public Const resCOMMON_CANTFINDSRCFILE = 2211
Public Const resCOMMON_CANTREGTLB = 2212
Public Const resCOMMON_RICHED32NOTCOPIED = 2213
