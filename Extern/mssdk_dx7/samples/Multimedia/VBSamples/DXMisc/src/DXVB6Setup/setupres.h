#include "commonrc.h" // Common to bootstrapper and setup1

//
//Global
//
#define resSETMSG 1

//
//frmBegin
//
#define resSPECDEST 100
#define resSPECNODEST 101

//
//frmWelcome
//
#define resWELCOME 200

//
//frmPath
//
#define resDESTPROMPT 301
#define resDESTDIR 302
#define resCREATE 303
#define resCHANGEDIR 304

//
//Setup1.Bas
//
#define resCALCSPACE 400
#define resDRVREAD 401
#define resDRVCHK 402
#define resCANTOPEN 403

#define resMAKEDIR 407

#define resASKEXIT 411
#define resINTERRUPTED 412
#define resCANRUN 413
#define resINCOMPLETE 414
#define resQUITNOW 415
#define resQUITSETUP 416
#define resSUCCESS 417
#define resERROR 418
#define resALLOCUNIT 419
#define resSAMEASSRC 420

#define resINSERT 423
#define resDISK 424
#define resINTO 425
#define resSECTNAME 426
#define resINVLINE 427
#define resDIRSPECIFIED 428
#define resDIRINVALID 429
#define resNOTEXIST 430
#define resDIRINVNAME 431
#define resWRITEPROT 432
#define resINUSE 433
#define resOUTOFSPACE 434
#define resACCESSVIOLATION 435
#define resSHARINGVIOLATION 436
#define resOUTOFMEMORY 437
#define resCANNOTCREATE 438
#define resCANNOTDELETE 439
#define resCANNOTRENAME 440
#define resCANNOTREADSRC 441
#define resCANNOTREADDST 442
#define resBUFFTOOSMALL 443
#define resNOINSTALL 444
#define resCHKCONNECT 445
#define resWARNIGNORE 446

#define resCANTREADUNC 449
#define resCHECKUNC 450
#define resCANTFINDAPPREMOVALLOG 451
#define resBADCOMMANDLINE 452
#define resCANTFINDAPPREMOVALEXE 453

#define resCANTREGISTERAPPREMOVER 455

#define resCHOOSENEWDEST 460

#define resERR_REG 462
#define resMICROSOFTSHARED 463

#define resCANTCREATEPROGRAMGROUP 465
#define resCANTCREATEPROGRAMICON 466
#define resCANTCOPYLOG 467
#define resCANTFINDREGFILE 468
#define resCANTRUNPROGRAM 469
#define resREMOTELINENOTFOUND 470

#define resCANTCOPYPATHTOOLONG 473
#define resCANTCREATEICONPATHTOOLONG 474
#define resICONMISSING 475
//
//frmSetup1
//
#define resBADDEFDIR 500
#define resDISKSPACE 501
#define resPROGMAN 502
#define resUPDATING 503

#define resSETUP 505
#define resNOSETUPLST 506
#define resUNEXPECTED 507

#define resSTILLWITHINACTION 509
#define resREMAUTGROUPNAME 510
#define resAUTMGR32ICON 511
#define resRACMGR32ICON 512

#define resINSTALLADO 514
#define resREBOOT 515
#define resREBOOTNO 516
//
//Common.Bas
//
#define resDISKSPCERR 600

//
//Forms/Controls
//
#define resBTNINSTALL 700
#define resFRMDIRECTORY 701
#define resBTNCHGDIR 702
#define resBTNEXIT 703
#define resLBLBEGIN 704
#define resBTNCANCEL 705
#define resLBLDESTFILE 706
#define resBTNINSTALLNOW 707
#define resBTNCHGDRV 708
#define resLBLDRIVE 709
#define resLBLAVAIL 710
#define resLBLNEEDED 711
#define resLBLREQUIRED 712
#define resLBLNOSPACE 713
#define resBTNOK 714
#define resLBLDRIVES 715
#define resLBLDIRS 716
#define resLBLPATH 717
#define resLBLRUNNING 718
#define resBTNTOOLTIPBEGIN 719
//
// Group.Frm
//
#define resGROUPFRM 800
#define resGROUPLBLMAIN 801 
#define resGROUPLBLGROUP 802
#define resGROUPLBLGROUPS 803
#define resGROUPBTNCONTINUE 804
#define resGROUPINVALIDGROUPNAME 805
//
//ServerDt.Frm
//
#define resNETWORKADDRESS 900
#define resNETWORKPROTOCOL 901
#define resOK 902
#define resCANCEL 903
#define resREMOTESERVERDETAILSTITLE 904
#define resREMOTESERVERDETAILSLBL 905
#define resNOTEPROTOSEQNOTSUPPORTED 906
#define resNOTEPROTOSEQINVALID 907
#define resPROTOSEQUNEXPECTEDERR 908
#define resNOPROTOCOLSINSETUPLST 909
#define resNOPROTOCOLSSUPPORTED1 910
#define resNOPROTOCOLSSUPPORTED2 911
#define resSELECTEDPROTONOTSUPPORTED 912


// Starting new resources at 1000
#define resOVERWRITEFORM	1000
#define resOVERWRITEINFO	1001
#define resOVERWRITEFILE	1002
#define resOVERWRITEDESC	1003
#define resOVERWRITEVER	1004
#define resOVERWRITEKEEP	1005
#define resOVERNOTOALL	1006
#define resOVERYES		1007
#define resOVERNO			1008
// Note: commonrc.h starts with ID numbers at 2000
