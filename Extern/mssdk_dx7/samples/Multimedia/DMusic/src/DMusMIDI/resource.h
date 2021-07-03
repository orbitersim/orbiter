//{{NO_DEPENDENCIES}}
// Microsoft Developer Studio generated include file.
// Used by Midiplyr.rc
//

/* Resource ID's
*/
#define ID_ICON             50
#define ID_MENU             51

#define IDM_EXIT            100
#define IDM_ABOUT           101
#define IDM_OPEN            102
#define IDM_PLAY            103
#define IDM_PAUSE           104
#define IDM_STOP            105
#define IDM_TOOLBAR         106
#define IDM_STATUS          107
#define IDM_AUTOPLAY        108
#define IDM_REVERB			109

#define IDM_DEVMIN          129
#define IDM_MAPPER          129         /* MUST be IDM_DEVICES - 1 */
#define IDM_DEVICES         130         /* 129 thru 149 */
#define IDM_DEVMAX          149

#define IDB_TOOLBAR         200

#define IDC_TOOLBAR         300
#define IDC_STATBAR         301

#define IDS_APPTITLEMASK    1000
#define IDS_APPNAME         1001
#define IDS_UNTITLED        1002
#define IDS_PLAYTHRU		1003

/* ID's for these must be contiguous !!!
** Note that we also use these as IDM_ items in the Options menu
*/
#define IDS_TF_FIRST      1010
#define IDS_HMS                 1010
#define IDS_TICKS              1011
#define IDS_TF_LAST       1011

/* ID's for the segment's state descriptions
** These must also be contigous and in the same order as the SEG_S
** states
*/
#define SEG_S_OPENED        1
#define SEG_S_PLAYING       4
#define SEG_S_PAUSED        5

#define IDS_STATES          1020
#define IDS_OPENED          (IDS_STATES + SEG_S_OPENED)
#define IDS_PLAYING         (IDS_STATES + SEG_S_PLAYING)
#define IDS_PAUSED          (IDS_STATES + SEG_S_PAUSED)

#define N_TIME_FORMATS      (IDS_TF_LAST - IDS_TF_FIRST + 1)
#define CB_TIME_FORMATS     40

#define IDS_OPENFAILED      1050
#define IDS_PREROLLFAILED   1051
#define IDS_TESTERR         1052
#define IDS_STOPFAILED		1053

// Next default values for new objects
// 
#ifdef APSTUDIO_INVOKED
#ifndef APSTUDIO_READONLY_SYMBOLS
#define _APS_NO_MFC                     1
#define _APS_NEXT_RESOURCE_VALUE        102
#define _APS_NEXT_COMMAND_VALUE         40002
#define _APS_NEXT_CONTROL_VALUE         1000
#define _APS_NEXT_SYMED_VALUE           101
#endif
#endif
