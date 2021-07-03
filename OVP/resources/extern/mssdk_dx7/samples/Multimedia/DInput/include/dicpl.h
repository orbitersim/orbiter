//-----------------------------------------------------------------------------
// File: dicpl.h
//
// Desc: DInput suuport file
//
// Copyright (C) 1992-1999 Microsoft Corporation
//-----------------------------------------------------------------------------
#ifndef DICPL_H
#define DICPL_H


// Maximum pages allowed on a server
#define MAX_PAGES 26


// Interface ID
// {7854FB22-8EE3-11d0-A1AC-0000F8026977}
DEFINE_GUID(IID_IDIGameCntrlPropSheet, 
0x7854fb22, 0x8ee3, 0x11d0, 0xa1, 0xac, 0x0, 0x0, 0xf8, 0x2, 0x69, 0x77);


//-----------------------------------------------------------------------------
// Structures
//-----------------------------------------------------------------------------

// This pragma may not be supported by all compilers.
// Please consult your compiler documentation.
#pragma pack(8)

typedef struct 
{
	DWORD	  dwSize;
	LPWSTR    lpwszPageTitle;
	DLGPROC	  fpPageProc;
	BOOL	  fProcFlag;
	DLGPROC	  fpPrePostProc;
	BOOL	  fIconFlag;
	LPWSTR	  lpwszPageIcon;
	LPWSTR    lpwszTemplate; 
	LPARAM	  lParam;
	HINSTANCE hInstance;
} DIGCPAGEINFO, *LPDIGCPAGEINFO;


typedef struct 
{
	DWORD  dwSize;
	USHORT nNumPages;
	LPWSTR lpwszSheetCaption;
	BOOL   fSheetIconFlag;
	LPWSTR lpwszSheetIcon;
} DIGCSHEETINFO, *LPDIGCSHEETINFO;




//-----------------------------------------------------------------------------
// Interface as Exposed by the InProcServer Property Sheet
//-----------------------------------------------------------------------------
DECLARE_INTERFACE_( IDIGameCntrlPropSheet, IUnknown)
{
	// IUnknown Members
	STDMETHOD(QueryInterface)	(THIS_ REFIID, LPVOID * ppvObj) PURE;
	STDMETHOD_(ULONG, AddRef)	(THIS) PURE;
	STDMETHOD_(ULONG,Release)	(THIS) PURE;

	// IServerProperty Members
	STDMETHOD(GetSheetInfo)		(THIS_ LPDIGCSHEETINFO *) PURE; 	
	STDMETHOD(GetPageInfo)		(THIS_ LPDIGCPAGEINFO *) PURE; 	
	STDMETHOD(SetID)			(THIS_ USHORT nID) PURE;
};

typedef IDIGameCntrlPropSheet *LPIDIGAMECNTRLPROPSHEET;




//-----------------------------------------------------------------------------
// CLASS DEFINITION for CServerClassFactory
//-----------------------------------------------------------------------------
class CServerClassFactory : public IClassFactory
{
protected:
	ULONG   m_ServerCFactory_refcount;
    
public:
	// constructor
	CServerClassFactory();
	// destructor
	~CServerClassFactory();
        
	// IUnknown methods
	STDMETHODIMP            QueryInterface( REFIID, PPVOID );
	STDMETHODIMP_(ULONG)    AddRef();
	STDMETHODIMP_(ULONG)    Release();
    
	// IClassFactory methods
	STDMETHODIMP    		CreateInstance( LPUNKNOWN, REFIID, PPVOID );
	STDMETHODIMP    		LockServer( BOOL );
};




//-----------------------------------------------------------------------------
// CLASS DEFINITION for CDIGameCntrlPropSheet
//-----------------------------------------------------------------------------
class CDIGameCntrlPropSheet : public IDIGameCntrlPropSheet
{
	friend	CServerClassFactory;

private:
	DWORD   m_cProperty_refcount;
		
public:
	CDIGameCntrlPropSheet();
	~CDIGameCntrlPropSheet();
		
	// IUnknown methods
    STDMETHODIMP            QueryInterface( REFIID, PPVOID );
    STDMETHODIMP_(ULONG)    AddRef();
    STDMETHODIMP_(ULONG)    Release();
		
	STDMETHODIMP			GetSheetInfo( LPDIGCSHEETINFO* lpSheetInfo );
	STDMETHODIMP			GetPageInfo ( LPDIGCPAGEINFO* lpPageInfo  );
	STDMETHODIMP			SetID( USHORT nID );
};

typedef CDIGameCntrlPropSheet *LPCDIGAMECNTRLPROPSHEET;




//-----------------------------------------------------------------------------
// ERRORS
//-----------------------------------------------------------------------------
#define DIGCERR_ERRORSTART			0x80097000
#define DIGCERR_NUMPAGESZERO	   	0x80097001
#define DIGCERR_NODLGPROC		   	0x80097002
#define DIGCERR_NOPREPOSTPROC		0x80097003
#define DIGCERR_NOTITLE				0x80097004
#define DIGCERR_NOCAPTION		   	0x80097005
#define DIGCERR_NOICON				0x80097006
#define DIGCERR_STARTPAGETOOLARGE	0x80097007
#define DIGCERR_NUMPAGESTOOLARGE	0x80097008
#define DIGCERR_INVALIDDWSIZE		0x80097009
#define DIGCERR_ERROREND			0x80097100


#endif // DICPL_H

