//-----------------------------------------------------------------------------
// File: CGrpTree.h
//
// Desc: 
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef CGRPTREE_H
#define CGRPTREE_H

#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <dplay.h>
#include <dplobby.h>
#include "resource.h"

#define ST_NO_SUBGROUPS         0
#define ST_SEARCH_SUBGROUPS     1

// Images
enum
{ 
    IMG_SHORTCUT_OVERLAY = 0, IMG_GROUP, IMG_HIDDEN_GROUP, 
    IMG_HIDDEN_GROUP_SELECTED, IMG_HIDDEN_STAGING_AREA_IN_PROGRESS,
    IMG_HIDDEN_STAGING_AREA, IMG_GROUP_SELECTED, IMG_PLAYER,
    IMG_STAGING_AREA_IN_PROGRESS, IMG_SPECTATOR, IMG_STAGING_AREA,
    IMG_OWNER_OVERLAY
};

// Overlays
enum
{
    OV_SHORTCUT = 1, OV_OWNER
};

typedef enum
{
    BT_INVALID = 0, BT_GROUP, BT_SHORTCUT_IN_GROUP,
    BT_PLAYER_IN_GROUP, BT_PLAYER
} BRANCH_TYPE;

class CGroupTree;

struct BRANCHSTRUCT
{
    DPID        dpid;
    BRANCH_TYPE btType;
    DWORD       dwFlags;
};

struct ENUMTREESTRUCT
{
    DPID            dpidParent;
    CGroupTree *    lpTree;
    BOOL            bRecursive;
};


class CGroupTree
{
private:
    HINSTANCE       m_hInst;
    HWND            m_hwndTreeView;
    HWND            m_hwndParent;
    HWND            m_hwndEditCtrl;
    HIMAGELIST      m_hImageList;
    BOOL            m_fDragging;
    BRANCHSTRUCT    m_bsParentOfDragging;
    BRANCHSTRUCT    m_bsDragging;
    BRANCHSTRUCT    m_bsDropTarget;
    HMENU           m_hMenu,
                    m_hRootMenu,
                    m_hGroupMenu,
                    m_hPlayerMenu,
                    m_hShortcutMenu,
                    m_hPlayerInGroupMenu;
    DPID            m_dpidMenuTarget;
    DPID            m_dpidPlayer;
    DPID            m_dpidLastGroup;

private:
    HTREEITEM FindItem( HTREEITEM htiSearchRoot, 
                        DPID dpidTarget, 
                        BRANCH_TYPE bt, 
                        DWORD dwSearch );
    HTREEITEM Insert(   HTREEITEM htiParent, 
                        DPID dpID, 
                        LPSTR lpShortNameA, 
                        BRANCH_TYPE bt, DWORD dwFlags );
    HRESULT RecursiveRename(    HTREEITEM htiSearchRoot, 
                                DPID dpidTarget, 
                                LPSTR lpszName );
    DPID    GetBranchStructOfParent( HTREEITEM htChildItem, BRANCHSTRUCT* lpbt );

public:
    LPDIRECTPLAY4A  m_lpDP4A;

public:
    CGroupTree();
    ~CGroupTree();

    BOOL    Init( HWND hWnd, LPDIRECTPLAY4A lpDP4A, DPID dpidPlayer );
    HRESULT CreateGroup( DPID dpidGroup,  LPSTR lpszShortNameA, DWORD dwFlags );
    HRESULT CreatePlayer( DPID dpidPlayer,  LPSTR lpszShortNameA, DWORD dwFlags );
    HRESULT DestroyGroup( DPID dpidGroup );
    HRESULT DestroyPlayer( DPID dpidPlayer );
    HRESULT CreateGroupInGroup( DPID dpidParentGroup, DPID dpidChildGroup, LPSTR lpszShortNameA, DWORD dwFlags );
    HRESULT AddPlayerToGroup(DPID dpidGroup, DPID dpidPlayer, DWORD dwFlags);
    HRESULT AddGroupToGroup( DPID dpidParentGroup, DPID dpidShortcut, DWORD dwFlags );
    HRESULT DeletePlayerFromGroup(DPID dpidGroup, DPID dpidPlayer );
    HRESULT DeleteGroupFromGroup( DPID dpidParentGroup, DPID dpidShortcut );
    HRESULT GetPlayerName( DPID dpidPlayerName, LPDPNAME * lplpName );
    HRESULT GetGroupName( DPID dpidGroupName, LPDPNAME * lplpName );
    DPID    GetDPIDOfCurrentSelection(BRANCHSTRUCT* lpbt = NULL);
    DPID    GetDPIDOfCurrentSelectionParent( BRANCHSTRUCT* lpbt = NULL);
    void    GetGroupIcons( DPID dpID, DWORD dwFlags, int * lpnNormal, int * lpnSelected );
    HRESULT SetPlayerName( DPID dpidPlayer, LPSTR lpszShortName );
    HRESULT SetGroupName( DPID dpidPlayer, LPSTR lpszShortName );
    HRESULT Refresh( BOOL bRecursive = TRUE );
    BOOL    Update(LPVOID lpvMsg);
    void    OnBeginDrag(NM_TREEVIEW *lpnmtv); 
    void    OnMouseMove( LONG xCur, LONG yCur); 
    void    OnLButtonUp(void); 
    void    OnRButtonDown( LONG xCur, LONG yCur); 
    void    OnDblClk( LONG xCur, LONG yCur);
    void    Redraw();
    HRESULT BeginLabelEdit(); 
    void    EndLabelEdit(TV_DISPINFO FAR * lpTVDisp );
    BOOL    OnWM_NOTIFY( WPARAM wParam, LPARAM lParam );
    HRESULT EditLabel();
    HRESULT CheckAccessRights(TV_DISPINFO FAR * lpTVDisp = NULL);
    void    SetGroupOwner( DPID dpidGroup, DPID dpidNewOwner, DPID dpidOldOwner );
};

#endif



