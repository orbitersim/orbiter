//-----------------------------------------------------------------------------
// File: CGrpTree.cpp
//
// Desc: An abstracted Tree control that knows how to handle
//       DirectPlay system messages and enumerations.
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#include "cgrptree.h"

// Make CallWindowProc() work whether compiling STRICT or not
#ifdef STRICT
#define SWNDPROC WNDPROC
#else
#define SWNDPROC FARPROC
#endif

SWNDPROC            gwpOrigEditProc;




//-----------------------------------------------------------------------------
// Name: EditCtrlSubProc()
// Desc:
//-----------------------------------------------------------------------------
LONG FAR PASCAL EditCtrlSubProc( HWND hWnd, WORD wMsg, WORD wParam,LONG lParam )
{
    switch( wMsg )
    {
        case WM_GETDLGCODE:
            return( DLGC_WANTALLKEYS |
                    CallWindowProc( gwpOrigEditProc, hWnd, wMsg,
                                    wParam, lParam ) );
 
        case WM_CHAR:
            //Process this message to avoid message beeps.
            if( (wParam == VK_RETURN) || (wParam == VK_TAB) )
                return 0;
            break;
    }

    return CallWindowProc( gwpOrigEditProc, hWnd, wMsg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Name: TV_EnumPlayersCallback()
// Desc:
//-----------------------------------------------------------------------------
BOOL FAR PASCAL TV_EnumPlayersCallback( DPID dpId, DWORD dwPlayerType, 
                                        const DPNAME* pName,
                                        DWORD dwFlags, VOID* pContext )
{
    ENUMTREESTRUCT* p = (ENUMTREESTRUCT*)pContext;

    if( DPPLAYERTYPE_GROUP == dwPlayerType )
    {
        if( DPENUMGROUPS_SHORTCUT & dwFlags )
        {
            p->lpTree->AddGroupToGroup( p->dpidParent, dpId, dwFlags );
        }
        else
        {
            HRESULT hr;
            ENUMTREESTRUCT  ets;
            ets.dpidParent = dpId;
            ets.lpTree     = p->lpTree;
            ets.bRecursive = p->bRecursive;

            if( NULL == p->dpidParent )
                p->lpTree->CreateGroup( dpId, pName->lpszShortNameA, dwFlags );
            else
                p->lpTree->CreateGroupInGroup( p->dpidParent, dpId, 
                                               pName->lpszShortNameA, dwFlags );

            if( ets.bRecursive )
            {
                hr = p->lpTree->m_lpDP4A->EnumGroupsInGroup( dpId, NULL, 
                                                             TV_EnumPlayersCallback, 
                                                             &ets, DPENUMPLAYERS_ALL );

                hr = p->lpTree->m_lpDP4A->EnumGroupPlayers( dpId, NULL, 
                                                            TV_EnumPlayersCallback, 
                                                            &ets, DPENUMPLAYERS_ALL );
            }

        }
    }
    else
    {
        if( p->dpidParent )
            p->lpTree->AddPlayerToGroup( p->dpidParent, dpId, dwFlags );
        else
            p->lpTree->CreatePlayer( dpId, pName->lpszShortNameA, dwFlags );
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: CompareFunc()
// Desc:
//-----------------------------------------------------------------------------
int CALLBACK CompareFunc( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort )
{
    BRANCHSTRUCT* lp1 = (BRANCHSTRUCT*)lParam1;
    BRANCHSTRUCT* lp2 = (BRANCHSTRUCT*)lParam2;

    if( lp1->btType < lp2->btType )
        return -1;

    if( lp1->btType > lp2->btType )
        return 1;

    // If we got here, they are of the same type, so sort alphabetically

    DPNAME* lpdpn1 = NULL;
    DPNAME* lpdpn2 = NULL;
    int     iRes = 0;
    HRESULT hr = DPERR_GENERIC;
    CGroupTree* lpgt = (CGroupTree *) lParamSort;

    if( (lp1->btType == BT_PLAYER ) || (lp1->btType == BT_PLAYER_IN_GROUP ) )
        hr = lpgt->GetPlayerName( lp1->dpid, &lpdpn1 );
    else
        hr = lpgt->GetGroupName( lp1->dpid, &lpdpn1 );

    if( FAILED(hr) )
        goto FAILURE;

    if( (lp2->btType == BT_PLAYER ) || (lp2->btType == BT_PLAYER_IN_GROUP ) )
        hr = lpgt->GetPlayerName( lp2->dpid, &lpdpn2 );
    else
        hr = lpgt->GetGroupName( lp2->dpid, &lpdpn2 );

    if( FAILED(hr) )
        goto FAILURE;

    iRes = strcmp( lpdpn1->lpszShortNameA, lpdpn2->lpszShortNameA );

    if( 0 == iRes )
    {
        // The groups have the same name.
        if ( lp1->dpid <  lp2->dpid )
            iRes = -1;
        else if ( lp1->dpid >  lp2->dpid )
            iRes = 1;
        else
            iRes = -1;
    }

FAILURE:
    if( lpdpn1 )
        LocalFree( lpdpn1 );
    if( lpdpn2 )
        LocalFree( lpdpn2 );

    return iRes;
}




//-----------------------------------------------------------------------------
// Name: CGroupTree()
// Desc:
//-----------------------------------------------------------------------------
CGroupTree::CGroupTree()
{
    m_hInst         = GetModuleHandle( NULL );
    m_hwndTreeView  = NULL;
    m_hwndParent    = NULL;
    m_lpDP4A        = NULL;
    m_fDragging     = FALSE;
    m_dpidPlayer    = 0;
    m_dpidLastGroup = NULL;

    // Prepare popup menus
    m_hMenu              = LoadMenu( m_hInst, MAKEINTRESOURCE(IDM_MENU) );
    m_hRootMenu          = GetSubMenu( m_hMenu, 0 );
    m_hGroupMenu         = GetSubMenu( m_hMenu, 1 );
    m_hPlayerMenu        = GetSubMenu( m_hMenu, 2 );
    m_hShortcutMenu      = GetSubMenu( m_hMenu, 3 );
    m_hPlayerInGroupMenu = GetSubMenu( m_hMenu, 4 );

    // Prepare tree icons
    m_hImageList = ImageList_LoadImage( m_hInst, MAKEINTRESOURCE(IDB_BITMAP1),
                                        32, 4, RGB (0,128,128),
                                        IMAGE_BITMAP, 0 );
    ImageList_SetBkColor( m_hImageList, GetSysColor(COLOR_WINDOW) );
    ImageList_SetOverlayImage( m_hImageList, IMG_SHORTCUT_OVERLAY, OV_SHORTCUT );
    ImageList_SetOverlayImage( m_hImageList, IMG_OWNER_OVERLAY, OV_OWNER );

    ZeroMemory( &m_bsDragging, sizeof( BRANCHSTRUCT ) );

}




//-----------------------------------------------------------------------------
// Name: ~CGroupTree()
// Desc:
//-----------------------------------------------------------------------------
CGroupTree::~CGroupTree()
{
    if( m_hImageList )
        ImageList_Destroy( m_hImageList );
    m_hImageList = NULL;

    DestroyMenu( m_hPlayerInGroupMenu );
    DestroyMenu( m_hShortcutMenu );
    DestroyMenu( m_hPlayerMenu );
    DestroyMenu( m_hGroupMenu );
    DestroyMenu( m_hRootMenu );
    DestroyMenu( m_hMenu );
}




//-----------------------------------------------------------------------------
// Name: Init()
// Desc:
//-----------------------------------------------------------------------------
BOOL CGroupTree::Init( HWND hWnd, LPDIRECTPLAY4A pDP, DPID dpidPlayer )
{
    if( hWnd && pDP && dpidPlayer )
    {
        m_hwndTreeView = hWnd;
        m_hwndParent   = GetParent( m_hwndTreeView );
        m_lpDP4A       = pDP;
        m_dpidPlayer   = dpidPlayer;

        TreeView_SetImageList( hWnd, m_hImageList, TVSIL_NORMAL );
        return TRUE;
    }
    else
        return FALSE;
}




//-----------------------------------------------------------------------------
// Name: GetPlayerName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::GetPlayerName( DPID dpidPlayer, DPNAME** ppName )
{
    HRESULT hr      = DPERR_GENERIC;
    VOID*   pData   = NULL;
    DWORD   dwSize  = 0;

    hr = m_lpDP4A->GetPlayerName( dpidPlayer, NULL, &dwSize );

    if( DPERR_BUFFERTOOSMALL == hr )
    {
        pData = LocalAlloc( LPTR, dwSize );

        if( pData )
        {
            hr = m_lpDP4A->GetPlayerName( dpidPlayer, pData, &dwSize );

            if( FAILED(hr) )
            {
                LocalFree( pData );
                pData = NULL;
            }
        }
        else
        {
            hr = DPERR_OUTOFMEMORY;
        }
    }

    *ppName = (DPNAME*)pData;

    return hr;
}




//-----------------------------------------------------------------------------
// Name: GetGroupName
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::GetGroupName( DPID dpidGroup, DPNAME** ppName )
{
    HRESULT hr    = DPERR_GENERIC;
    VOID*   pData = NULL;
    DWORD   dwSize  = 0;

    hr = m_lpDP4A->GetGroupName( dpidGroup, pData, &dwSize );

    if( DPERR_BUFFERTOOSMALL == hr )
    {
        pData = LocalAlloc( LPTR,  dwSize );

        if( NULL != pData )
        {
            hr = m_lpDP4A->GetGroupName( dpidGroup, pData, &dwSize );

            if( FAILED(hr) )
            {
                LocalFree( pData );
                pData = NULL;
            }
        }
        else
        {
            hr = DPERR_OUTOFMEMORY;
        }
    }


    *ppName = (DPNAME*)pData;

    return hr;
}




//-----------------------------------------------------------------------------
// Name: FindItem()
// Desc:
//-----------------------------------------------------------------------------
HTREEITEM CGroupTree::FindItem( HTREEITEM htiSearchRoot, DPID dpidTarget, 
                                BRANCH_TYPE bt, DWORD dwSearch )
{
    TV_ITEM       tvi;
    HTREEITEM     hItem;
    HTREEITEM     htiSubSearch;
    BRANCHSTRUCT* lpbs = NULL;
    BOOL          bRet;

    if( TVI_ROOT == htiSearchRoot )
        hItem = TreeView_GetRoot( m_hwndTreeView );
    else
        hItem = TreeView_GetChild( m_hwndTreeView, htiSearchRoot );

    while( hItem )
    {
        ZeroMemory( &tvi, sizeof(TV_ITEM) );
        tvi.mask  = TVIF_PARAM;
        tvi.hItem = hItem;

        bRet = TreeView_GetItem( m_hwndTreeView, &tvi );
        lpbs = (BRANCHSTRUCT*)tvi.lParam;

        if( (bRet) && (lpbs) )
        {
            if( (lpbs->dpid == dpidTarget) && ( bt == lpbs->btType) )
                return tvi.hItem;

            if( (lpbs->btType == BT_GROUP) && (ST_SEARCH_SUBGROUPS & dwSearch) )
            {
                htiSubSearch = FindItem( hItem, dpidTarget, bt, dwSearch );
                if( htiSubSearch )
                    return htiSubSearch;
            }
        }
        else
        {
            // This is bad. The item doesn't have valid data attached to it.
            OutputDebugString("CGroupTree::FindItem: Tree view item has no data.\r\n" );
        }

        hItem = TreeView_GetNextSibling( m_hwndTreeView, hItem );
    }

    return NULL;
}




//-----------------------------------------------------------------------------
// Name: Insert()
// Desc:
//-----------------------------------------------------------------------------
HTREEITEM CGroupTree::Insert( HTREEITEM htiParent, DPID dpID,
                              LPSTR lpszShortNameA, BRANCH_TYPE bt,
                              DWORD dwFlags )
{
    BRANCHSTRUCT* lpbs    = NULL;
    HTREEITEM     htrItem = NULL;

    if( NULL != FindItem( htiParent, dpID, bt, ST_NO_SUBGROUPS ) )
    {
        // if the item is already in the tree, don't insert another copy.
        // Each player, shortcut, group or subgroup can appear only once 
        // within a give parent group or root.
        return NULL;
    }

    if( lpbs = (BRANCHSTRUCT*)LocalAlloc( LPTR,  sizeof(BRANCHSTRUCT) ) )
    {
        TV_INSERTSTRUCT     tvi;
        TV_SORTCB           tvscb; 

        ZeroMemory(lpbs, sizeof( BRANCHSTRUCT ) );
        ZeroMemory(&tvscb, sizeof(tvscb));
        ZeroMemory(&tvi, sizeof(tvi));
        tvi.hParent             = htiParent;
        tvi.hInsertAfter        = TVI_LAST;

        tvi.item.mask           = TVIF_IMAGE | TVIF_PARAM | TVIF_SELECTEDIMAGE | TVIF_TEXT;
        tvi.item.pszText        = lpszShortNameA;
        tvi.item.cchTextMax     = strlen(lpszShortNameA)+1;

        switch( bt )
        {
            case BT_PLAYER:
            case BT_PLAYER_IN_GROUP:
                tvi.item.iImage = (DPPLAYER_SPECTATOR & dwFlags) ?  IMG_SPECTATOR: IMG_PLAYER;
                tvi.item.iSelectedImage = tvi.item.iImage;

                if (DPPLAYER_OWNER & dwFlags)
                {
                    tvi.item.mask |= TVIF_STATE;
                    tvi.item.state = INDEXTOOVERLAYMASK(OV_OWNER); 
                    tvi.item.stateMask = TVIS_OVERLAYMASK;
                }
                break;

            case BT_GROUP:
                GetGroupIcons( dpID, dwFlags, &tvi.item.iImage, &tvi.item.iSelectedImage );
                break;

            case BT_SHORTCUT_IN_GROUP:
                GetGroupIcons( dpID, dwFlags, &tvi.item.iImage, &tvi.item.iSelectedImage );

                tvi.item.mask |= TVIF_STATE;
                tvi.item.state = INDEXTOOVERLAYMASK(OV_SHORTCUT); 
                tvi.item.stateMask = TVIS_OVERLAYMASK;
                break;
        }

        tvi.item.lParam = (LPARAM) lpbs;

        lpbs->dpid      = dpID;
        lpbs->btType    = bt;
        lpbs->dwFlags   = dwFlags;

        htrItem = TreeView_InsertItem( m_hwndTreeView, &tvi );

        tvscb.hParent     = htiParent; 
        tvscb.lpfnCompare = CompareFunc; 
        tvscb.lParam      = (LPARAM) this; 

        TreeView_SortChildrenCB( m_hwndTreeView, &tvscb, 0 );

        if( TVI_ROOT != htiParent )
        {
            TreeView_Expand( m_hwndTreeView, htiParent, TVE_EXPAND );
        }

        Redraw();
    }

    return htrItem;
}




//-----------------------------------------------------------------------------
// Name: CreatePlayer()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::CreatePlayer( DPID dpidPlayer, LPSTR strShortNameA,
                                  DWORD dwFlags )
{
    /*
    // We currently support only one local player. This section of code would
    // create a new player at the root for each create player message received.

    DPNAME* pName = NULL;

    if( NULL == strShortNameA )
    {
        GetPlayerName( dpidPlayer, &pName );
        strShortNameA = pName->lpszShortNameA;
    }

    if( strShortNameA )
    {
        Insert( TVI_ROOT, dpidPlayer, strShortNameA, BT_PLAYER, dwFlags );
    }

    if( pName )
        LocalFree( pName );
    */

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::CreateGroup( DPID dpidGroup, LPSTR strShortNameA,
                                 DWORD dwFlags  )
{
    DPNAME* pName = NULL;

    if (NULL == strShortNameA )
    {
        GetGroupName( dpidGroup, &pName );
        strShortNameA = pName->lpszShortNameA;
    }

    if( strShortNameA )
    {
        Insert( TVI_ROOT, dpidGroup, strShortNameA, BT_GROUP, dwFlags );
    }

    if( pName )
        LocalFree( pName );


    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateGroupInGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::CreateGroupInGroup( DPID dpidParentGroup,
                                        DPID dpidChildGroup,
                                        LPSTR strShortNameA, DWORD dwFlags )
{
    HTREEITEM hParentItem = NULL;

    if( hParentItem = FindItem( TVI_ROOT, dpidParentGroup, BT_GROUP,
                                ST_SEARCH_SUBGROUPS ) )
    {
        DPNAME* pName = NULL;

        if( NULL == strShortNameA )
        {
            GetGroupName( dpidChildGroup, &pName );
            strShortNameA = pName->lpszShortNameA;
        }

        if( strShortNameA )
        {
            Insert( hParentItem, dpidChildGroup, strShortNameA, BT_GROUP, dwFlags );
        }

        if( pName )
            LocalFree( pName );
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: AddPlayerToGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::AddPlayerToGroup( DPID dpidGroup, DPID dpidPlayer,
                                      DWORD dwFlags )
{
    HRESULT hr = DPERR_GENERIC;
    DPNAME* pPlayerName = NULL;

    hr = GetPlayerName( dpidPlayer, &pPlayerName );

    if( SUCCEEDED(hr) )
    {
        HTREEITEM hParentItem = NULL;

        if( hParentItem = FindItem( TVI_ROOT, dpidGroup, BT_GROUP,
                                    ST_SEARCH_SUBGROUPS ) )
        {
            Insert( hParentItem, dpidPlayer, pPlayerName->lpszShortNameA,
                    BT_PLAYER_IN_GROUP, dwFlags );
        }
    }

    if( pPlayerName )
        LocalFree( pPlayerName );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: AddGroupToGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::AddGroupToGroup( DPID dpidParentGroup, DPID dpidShortcut,
                                     DWORD dwFlags )
{
    HRESULT hr = DPERR_GENERIC;
    DPNAME* pGroupName = NULL;

    hr = GetGroupName( dpidShortcut, &pGroupName );

    if( SUCCEEDED(hr) )
    {
        HTREEITEM hParentItem = NULL;
        
        if( hParentItem = FindItem( TVI_ROOT, dpidParentGroup, BT_GROUP,
                                    ST_SEARCH_SUBGROUPS ) )
        {
            Insert( hParentItem, dpidShortcut, pGroupName->lpszShortNameA,
                    BT_SHORTCUT_IN_GROUP, dwFlags );
        }
    }

    if( pGroupName )
        LocalFree( pGroupName );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DestroyPlayer()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::DestroyPlayer( DPID dpidPlayer )
{
    /*
    // We are not currently adding additional players to the tree view. The 
    // Code below would add a player to the root of the tree control. If you
    // want that functionality.

    HTREEITEM htiPlayer = FindItem( TVI_ROOT, dpidPlayer, BT_PLAYER,
                                    ST_NO_SUBGROUPS );

    if( htiPlayer )
    {
        TreeView_DeleteItem( m_hwndTreeView, htiPlayer );
        Redraw();
    }
    */

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::DestroyGroup( DPID dpidGroup )
{
    HTREEITEM htiGroup = FindItem( TVI_ROOT, dpidGroup, BT_GROUP,
                                   ST_SEARCH_SUBGROUPS );

    if( htiGroup )
    {
        TreeView_DeleteItem( m_hwndTreeView, htiGroup );
        Redraw();
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DeletePlayerFromGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::DeletePlayerFromGroup( DPID dpidGroup, DPID dpidPlayer )
{
    HRESULT   hr = DPERR_GENERIC;
    HTREEITEM htiParent = NULL;
    HTREEITEM htiPlayer = NULL;

    if( htiParent = FindItem( TVI_ROOT, dpidGroup, BT_GROUP,
                              ST_SEARCH_SUBGROUPS ) )
    {
        if( htiPlayer = FindItem( htiParent, dpidPlayer, BT_PLAYER_IN_GROUP,
                                  ST_NO_SUBGROUPS ) )
        {
            TreeView_DeleteItem( m_hwndTreeView, htiPlayer );
            hr = DP_OK;

            Redraw();
        }
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DeleteGroupFromGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::DeleteGroupFromGroup( DPID dpidParentGroup,
                                          DPID dpidShortcut )
{
    HRESULT   hr = DPERR_GENERIC;
    HTREEITEM htiParent   = NULL;
    HTREEITEM htiShortcut = NULL;

    if( htiParent = FindItem( TVI_ROOT, dpidParentGroup, BT_GROUP,
                              ST_SEARCH_SUBGROUPS ) )
    {
        if( htiShortcut = FindItem( htiParent, dpidShortcut,
                                    BT_SHORTCUT_IN_GROUP, ST_NO_SUBGROUPS ) )
        {
            TreeView_DeleteItem( m_hwndTreeView, htiShortcut );
            hr = DP_OK;

            Redraw();
        }
    }
    return hr;
}




//-----------------------------------------------------------------------------
// Name: SetPlayerName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::SetPlayerName( DPID dpidPlayer, LPSTR strShortName )
{
    return RecursiveRename( TVI_ROOT, dpidPlayer, strShortName );
}




//-----------------------------------------------------------------------------
// Name: SetGroupName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::SetGroupName( DPID dpidPlayer, LPSTR strShortName )
{
    return RecursiveRename( TVI_ROOT, dpidPlayer, strShortName );
}




//-----------------------------------------------------------------------------
// Name: RecursiveRename()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::RecursiveRename( HTREEITEM htiSearchRoot, DPID dpidTarget, 
                                     LPSTR strName )
{
    TV_ITEM       tvi;
    HTREEITEM     hItem;
    BRANCHSTRUCT* lpbs = NULL;

    if( TVI_ROOT == htiSearchRoot )
        hItem = TreeView_GetRoot( m_hwndTreeView );
    else
        hItem = TreeView_GetChild( m_hwndTreeView, htiSearchRoot );

    while( hItem )
    {
        ZeroMemory( &tvi, sizeof(TV_ITEM) );
        tvi.mask  = TVIF_PARAM;
        tvi.hItem = hItem;

        TreeView_GetItem( m_hwndTreeView, &tvi );
        lpbs = (BRANCHSTRUCT*)tvi.lParam;

        if( lpbs->dpid == dpidTarget )
        {
            ZeroMemory( &tvi, sizeof(TV_ITEM) );
            tvi.hItem      = hItem;
            tvi.mask       = TVIF_TEXT;
            tvi.pszText    = strName;
            tvi.cchTextMax = strlen(strName)+1;

            TreeView_SetItem( m_hwndTreeView, &tvi );
        }

        if( lpbs->btType == BT_GROUP )
        {
            RecursiveRename( hItem, dpidTarget, strName );
        }

        hItem = TreeView_GetNextSibling( m_hwndTreeView, hItem );
    }

    return DP_OK;

}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::Refresh( BOOL bRecursive )
{
    HRESULT         hr = DPERR_GENERIC;
    ENUMTREESTRUCT  ets;

    ets.dpidParent = NULL;
    ets.lpTree     = this;
    ets.bRecursive = bRecursive;

    TreeView_DeleteAllItems( m_hwndTreeView );

    if (m_lpDP4A)
    {

        //We don't see other root level players in the lobby world.

        //hr = m_lpDP4A->EnumPlayers( NULL, TV_EnumPlayersCallback, &ets, DPENUMPLAYERS_ALL );

        //if (SUCCEEDED( hr ))
        //{
            hr = m_lpDP4A->EnumGroups( NULL, TV_EnumPlayersCallback, &ets, DPENUMPLAYERS_ALL );
        //}
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
BOOL CGroupTree::Update( VOID* lpvMsg )
{

    DWORD   dwMsgType       = ((LPDPMSG_GENERIC)lpvMsg)->dwType;
    LPSTR   lpszShortName   = NULL;
    BOOL    bReturn         = TRUE;

    if (!m_lpDP4A)
        return FALSE;

    // Draws the tree based strictly on system messages.
    // If you place a call to this method in the area where you process
    // your system messages, it should give you a good representation
    // of the DirectPlay group structure as it is created.

    switch(dwMsgType)
    {
    case DPSYS_CREATEPLAYERORGROUP:
        {
            LPDPMSG_CREATEPLAYERORGROUP lp = (LPDPMSG_CREATEPLAYERORGROUP)lpvMsg;

            lpszShortName = lp->dpnName.lpszShortNameA;
            if (DPPLAYERTYPE_PLAYER == lp->dwPlayerType)
            {
                CreatePlayer(lp->dpId, lpszShortName, lp->dwFlags );
            }
            else
            {
                if (NULL == lp->dpIdParent)
                {
                    CreateGroup(lp->dpId, lpszShortName, lp->dwFlags );
                }
                else
                {
                    CreateGroupInGroup(lp->dpIdParent, lp->dpId, lpszShortName, lp->dwFlags );
                }

            }
        }
        break;

        case DPSYS_ADDPLAYERTOGROUP:
        {
            LPDPMSG_ADDPLAYERTOGROUP lp = (LPDPMSG_ADDPLAYERTOGROUP)lpvMsg;
            DWORD   dwFlags;
            HRESULT hr = m_lpDP4A->GetPlayerFlags( lp->dpIdPlayer, &dwFlags );

            if (SUCCEEDED(hr))
            {
                // If I can't get his flags, he must have deleted himself
                // by the time I got this message.
                AddPlayerToGroup(lp->dpIdGroup, lp->dpIdPlayer, dwFlags );
            }

        }
        break;

        case DPSYS_ADDGROUPTOGROUP:
        {
            LPDPMSG_ADDGROUPTOGROUP lp = (LPDPMSG_ADDGROUPTOGROUP)lpvMsg;
            DWORD   dwFlags;
            HRESULT hr = m_lpDP4A->GetGroupFlags( lp->dpIdGroup, &dwFlags );

            if (SUCCEEDED(hr))
            {
                // If I can't get his flags, he must have deleted himself
                // by the time I got this message.
                AddGroupToGroup(lp->dpIdParentGroup, lp->dpIdGroup, dwFlags );
            }
        }
        break;

        case DPSYS_DESTROYPLAYERORGROUP:
        {
            LPDPMSG_DESTROYPLAYERORGROUP lp = (LPDPMSG_DESTROYPLAYERORGROUP) lpvMsg;

            if ( DPPLAYERTYPE_PLAYER == lp->dwPlayerType )
            {
                DestroyPlayer( lp->dpId );
            }
            else
            {
                DestroyGroup( lp->dpId );
            }
   
        }
        break;

        case DPSYS_DELETEGROUPFROMGROUP:
        {
            LPDPMSG_DELETEGROUPFROMGROUP lp =(LPDPMSG_DELETEGROUPFROMGROUP)lpvMsg;

            DeleteGroupFromGroup( lp->dpIdParentGroup, lp->dpIdGroup );
        }
        break;

        case DPSYS_DELETEPLAYERFROMGROUP:
        {
            LPDPMSG_DELETEPLAYERFROMGROUP lp =(LPDPMSG_DELETEPLAYERFROMGROUP)lpvMsg;

            DeletePlayerFromGroup( lp->dpIdGroup, lp->dpIdPlayer );
        }
        break;

        case DPSYS_SETPLAYERORGROUPDATA:
        //Nothing for right now.
        break;

        case DPSYS_SETPLAYERORGROUPNAME:
        {
            LPDPMSG_SETPLAYERORGROUPNAME lp = (LPDPMSG_SETPLAYERORGROUPNAME)lpvMsg;

            lpszShortName = lp->dpnName.lpszShortNameA;
            
            if ( DPPLAYERTYPE_PLAYER == lp->dwPlayerType )
            {
                SetPlayerName(lp->dpId, lpszShortName );
            }
            else
            {
                SetGroupName(lp->dpId, lpszShortName );
            }

        }
        break;

        case DPSYS_SETGROUPOWNER:
        {
            LPDPMSG_SETGROUPOWNER   lp = (LPDPMSG_SETGROUPOWNER) lpvMsg;

            SetGroupOwner( lp->idGroup, lp->idNewOwner, lp->idOldOwner );
        }
        break;

        default:
            //Code a new message...
            bReturn = FALSE;
            break;
    }

    return bReturn;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
DPID CGroupTree::GetDPIDOfCurrentSelection( BRANCHSTRUCT* pbt )
{

    HTREEITEM htItem = TreeView_GetSelection( m_hwndTreeView );

    if( htItem )
    {
        TV_ITEM tvi;
        ZeroMemory( &tvi, sizeof( TV_ITEM ) );
        tvi.mask  = TVIF_PARAM;
        tvi.hItem = htItem;

        TreeView_GetItem( m_hwndTreeView, &tvi );

        if( pbt )
        {
            pbt->dpid   = ((BRANCHSTRUCT*)tvi.lParam)->dpid;
            pbt->btType = ((BRANCHSTRUCT*)tvi.lParam)->btType;
        }

        return ((BRANCHSTRUCT*)tvi.lParam)->dpid;
    }

    return 0;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
DPID CGroupTree::GetDPIDOfCurrentSelectionParent( BRANCHSTRUCT* pbt )
{
    HTREEITEM htChildItem = TreeView_GetSelection(m_hwndTreeView);
    HTREEITEM htItem = TreeView_GetParent( m_hwndTreeView, htChildItem );

    if( htItem )
    {
        TV_ITEM tvi;
        ZeroMemory( &tvi, sizeof( TV_ITEM ) );
        tvi.mask  = TVIF_PARAM;
        tvi.hItem = htItem;

        TreeView_GetItem( m_hwndTreeView, &tvi );

        if( pbt )
        {
            pbt->dpid   = ((BRANCHSTRUCT*)tvi.lParam)->dpid;
            pbt->btType = ((BRANCHSTRUCT*)tvi.lParam)->btType;
        }

        return ((BRANCHSTRUCT*)tvi.lParam)->dpid;
    }

    return 0;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CGroupTree::OnBeginDrag( NM_TREEVIEW* lpnmtv )
{ 
    HIMAGELIST    himl;    // Handle of image list 
    BRANCHSTRUCT* lp      = NULL;
    int           level   = 0;
    UINT          xIndent = 0;
    HTREEITEM     htItem  = NULL;

    lp = (BRANCHSTRUCT*)lpnmtv->itemNew.lParam;
    m_bsDragging = *((BRANCHSTRUCT*)(lpnmtv->itemNew.lParam));
    GetBranchStructOfParent( lpnmtv->itemNew.hItem, &m_bsParentOfDragging );

   // Tell the tree-view control to create an image to use 
    // for dragging. 
    himl = TreeView_CreateDragImage(m_hwndTreeView, lpnmtv->itemNew.hItem); 
 
    // Start the drag operation. 
    RECT rcItem;

    TreeView_GetItemRect(m_hwndTreeView, lpnmtv->itemNew.hItem, &rcItem, FALSE);

    htItem = lpnmtv->itemNew.hItem;

    do
    {
        htItem = TreeView_GetParent( m_hwndTreeView, htItem );
        level++;
    }
    while (htItem); 

    xIndent = TreeView_GetIndent( m_hwndTreeView ) * level;

    BOOL b = ImageList_BeginDrag(   himl, 0, 
                                    lpnmtv->ptDrag.x-rcItem.left - xIndent, 
                                    lpnmtv->ptDrag.y-rcItem.top); 
  
    // Hide the mouse cursor, and direct mouse input to the 
    // parent window. 

    ShowCursor( FALSE );
    SetCapture(m_hwndParent); 
    m_fDragging = TRUE;
    
    ImageList_DragEnter( m_hwndTreeView, 
                        lpnmtv->ptDrag.x-rcItem.left - xIndent, 
                        lpnmtv->ptDrag.y);
    return; 
} 




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CGroupTree::OnMouseMove( LONG xCur, LONG yCur ) 
{ 
    HTREEITEM htiTarget;  // handle of target item 
    TV_HITTESTINFO tvht;  // hit test information 
 
    TV_ITEM tvi;

    ZeroMemory(&tvi, sizeof(TV_ITEM ) );

    if (m_fDragging) 
    { 
 
        // Drag the item to the current position of the mouse cursor. 
        RECT rcTree, rcParent;

        GetWindowRect( m_hwndParent, &rcParent );
        GetWindowRect( m_hwndTreeView, &rcTree );
        ImageList_DragMove(xCur, yCur); 
 
        // Find out if the cursor is on the item. If it is, highlight 
        // the item as a drop target. 
        tvht.pt.x = xCur; 
        tvht.pt.y = yCur; 
        if ((htiTarget = TreeView_HitTest(m_hwndTreeView, &tvht)) != NULL) 
        { 
            ImageList_DragLeave( m_hwndTreeView );
            TreeView_SelectDropTarget(m_hwndTreeView, htiTarget);
            ImageList_DragEnter( m_hwndTreeView, xCur, yCur);

            tvi.mask            = TVIF_PARAM;
            tvi.hItem           = htiTarget;

            TreeView_GetItem(m_hwndTreeView, &tvi);

            memcpy( &m_bsDropTarget, (BRANCHSTRUCT*)tvi.lParam, sizeof(BRANCHSTRUCT) );

        }
        else
        {
            ZeroMemory( &m_bsDropTarget, sizeof(BRANCHSTRUCT) );
        }
    } 
    return; 
} 
 



//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
VOID CGroupTree::OnLButtonUp()
{ 
    if (m_fDragging) 
    { 
        ImageList_EndDrag(); 
        ReleaseCapture(); 
        ShowCursor( TRUE );
        m_fDragging = FALSE;
        ImageList_DragLeave( m_hwndTreeView );
    
        if (m_bsDropTarget.dpid)
        {
            switch (m_bsDragging.btType )
            {
                case BT_PLAYER:
                    m_lpDP4A->AddPlayerToGroup(m_bsDropTarget.dpid, m_bsDragging.dpid ); 
                    break;

                case BT_GROUP:
                    m_lpDP4A->AddGroupToGroup(m_bsDropTarget.dpid, m_bsDragging.dpid );
                    break;

                case BT_PLAYER_IN_GROUP:
                    if (m_bsDropTarget.dpid != m_bsParentOfDragging.dpid)
                    {
                        m_lpDP4A->AddPlayerToGroup(m_bsDropTarget.dpid, m_bsDragging.dpid );
                        m_lpDP4A->DeletePlayerFromGroup(m_bsParentOfDragging.dpid, m_bsDragging.dpid ); 
                    }
                    break;

                case BT_SHORTCUT_IN_GROUP:
                    if (m_bsDropTarget.dpid != m_bsParentOfDragging.dpid)
                    {
                        m_lpDP4A->AddGroupToGroup(m_bsDropTarget.dpid, m_bsDragging.dpid ); 
                        m_lpDP4A->DeleteGroupFromGroup(m_bsParentOfDragging.dpid, m_bsDragging.dpid ); 
                    }
                    break;
            }
        }

        TreeView_SelectDropTarget( m_hwndTreeView, (HTREEITEM) NULL );
    } 
    return; 
} 




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
void CGroupTree::Redraw(void) 
{
    RECT r;
    
    GetWindowRect(m_hwndTreeView, &r );
    InvalidateRect(m_hwndTreeView, &r, TRUE);
    UpdateWindow(m_hwndParent);

}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
void CGroupTree::OnRButtonDown( LONG xCur, LONG yCur ) 
{ 
    HTREEITEM      htiTarget;  // handle of target item 
    TV_HITTESTINFO tvht;       // hit test information 
    BRANCHSTRUCT*  lpbs = NULL;
    HMENU          hMenu = NULL;
 
    TV_ITEM tvi;

    ZeroMemory(&tvi, sizeof(TV_ITEM ) );
    RECT rc;

    GetWindowRect( m_hwndTreeView, &rc );

    // Find out if the cursor is on the item. If it is, highlight 
    // the item as a drop target. 
    tvht.pt.x = xCur - rc.left; 
    tvht.pt.y = yCur - rc.top; 
    if ((htiTarget = TreeView_HitTest(m_hwndTreeView, &tvht)) != NULL) 
    { 
        TreeView_SelectItem(m_hwndTreeView, htiTarget);

        tvi.mask            = TVIF_PARAM;
        tvi.hItem           = htiTarget;

        TreeView_GetItem(m_hwndTreeView, &tvi);

        if (lpbs = (BRANCHSTRUCT*)tvi.lParam)
        {
            m_dpidMenuTarget = lpbs->dpid;

            switch (lpbs->btType)
            {
            case BT_GROUP:
                hMenu = m_hGroupMenu;
                EnableMenuItem( hMenu,  ID_GROUP_STARTSESSION,  
                    (DPGROUP_STAGINGAREA & lpbs->dwFlags)?MF_ENABLED:MF_GRAYED ); 
                EnableMenuItem( hMenu,  ID_GROUP_CONNECTIONSETTINGS,    
                    (DPGROUP_STAGINGAREA & lpbs->dwFlags)?MF_ENABLED:MF_GRAYED ); 
                break;

            case BT_PLAYER:
                hMenu = m_hPlayerMenu;
                break;

            case BT_SHORTCUT_IN_GROUP:
                hMenu = m_hShortcutMenu;
                break;

            case BT_PLAYER_IN_GROUP:
                hMenu = m_hPlayerInGroupMenu;
                break;

            default:
                hMenu = m_hRootMenu;
                break;
            }
        }
    }
    else
    {
            hMenu = m_hRootMenu;
    }

    TrackPopupMenuEx(   hMenu,  
                        TPM_LEFTALIGN|TPM_LEFTBUTTON|TPM_RIGHTBUTTON, 
                        xCur, yCur, m_hwndParent, NULL );   
} 




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
void CGroupTree::OnDblClk( LONG xCur, LONG yCur ) 
{ 
    HTREEITEM      htiTarget;  // handle of target item 
    TV_HITTESTINFO tvht;  // hit test information 
    BRANCHSTRUCT*  lpbs = NULL;
    HMENU          hMenu = NULL;
 
    TV_ITEM tvi;

    ZeroMemory(&tvi, sizeof(TV_ITEM ) );
    RECT rc;

    GetWindowRect( m_hwndTreeView, &rc );

    // Find out if the cursor is on the item. If it is, highlight 
    // the item as a drop target. 
    tvht.pt.x = xCur - rc.left; 
    tvht.pt.y = yCur - rc.top; 
    if ((htiTarget = TreeView_HitTest(m_hwndTreeView, &tvht)) != NULL) 
    { 
        TreeView_SelectItem(m_hwndTreeView, htiTarget);

        tvi.mask            = TVIF_PARAM;
        tvi.hItem           = htiTarget;

        TreeView_GetItem(m_hwndTreeView, &tvi);

        if (lpbs = (BRANCHSTRUCT*)tvi.lParam)
        {
            m_dpidMenuTarget = lpbs->dpid;

            switch (lpbs->btType)
            {
            case BT_SHORTCUT_IN_GROUP:
            case BT_GROUP:
                m_lpDP4A->AddPlayerToGroup(lpbs->dpid, m_dpidPlayer );

                if (m_dpidLastGroup != lpbs->dpid)
                {
                    m_lpDP4A->DeletePlayerFromGroup(m_dpidLastGroup, m_dpidPlayer ); 
                    m_dpidLastGroup = lpbs->dpid;
                }
                break;

            default:
                break;
            }
        }
    }
} 




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::BeginLabelEdit() 
{
    // Workaround for Windows tree control/dialog odd behavior
    // Retrieve the handle of the edit control. 
    m_hwndEditCtrl = TreeView_GetEditControl( m_hwndTreeView ); 

    // Subclass the edit control. 
    gwpOrigEditProc = (SWNDPROC) SetWindowLong(m_hwndEditCtrl, GWL_WNDPROC, (LONG) EditCtrlSubProc );

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
void CGroupTree::EndLabelEdit( TV_DISPINFO FAR * lpTVDisp ) 
{
    HRESULT hr;

    // Subclass the edit control. 
    SetWindowLong(m_hwndEditCtrl, GWL_WNDPROC, (LONG) gwpOrigEditProc);

    if (lpTVDisp->item.pszText)
    {
        BRANCHSTRUCT* lpbs = (BRANCHSTRUCT*)lpTVDisp->item.lParam;

        hr = CheckAccessRights(lpTVDisp);

        if (SUCCEEDED(hr))
        {   
            DPNAME dpn;
            dpn.dwSize = sizeof(DPNAME);
            dpn.lpszShortNameA = lpTVDisp->item.pszText;
            dpn.lpszLongNameA = lpTVDisp->item.pszText;

            switch (lpbs->btType)
            {
            case  BT_GROUP:
                m_lpDP4A->SetGroupName(lpbs->dpid, &dpn, DPSET_REMOTE );
                break;

            case BT_PLAYER_IN_GROUP:
            case BT_PLAYER:
                m_lpDP4A->SetPlayerName(lpbs->dpid, &dpn,  DPSET_REMOTE );
                break;
            }
        }
        else
        {
            MessageBox( m_hwndParent, "Cannot change the name of remote players or groups", "Error", MB_OK);
            return;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
DPID CGroupTree::GetBranchStructOfParent( HTREEITEM htChildItem,
                                          BRANCHSTRUCT* lpbt )
{

    HTREEITEM   htItem;
    TV_ITEM tvi;

    ZeroMemory( &tvi, sizeof( TV_ITEM ) );
    ZeroMemory( lpbt, sizeof( BRANCHSTRUCT ) );

    htItem = TreeView_GetParent( m_hwndTreeView, htChildItem );

    if (htItem)
    {

        tvi.mask            = TVIF_PARAM;
        tvi.hItem           = htItem;

        TreeView_GetItem(m_hwndTreeView, &tvi);

        if (lpbt)
        {
            *lpbt = *((BRANCHSTRUCT*)tvi.lParam);
        }

        return ((BRANCHSTRUCT*)tvi.lParam)->dpid;
    }

    return 0;

}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
BOOL CGroupTree::OnWM_NOTIFY( WPARAM wParam, LPARAM lParam )
{
    int idCtrl = (int) wParam;
    LPNMHDR pnmh = (LPNMHDR)lParam;
    if (NULL == pnmh)
        return FALSE;
    
    NM_TREEVIEW*  nmtv;
    TV_ITEM*      pItemNew = NULL;
    TV_ITEM*      pItemOld = NULL;
    BRANCHSTRUCT* lpBranch = NULL;
    HRESULT       hr;

    switch(pnmh->code)
    {
        case NM_RCLICK:
        {
            POINT p;
            GetCursorPos(&p);
            OnRButtonDown(p.x, p.y );
        }
        break;

        case NM_DBLCLK:
        {
            POINT p;
            GetCursorPos(&p);
            OnDblClk(p.x, p.y );
        }
        break;

        case TVN_BEGINLABELEDIT:
            hr = BeginLabelEdit();

            if (FAILED(hr))
                return TRUE;
        break;

        case TVN_ENDLABELEDIT:
            EndLabelEdit((TV_DISPINFO FAR *) lParam);
        break;

        case TVN_SELCHANGING:
        break;

        case TVN_DELETEITEM:
            nmtv    = (NM_TREEVIEW *)lParam;
            pItemOld    = &nmtv->itemOld;
            LocalFree( (LPVOID) pItemOld->lParam );
        break;

        case TVN_BEGINDRAG: 
            OnBeginDrag((NM_TREEVIEW *) lParam); 
            break; 

    }

    return 0;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::EditLabel()
{
    HRESULT hr; // Initialized by call to CheckAccessRights()
    HTREEITEM   htItem;
    TV_ITEM tvi;

    ZeroMemory( &tvi, sizeof( TV_ITEM ) );

    htItem = TreeView_GetSelection(m_hwndTreeView);

    if (htItem)
    {
        HWND hwnd = NULL;

        hwnd = TreeView_EditLabel( m_hwndTreeView, htItem );
        hr = (hwnd ? DP_OK : DPERR_ACCESSDENIED); 
    }
    else
    {
        hr = DPERR_ACCESSDENIED;
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
HRESULT CGroupTree::CheckAccessRights( TV_DISPINFO FAR * lpTVDisp )
{
    HRESULT hr = DPERR_GENERIC;
    DWORD   dwFlags = 0;
    BRANCHSTRUCT bsCurSel;

    if (NULL == lpTVDisp)
    {
        GetDPIDOfCurrentSelection( &bsCurSel );
    }
    else
    {   
        bsCurSel = *((BRANCHSTRUCT*)lpTVDisp->item.lParam);
    }

    switch ( bsCurSel.btType )
    {
        case BT_PLAYER:
        case BT_PLAYER_IN_GROUP:
            hr = m_lpDP4A->GetPlayerFlags( bsCurSel.dpid, &dwFlags );
            break;

        case BT_GROUP:
            hr = m_lpDP4A->GetGroupFlags( bsCurSel.dpid, &dwFlags );
            break;

        default:
            break;
    }

    if (FAILED(hr))
        return hr;

    if (dwFlags & DPPLAYER_LOCAL )
    {
        hr = DP_OK;
    }
    else
    {
        hr = DPERR_ACCESSDENIED;
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc:
//-----------------------------------------------------------------------------
void CGroupTree::GetGroupIcons( DPID dpID, DWORD dwFlags, int* lpnNormal,
                                int* lpnSelected )
{
    if (DPGROUP_STAGINGAREA & dwFlags)
    {
        HRESULT hr;
        LPDPLCONNECTION lp = NULL;
        DWORD           dwSize = 0;
        BOOL            bSessionInProgress = FALSE;

        // if it is a staging area, check to see if the session is in progress.
        hr = m_lpDP4A->GetGroupConnectionSettings( 0, dpID, NULL, &dwSize );
        if (DPERR_BUFFERTOOSMALL == hr )
        {
            lp = (LPDPLCONNECTION) GlobalAllocPtr( GHND, dwSize );

            if (lp)
            {
                hr = m_lpDP4A->GetGroupConnectionSettings( 0, dpID, lp, &dwSize );

                if (!IsEqualGUID(lp->lpSessionDesc->guidInstance, GUID_NULL))
                {
                    // If the server has assigned an instance guid 
                    // to our connection structure, the session 
                    // has already started.

                    bSessionInProgress = TRUE;
                }
                GlobalFreePtr(lp);
            }
        }


        if (DPGROUP_HIDDEN & dwFlags)
        {
            *lpnNormal  = (bSessionInProgress)?IMG_HIDDEN_STAGING_AREA_IN_PROGRESS:IMG_HIDDEN_STAGING_AREA;
            *lpnSelected = *lpnNormal;
        }
        else
        {
            *lpnNormal = (bSessionInProgress)?IMG_STAGING_AREA_IN_PROGRESS:IMG_STAGING_AREA;
            *lpnSelected = *lpnNormal;
        }
    }
    else
    {

        if (DPGROUP_HIDDEN & dwFlags)
        {
            *lpnNormal = IMG_HIDDEN_GROUP;
            *lpnSelected = IMG_HIDDEN_GROUP_SELECTED;
        }
        else
        {
            *lpnNormal = IMG_GROUP;
            *lpnSelected = IMG_GROUP_SELECTED;
        }
    }

}




//-----------------------------------------------------------------------------
// Name: SetGroupOwner()
// Desc:
//-----------------------------------------------------------------------------
VOID CGroupTree::SetGroupOwner( DPID dpidGroup, DPID dpidNewOwner,
                                DPID dpidOldOwner )
{
    HRESULT hr = DPERR_GENERIC;
    DWORD   dwFlags;

    if( dpidOldOwner )
    {
        // Delete the old owner icon from the group
        hr = DeletePlayerFromGroup( dpidGroup, dpidOldOwner );

        // Get the old owner's flags
        hr = m_lpDP4A->GetPlayerFlags( dpidOldOwner, &dwFlags );

        // Add the old owner with the icon of a normal player
        hr = AddPlayerToGroup( dpidGroup, dpidOldOwner, dwFlags );

    }

    // Delete the new owner icon from the group
    hr = DeletePlayerFromGroup( dpidGroup, dpidNewOwner );

    // Get the new owner's flags
    hr = m_lpDP4A->GetPlayerFlags( dpidNewOwner, &dwFlags );

    // Add the new owner icon with the owner overlay
    hr = AddPlayerToGroup( dpidGroup, dpidNewOwner, dwFlags|DPPLAYER_OWNER );
}



