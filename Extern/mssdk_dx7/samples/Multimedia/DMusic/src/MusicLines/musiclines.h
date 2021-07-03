//
// MusicLines.h
//
// Global definitions for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MusicLines_
#define _MusicLines_

// Arbitrary maximum number of players for data structures which are statically allocated
//
#define MAX_PLAYERS     2

// Frame time and timer resolution in milliseconds
//
#define TIMER_RESOLUTION    1
#define GAME_FRAME_TIME     33

// The main data structure for MusicLines is an array of tile structures which represents the playing field. 
//
class Tile
{
public:
    Tile() 
    {  m_Contents = Empty; m_PlayerNo = 0; m_Dirty = 1; }

    enum TileContents
    {
        Empty           = 1,
        PlayerHead      = 0,            // These three are the offsets of the tile in the tile bitmap
        PlayerTail      = 16,
        DeadPlayer      = 32
    };

    int  inline IsDirty() { return m_Dirty; }
    void inline SetDirty() { m_Dirty = 1; }
    void inline ClrDirty() { m_Dirty = 0; }

    TileContents inline GetContents() { return m_Contents; }
    void inline SetContents(TileContents Contents) { m_Contents = Contents; }

    int inline GetPlayerNo() { return m_PlayerNo; }
    void inline SetPlayerNo(int PlayerNo) { m_PlayerNo = PlayerNo; }

private:
    TileContents        m_Contents;
    int                 m_PlayerNo;     // If PlayerHead or PlayerTail
    int                 m_Dirty : 1;
};

// The size of the playing field. In later versions this might be variable.
//
int inline PlayfieldXTiles() { return 40; }
int inline PlayfieldYTiles() { return 30; }

// The global array of tiles
//
extern Tile theTiles[];

#endif // _MusicLines_

