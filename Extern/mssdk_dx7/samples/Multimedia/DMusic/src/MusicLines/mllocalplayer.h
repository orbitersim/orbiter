//
// MLLocalPlayer.h
//
// Local player implementation
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef _MLLocalPlayer_
#define _MLLocalPlayer_

// LocalPlayer is a Player controlled by a local controller.
//
class LocalPlayer : public Player
{
public:
    LocalPlayer(int PlayerNo, Game *pGame) :
        m_PlayerNo(PlayerNo), Player(PlayerNo, pGame)
    {}

    void Reset(ArenaPt pt);
    Move ComputeMove();

private:
    int                 m_PlayerNo;         // For class Input mapping of controls -> player
};

#endif // _MLLocalPlayer_


