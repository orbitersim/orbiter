//
// MLLocalPlayer.cpp
//
// Computer player intelligence
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <mmsystem.h>
#include <limits.h>
#include <assert.h>
#include <stdlib.h>

#include "Debug.h"

#include "MusicLines.h"
#include "MLGame.h"
#include "MLInput.h"
#include "MLLocalPlayer.h"
#include "MLMusic.h" // Does not belong here!!!

#define PLAY(x,p) \
    { if (theMusic) theMusic->Play((x),(p)); }

// LocalPlayer::Reset
// 
// Reset the state of the local player to start-of-game.
//
void LocalPlayer::Reset(
    ArenaPt pt)
{
    Player::Reset(pt);
}

// LocalPlayer::ComputeMove
//
// Move in the direction indicated by the human controlling this player.
//
Player::Move LocalPlayer::ComputeMove()
{
    Input::GameAction NewAction;

    if (theInput->GetPlayerAction(m_PlayerNo, &NewAction))
    {
        switch (NewAction)
        {
        case Input::PlayerUp:
            if (LastMove() == Player::Down)
                break;
            
            return Player::Up;

        case Input::PlayerDown:
            if (LastMove() == Player::Up)
                break;

            return Player::Down;

        case Input::PlayerLeft:
            if (LastMove() == Player::Right)
                break;

            return Player::Left;

        case Input::PlayerRight:
            if (LastMove() == Player::Left)
                break;

            return Player::Right;
        }
    }

    return LastMove();
}
