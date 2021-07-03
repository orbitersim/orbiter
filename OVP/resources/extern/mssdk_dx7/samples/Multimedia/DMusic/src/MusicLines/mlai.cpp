//
// MLAI.cpp
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
#include "MLAI.h"

// AIPlayer::Reset
//
void AIPlayer::Reset(
    ArenaPt pt)
{
    m_SuicideCountdown = -1;

    Player::Reset(pt);
}

// AIPlayer::MakeMove
//
#define AI_LEFT     0x0001
#define AI_RIGHT    0x0002
#define AI_UP       0x0004
#define AI_DOWN     0x0008

struct AIDirections
{
    int             dx;
    int             dy;
    unsigned        flag;
    Player::Move    move;
} AIDirectionList[] =
{
    { -1,  0, AI_LEFT,      Player::Left },
    {  1,  0, AI_RIGHT,     Player::Right },
    {  0, -1, AI_UP,        Player::Up },
    {  0,  1, AI_DOWN,      Player::Down }
};

static const AIDirections *AIDirectionListEnd = 
    AIDirectionList + sizeof(AIDirectionList) / sizeof(AIDirectionList[0]);

Player::Move AIPlayer::ComputeMove()
{
    unsigned PossibleMoves = (AI_LEFT | AI_RIGHT | AI_UP | AI_DOWN);
    unsigned PossibleMoveCount = 4;
    Tile *pTile;
    AIDirections *pDirs;

    ArenaPt ptAI = GetPos();
    ArenaPt ptNew;
        
    // Flight: If there's a wall in any direction, don't move that direction
    //
    for (pDirs = AIDirectionList; pDirs != AIDirectionListEnd; pDirs++)
    {
        ptNew = ArenaPt(ptAI, pDirs->dx, pDirs->dy);
        pTile = m_Game->Arena(ptNew);

        if (pTile->GetContents() != Tile::Empty && 
            pTile->GetContents() != Tile::PlayerHead)
        {
            PossibleMoves &= ~pDirs->flag;
            --PossibleMoveCount;
        }
    }

    if (m_SuicideCountdown != -1)
    {
        if (m_SuicideCountdown)
        {
            --m_SuicideCountdown;
        }

        if (PossibleMoveCount < 4 && !m_SuicideCountdown)
        {
            PossibleMoves ^= ~0;
            PossibleMoveCount = 4 - PossibleMoveCount;
        }
    }
    else if (m_Game->LivePlayers() == 1)
    {
        // Opponent is dead. Since we started moving on the first possible move, give
        // ourselves a countdown and then die soon so we don't hold up the game.
        //
        m_SuicideCountdown = 5;
    }
    else
    {
        // Fight: If we can move multiple directions without moving, go for the opponent's
        // head.
        //
        if (PossibleMoveCount > 1)
        {
            // Every now and then do something random so the player has a chance.
            //
            if ((rand() % m_Difficulty) == 1)
            {
                int MoveIndex = rand() % PossibleMoveCount;
                
                for (pDirs = AIDirectionList; pDirs != AIDirectionListEnd; pDirs++)
                {
                    if (!(PossibleMoves & pDirs->flag))
                    {
                        continue;
                    }

                    if (!MoveIndex)
                    {
                        PossibleMoves = pDirs->flag;
                        PossibleMoveCount = 1;
                        break;
                    }
                    
                    --MoveIndex;
                }
            }
            else
            {
                // Go for the throat... er head.
                //
                ArenaPt ptOpponent = m_Game->PlayerLastPos(1 - m_PlayerNo);

                int TheMove = 0;
                int MinDist = INT_MAX;

                for (pDirs = AIDirectionList; pDirs != AIDirectionListEnd; pDirs++)
                {
                    if (!(PossibleMoves & pDirs->flag))
                    {
                        continue;
                    }

                    ptNew = ArenaPt(ptAI, pDirs->dx, pDirs->dy);

                    int CurrDist = Dist(ptNew, ptOpponent);

                    if (CurrDist < MinDist)
                    {
                        MinDist = CurrDist;
                        TheMove = pDirs->flag;           
                    }
                }

                assert(TheMove);

                PossibleMoves = TheMove;
                PossibleMoveCount = 1;
            }
        }
    }

    if (PossibleMoveCount)
    {        
        for (pDirs = AIDirectionList; pDirs != AIDirectionListEnd; pDirs++)
        {
            if (pDirs->flag & PossibleMoves)
            {
                break;
            }
        }

        assert(pDirs != AIDirectionListEnd);

        return pDirs->move;
    }

    
    // We're about to die, any direction is fine. We're unfortunately not allowed to stand still.
    //

    return Player::Up;
}

int AIPlayer::Dist(
    ArenaPt pt0,
    ArenaPt pt1)
{
    int dx = pt1.X() - pt0.X();
    int dy = pt1.Y() - pt0.Y();

    if (dx < 0) dx = -dx;
    if (dy < 0) dy = -dy;

    if (ArenaX - dx < dx) dx = ArenaX - dx;
    if (ArenaY - dy < dy) dy = ArenaY - dy;

    return dx*dx + dy*dy;
}
