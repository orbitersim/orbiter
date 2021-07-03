//
// MLAI.cpp
//
// Computer player intelligence
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef _MLAI_
#define _MLAI_

// AIPlayer is the computer player
//
class AIPlayer : public Player
{
public:
    AIPlayer(int PlayerNo, Game *pGame, int iDifficulty) :
        m_PlayerNo(PlayerNo),
        m_Game(pGame),
        m_Difficulty(iDifficulty),
		Player(PlayerNo, pGame)
    {}

    void Reset(ArenaPt pt);
    Move ComputeMove();

private:    
    int                 m_PlayerNo;         // What player are we?
    Game               *m_Game;             // Back pointer to game so AI can see arena
    int                 m_SuicideCountdown; // Frames till we kill ourselves
    int                 m_Difficulty;       // Difficulty: How often are we random?

private:
	int Dist(ArenaPt pt0, ArenaPt pt1);
	
};   

#endif // _MLAI_


