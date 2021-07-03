//
// MLGame.cpp
//
// Game logic for MusicLines
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
#include "MLRender.h"
#include "MLMusic.h"
#include "Resource.h"
#include "MLAI.h"
#include "MLLocalPlayer.h"

#define PLAY(x,p) \
    { if (theMusic) theMusic->Play((x),(p)); }

// Arena size for 1 or 2 player local game
//
const int ArenaX = 40;
const int ArenaY = 28;

// Game::Game
//
Game::Game(
    HWND hWnd,
    int iDifficulty,
    PlayerType Types[2])
{
    m_hWnd = hWnd;

    m_Arena = new Tile[ArenaX * ArenaY];

    ZeroMemory(m_Players, sizeof(m_Players));

    m_nHumanPlayers = 0;
    for (int PlayerNo = 0; PlayerNo < 2; PlayerNo++)
    {
        if (Types[PlayerNo] == PlayerHuman)
        {
            ++m_nHumanPlayers;
            m_Players[PlayerNo] = new LocalPlayer(PlayerNo, this);
        }
        else
        {
            m_Players[PlayerNo] = new AIPlayer(PlayerNo, this, iDifficulty);
        }
    }

    m_nPlayers = 2;
}

// Game::~Game
//
Game::~Game()
{
    delete[] m_Arena;
    delete m_Players[0];
    delete m_Players[1];
}

// Game::Initialize
// 
// Load all the art and reset the game to its initial state.
//
BOOL Game::Initialize()
{
    DWORD dwNow = timeGetTime();
    if (!(theGraphicsEngine->BeginLevelLoad()) ||
        !(theGraphicsEngine->LoadPlayerTiles(0, MAKEINTRESOURCE(IDB_PLAYER0))) ||
        !(theGraphicsEngine->LoadPlayerTiles(1, MAKEINTRESOURCE(IDB_PLAYER1))) ||
        !(theGraphicsEngine->LoadPlayerScoreBitmap(0, MAKEINTRESOURCE(IDB_SCORE0))) ||
        !(theGraphicsEngine->LoadPlayerScoreBitmap(1, MAKEINTRESOURCE(IDB_SCORE1))) ||
        !(theGraphicsEngine->LoadBackdrop(MAKEINTRESOURCE(IDB_BACKDROP))) ||
        !(theGraphicsEngine->EndLevelLoad()))
    {
        return FALSE;
    }       
    dwNow = timeGetTime() - dwNow;
    Trace(0, "Art load: %u ms", dwNow);

    Reset();

    return TRUE;
}

// Game::RenderFrame
//
// Check meta actions to see if the game state must be changed (pause, quit, etc.)
// If the game state is running and it is time to make a move, process moves
// from all players. 
// Call the graphics engine to display the frame.
//
void Game::RenderFrame(BOOL fMakeMove)
{
#ifdef _DEBUG
    static DWORD msLast = 0;
    DWORD msTop = timeGetTime();
   
    DWORD msSinceLast;
    if (msLast)
        msSinceLast = msTop - msLast;
    else
        msSinceLast = 0;

    msLast = msTop;

    if (fMakeMove)
    {
        if (m_dwLastMoveTime)
        {
            Trace(0, "%lu ms since last move", msTop - m_dwLastMoveTime);
        }
        m_dwLastMoveTime = msTop;
    }
#endif

    if (m_GameState == GameRestartWait)
    {
        if (!m_nHumanPlayers)
        {
            // All computer players. Go ahead and start.
            //
            m_GameState = GameRunning;
            PLAY(Music::Up, 0);
        }
        else if (theInput->KeyPressed())
        {
            // One of the humans pressed a key.
            //
            m_GameState = GameRunning;
            PLAY(Music::Up, 0);
        }
    }
    // Get and process meta actions
    //
    Input::GameAction Action;

    if (!theInput->GetMetaAction(&Action))
    {
        Action = Input::None;
    }

    if (Action == Input::MetaQuit)
    {
        Trace(0, "Got quit message");
        PostMessage(m_hWnd, WM_CLOSE, 0, 0);
        return;
    } 

    if (Action == Input::MetaPause)
    {
        Trace(0, "Got pause key");
        switch (m_GameState)
        {
            case GamePaused:  
                m_GameState = GameRunning; 
                break;

            case GameRunning: 
                m_GameState = GamePaused; 
                break;

            case GameDead:
                Reset();
                break;

            default:
                break;
        }
    }

#ifdef _DEBUG
    if (Action == Input::MetaRenderTest)
    {
        switch (m_GameState)
        {
        case GameRenderTest:
            Reset();
            m_GameState = GameRunning;
            break;

        default:
            SetupRenderTest();
            m_GameState = GameRenderTest;
            break;
        }
    }
#endif

    // Process player moves
    //
    if (m_GameState == GameRunning)
    {
        if (fMakeMove)
        {
            BOOL fCheckDead = FALSE;

            // Make all moves first, then check death, so no player has an advantage
            //
            int player;

            for (player = 0; player < m_nPlayers; player++)
            {
                if (m_Players[player]->IsDead())
                {
                    continue;
                }

                m_Players[player]->MakeMove();
            }

            for (player = 0; player < m_nPlayers; player++)
            {
                if (m_Players[player]->IsDead())
                {
                    continue;
                }

                ArenaPt ptPlayerPos = m_Players[player]->GetPos();
                Tile *PlayerPos = Arena(ptPlayerPos);

                if (!m_Players[player]->HasMoved())
                {
                    // NOTE: Doesn't check for the case where the the other player ran over the not-moving
                    // head
                    continue;
                }

                if (m_nMaxLength < ++m_Players[player]->Length())
                {
                    m_nMaxLength = m_Players[player]->Length();
                }

                ArenaPt ptPlayerLastPos = m_Players[player]->GetLastPos();
                Tile *PlayerLastPos = Arena(ptPlayerLastPos);

                PlayerLastPos->SetContents(Tile::PlayerTail);
                PlayerLastPos->SetPlayerNo(player);

                assert(m_nPlayers == 2);
                    
                if (PlayerPos->GetContents() == Tile::PlayerTail ||
                    PlayerPos->GetContents() == Tile::DeadPlayer)
                {
                    // Oops! Ran into someone
                    //
                    m_Players[player]->SetDead();
                    --m_nAlive;
                    fCheckDead = TRUE;
                    theGraphicsEngine->Shake();

                    assert(m_nMaxLength >= m_Players[player]->Length());

                    if (m_Players[player]->Length() == m_nMaxLength)
                    {
                        // This player died, but other player has not won yet
                        PLAY( Music::Death, player );
                    }
                    else
                    {
                        // This player died, other player won
                        PLAY( Music::Win, player );
                    }

                    BOOL fWon = TRUE;
                    int thisLength = m_Players[player]->Length();
                    for (int otherPlayer = 0; otherPlayer < m_nPlayers && fWon; otherPlayer++)
                    {
                        if (otherPlayer != player && 
                            m_Players[otherPlayer]->Length() > thisLength)
                        {
                            // Someone else already won.
                            //
                            fWon = FALSE;
                        }
                    }

                    
                }
                else
                {
                    PlayerPos->SetContents(Tile::PlayerHead);
                    PlayerPos->SetPlayerNo(player);
                }
            }

            if (m_nAlive == 1 && 
                m_Players[0]->Length() == m_Players[1]->Length())
            {                
                // Living player has just caught up with dead player
                //
				PLAY( Music::Win, m_Players[0]->IsDead() ? 1 : 0 );
            }

			if (fCheckDead)
            {
                Tile *thisTile = Arena();
                Tile *endTile = thisTile + ArenaX * ArenaY;

                for (; thisTile != endTile; thisTile++)
                {
                    // Note: Because of the order of evaluation above, a dead player will never
                    // have a head.
                    //
                    if (thisTile->GetContents() == Tile::PlayerTail &&
                        m_Players[thisTile->GetPlayerNo()]->IsDead())
                    {
                        thisTile->SetContents(Tile::DeadPlayer);
                    }
                }

                if (m_nAlive == 0)
                {
                    UpdateScore();
                    PLAY(Music::EndLevel, 0);
                    m_GameState = GameDead;
                }
            }
        }
    }

    theGraphicsEngine->RenderFrame();
}

// Game::UpdateScore
//
// Everyone is dead. Look for the maximum length and increment that player's score. 
// If there is a tie, no-one wins.
//
void Game::UpdateScore()
{
    if (m_Players[0]->Length() == m_Players[1]->Length())
    {
        // Tie game, no-one wins
        //
        return;
    }

    if (m_Players[0]->Length() > m_Players[1]->Length())
    {
        ++m_Players[0]->Score();
        theGraphicsEngine->SetPlayerScore(0, m_Players[0]->Score());
    }
    else
    {
        ++m_Players[1]->Score();
        theGraphicsEngine->SetPlayerScore(1, m_Players[1]->Score());
    }
}

// Game::Reset
//
// Clear the arena and players to their start-of-game state.
//
void Game::Reset()
{
    Tile *pTile = m_Arena;

#ifdef _DEBUG
    m_dwLastMoveTime = 0;
#endif

    for (int idx = 0; idx < ArenaX * ArenaY; idx++, pTile++)
    {
        pTile->SetContents(Tile::Empty);
    }

    m_Arena[15*40 + 10].SetContents(Tile::PlayerHead);
    m_Arena[15*40 + 10].SetPlayerNo(0);
    m_Arena[15*40 + 30].SetContents(Tile::PlayerHead);
    m_Arena[15*40 + 30].SetPlayerNo(1);

    m_Players[0]->Reset(ArenaPt(10, 15));
    m_Players[0]->ClrDead();
    m_Players[0]->Length() = 0;
    m_Players[1]->Reset(ArenaPt(30, 15));
    m_Players[1]->ClrDead();
    m_Players[1]->Length() = 0;

    m_nAlive = m_nPlayers;
    m_GameState = GameRestartWait;
    m_nMaxLength = 0;

    // About to start a new level
    //
    PLAY(Music::StartLevel, 0);
    theInput->Reset();
}

#ifdef _DEBUG
// Game::SetupRenderTest
//
// Fill the whole arena with a bitmap to get a feel for max rendering time.
//
void Game::SetupRenderTest()
{
    Tile *pTile = m_Arena;

    for (int idx = 0; idx < ArenaX * ArenaY; idx++, pTile++)
    {
        pTile->SetContents(Tile::PlayerTail);
        pTile->SetPlayerNo(0);
    }
}
#endif

// Player::Player
//
Player::Player(int PlayerNo, Game *pGame)
{
    m_PlayerNo = PlayerNo;
    m_Score = 0; 
}

// Player::MakeMove
//
//
// MoveTable must be in same order as enum Player::Move
//
static struct dMove 
{
    int dx;
    int dy;
    Music::PlayEvent event;
} MoveTable[] = 
{
    {  0, -1, Music::Up     },     // Up
    {  0,  1, Music::Down   },     // Down
    { -1,  0, Music::Left   },     // Left
    {  1,  0, Music::Right  },     // Right
};
const int nMoveTable = sizeof(MoveTable) / sizeof(MoveTable[0]);

void Player::MakeMove()
{
    m_moveLast = m_moveCurr;
    m_moveCurr = ComputeMove();

    m_HasMoved = (m_moveCurr != None);

    if (m_HasMoved)
    {
        assert(m_moveCurr >= 0);
        assert(m_moveCurr < nMoveTable);

        if (m_moveCurr != m_moveLast)
        {
            PLAY(MoveTable[m_moveCurr].event, m_PlayerNo);
        }
        
        SetLastPos();

        m_Curr.Offset(MoveTable[m_moveCurr].dx, MoveTable[m_moveCurr].dy);
    }    
}

void Player::Reset(ArenaPt pt)
{
    m_Curr = pt;
    m_Last = pt;

    m_Dead = FALSE; 
    m_Length = 0; 
    m_moveLast = m_moveCurr = None;
}

