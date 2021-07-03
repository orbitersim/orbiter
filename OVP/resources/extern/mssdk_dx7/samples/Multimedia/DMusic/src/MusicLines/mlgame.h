//
// MLGame.h
//
// Game logic for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
 
#ifndef _MLGame_
#define _MLGame_

extern const int ArenaX;
extern const int ArenaY;

class ArenaPt
{
public:
    ArenaPt(int x = 0, int y = 0) :
        m_x(x), m_y(y)
    {}

    ArenaPt(const ArenaPt &pt, int dx, int dy)
    {
        m_x = pt.X();
        m_y = pt.Y();
        m_x = IncX(dx);
        m_y = IncY(dy);
    }

    inline int X() const                { return m_x; }
    inline int Y() const                { return m_y; }

    inline int IncX(int dx = 1) const   { return ((m_x + dx + ArenaX) % ArenaX); }
    inline int IncY(int dy = 1) const   { return ((m_y + dy + ArenaY) % ArenaY); }

    inline void Set(int x, int y)       { m_x = x; m_y = y; }
    inline void Offset(int dx, int dy)  { m_x = IncX(dx); m_y  = IncY(dy); }

    int m_x;
    int m_y;
};

// Abstraction of Player allows implementation of different player types: AI, human, remote, etc.
//
class Game;

class Player
{
public:
    enum Move
    {
        None    = -1,       // Only valid from start until first move
        Up      = 0,
        Down    = 1,
        Left    = 2,
        Right   = 3
    };

    Player(int PlayerNo, Game *pGame);
    virtual ~Player() {}
    virtual void Reset(ArenaPt pt);

    void MakeMove();
        
    inline ArenaPt GetPos()                 { return m_Curr; }
    inline void SetLastPos()                { m_Last = m_Curr; }
    inline ArenaPt GetLastPos()             { return m_Last; }

    inline BOOL HasMoved()                  { return m_HasMoved; }

    inline void ClrDead()                   { m_Dead = FALSE; }
    inline void SetDead()                   { m_Dead = TRUE;  }
    inline BOOL IsDead()                    { return m_Dead;  }


    inline int &Score()                     { return m_Score; }
    inline int &Length()                    { return m_Length; }

    inline Move LastMove()                  { return m_moveLast; }

private:
    int     m_PlayerNo;
    Move    m_moveLast;
    Move    m_moveCurr;
    BOOL    m_Dead;
    BOOL    m_HasMoved;
    ArenaPt m_Last;
    ArenaPt m_Curr;
    int     m_Score;
    int     m_Length;

protected:
    virtual Move ComputeMove() = 0;
};

class Game
{
public:
    enum PlayerType
    {
        PlayerHuman,
        PlayerComputer
    };
    Game(HWND hWnd, int iDifficulty, PlayerType PlayerTypes[2]);
    ~Game();

    BOOL Initialize();

    inline Tile *Arena()                    { return m_Arena; }
    inline Tile *Arena(const ArenaPt &pt)   { return &m_Arena[pt.X() + pt.Y() * ArenaX]; }

    inline int LivePlayers()                { return m_nAlive; }

    // RenderFrame is the main entry into the game. It manages game setup, play, and scoring.
    //
    void RenderFrame(BOOL fMakeMove);

    // These are for the AI
    //
    inline Player::Move PlayerLastMove(int PlayerNo) 
                                            { return m_Players[PlayerNo]->LastMove(); }

    inline ArenaPt PlayerLastPos(int PlayerNo)
                                            { return m_Players[PlayerNo]->GetLastPos(); }

private:
    enum GameState
    {
        GameRunning,
        GamePaused,
        GameDead,
        GameRestartWait,
#ifdef _DEBUG
        GameRenderTest,
#endif
    };
    
    HWND m_hWnd;                            // Main game window to destroy on quit
    GameState m_GameState;                  // What's game as a whole doing right now?

    Tile *m_Arena;                          // -> Array of tiles representing arena

    int m_nPlayers;                         // Players in the game this time
    int m_nHumanPlayers;
    int m_nAlive;                           // How many are still alive?
    int m_nMaxLength;                       // Best length so far
    Player *m_Players[MAX_PLAYERS];         // -> objects controlling them

#ifdef _DEBUG
    DWORD m_dwLastMoveTime;
#endif 

private:
#ifdef _DEBUG
    void SetupRenderTest();
#endif
    void UpdateScore();
    void Reset();
};

extern Game *theGame;

#endif // _MLGame_
