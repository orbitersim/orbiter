// ==============================================================
// Defines the AnimationState class.
// Tracks the state of a single vessel animation (or "door") across frames; used for default sounds.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

class AnimationState
{
public:
    enum class StateType
    {
        Unknown,    // not set yet
        Idle,       // no change since previous frame, or no animation found with the supplied animation ID (this is not an error)
        Opening,    // doorProc just moved from 0.0 up
        Open,       // doorProc just reached 1.0
        Moving,     // doorProc is changing and is between 0..1.0
        Closing,    // doorProc just moved from 1.0 down
        Closed      // doorProc just reached 0.0
    };

    // Constructor
    AnimationState(const StateType state, const double proc) :
        State(state), Proc(proc)
    {
    }

    // NOTE: if you add any additional data fields, ensure that this class can be copied correctly by value.
    StateType State;
    double Proc;       // 0..1.0; -1 = not set yet

    // display as a string for logging purposes
    const char *ToStr()
    {
        switch (State)
        {
        case StateType::Unknown:
            return "Unknown";

        case StateType::Idle:
            return "Idle";

        case StateType::Opening:
            return "Opening";

        case StateType::Open:
            return "Open";

        case StateType::Moving:
            return "Moving";
            
        case StateType::Closing:
            return "Closing";

        case StateType::Closed:
            return "Closed";
            
        default:
            // Invalid enum value specified in config file
            return "[ERROR: Invalid State value]";
        }
    }
};
