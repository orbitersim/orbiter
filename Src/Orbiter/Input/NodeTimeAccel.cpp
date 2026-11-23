#include "NodeTimeAccel.h"
#include "Orbiter.h"

extern Orbiter *g_pOrbiter;

void NodeTimeAccel::RegisterPins()
{
    RegisterPinWithDefault("Select", select, true);
    RegisterPin("Toggle pause", pause);
    RegisterPin("Increment time warp", inc);
    RegisterPin("Decrement time warp", dec);
    RegisterPin("x0.1",   x01);
    RegisterPin("x1",     x1);
    RegisterPin("x10",    x10);
    RegisterPin("x100",   x100);
    RegisterPin("x1000",  x1000);
    RegisterPin("x10000", x10000);
}

void NodeTimeAccel::UpdateOutputs(bool dryrun)
{
    if(!select || dryrun) return;

    if(pause) g_pOrbiter->TogglePause();
    if(inc) g_pOrbiter->IncWarpFactor();
    if(dec) g_pOrbiter->DecWarpFactor();

    if(x01)    g_pOrbiter->SetWarpFactor (0.1);
    if(x1)     g_pOrbiter->SetWarpFactor (1);
    if(x10)    g_pOrbiter->SetWarpFactor (10);
    if(x100)   g_pOrbiter->SetWarpFactor (100);
    if(x1000)  g_pOrbiter->SetWarpFactor (1000);
    if(x10000) g_pOrbiter->SetWarpFactor (10000);
}
