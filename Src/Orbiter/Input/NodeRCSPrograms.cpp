#include "NodeRCSPrograms.h"
#include "Vessel.h"

extern Vessel *g_focusobj;

void NodeRCSPrograms::RegisterPins()
{
    RegisterPinWithDefault("Select", select, true);
    RegisterPin("Prograde", prograde);
    RegisterPin("Retrograde", retrograde);
    RegisterPin("Normal +", normal);
    RegisterPin("Normal -", antinormal);
    RegisterPin("Kill Rotation", killrot);
    RegisterPin("Horiz. Level", hlevel);
    RegisterPin("Hold Altitude", holdalt);
    RegisterPin("Off", off);
}

void NodeRCSPrograms::UpdateOutputs(bool dryrun)
{
    if(!select || dryrun) return;

    if(killrot)    g_focusobj->TglNavMode(NAVMODE_KILLROT);
    if(hlevel)     g_focusobj->TglNavMode(NAVMODE_HLEVEL);
    if(prograde)   g_focusobj->TglNavMode(NAVMODE_PROGRADE);
    if(retrograde) g_focusobj->TglNavMode(NAVMODE_RETROGRADE);
    if(normal)     g_focusobj->TglNavMode(NAVMODE_NORMAL);
    if(antinormal) g_focusobj->TglNavMode(NAVMODE_ANTINORMAL);
    if(holdalt)    g_focusobj->TglNavMode(NAVMODE_HOLDALT);
    if(off)        g_focusobj->SetNavMode(0);
}
