#include "NodeRCSMode.h"
#include "Vessel.h"

extern Vessel *g_focusobj;

void NodeRCSMode::RegisterPins()
{
    RegisterPinWithDefault("Select", select, true);
    RegisterPin("Translation", translation);
    RegisterPin("Rotation", rotation);
    RegisterPin("Off", off);
    RegisterPin("Toggle", toggle);
}

void NodeRCSMode::UpdateOutputs(bool dryrun)
{
    if(!select || dryrun) return;

    if(translation) g_focusobj->SetAttMode(2);
    if(rotation)    g_focusobj->SetAttMode(1);
    if(off)         g_focusobj->SetAttMode(0);
    if(toggle)      g_focusobj->ToggleAttMode();
}
