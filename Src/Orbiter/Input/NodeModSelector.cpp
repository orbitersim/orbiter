#include "NodeModSelector.h"

void NodeModSelector::RegisterPins()
{
    RegisterPin("Mod 1", A);
    RegisterPin("Mod 2", B);
    RegisterPin("Sel 1", O1);
    RegisterPin("Sel 2", O2);
    RegisterPin("Sel 3", O3);
    RegisterPin("Sel 4", O4);
}

void NodeModSelector::UpdateOutputs(bool dryrun)
{
    O1 = !A && !B;
    O2 =  A && !B;
    O3 = !A &&  B;
    O4 =  A &&  B;
}
