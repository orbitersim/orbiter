class MyVessel: public VESSEL2 {  // or: VESSEL
public:
	MyVessel (OBJHANDLE hvessel, int flightmodel = 1);
    ...
};

MyVessel::MyVessel (OBJHANDLE hvessel, int flightmodel)
: VESSEL2 (hvessel, flightmodel)
{
	...
}

DLLCLBK VESSEL2 *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
    return new MyVessel (hvessel, flightmodel);
}

DLLCLBK void ovcExit (VESSEL2 *vessel)
{
    delete (MyVessel*)vessel;
}
