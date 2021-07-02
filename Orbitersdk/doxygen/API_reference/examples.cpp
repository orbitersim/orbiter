VESSEL2::VESSEL2 (OBJHANDLE hVessel, int fmodel) {}
/// \example VESSEL2.cpp
/// Example for constructing and destroying an overloaded VESSEL2 instance
/// during the instance initialisation of a vessel module.

void VESSEL2::clbkLoadStateEx (FILEHANDLE scn, void *status) {}
/// \example clbkLoadStateEx.cpp
/// Example of an overloaded VESSEL2::clbkLoadStateEx method.

void VESSEL2::clbkSetStateEx (const void *status) {}
/// \example clbkSetStateEx.cpp
/// Example of an overloaded VESSEL2::clbkSetStateEx method.

void VESSEL2::clbkPreStep (double SimT, double SimDT, double mjd) {}
/// \example clbkPreStep.cpp
/// Example of an overloaded VESSEL2::clbkPreStep method.