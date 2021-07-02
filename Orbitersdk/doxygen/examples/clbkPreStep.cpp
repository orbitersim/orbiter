class MyVessel: public VESSEL2 {
public:
	...
	clbkPreStep (double simt, double simdt, double mjd);
	...
};

void MyVessel::clbkPreStep (double simt, double simdt, double mjd)
{
	double F = mass * dv/simdt;
	AddForce(_V(0,0,F), _V(0,0,0));
}