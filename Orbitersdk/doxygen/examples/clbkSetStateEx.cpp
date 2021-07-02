class MyVessel: public VESSEL2 {
public:
	...
	clbkSetStateEx (const void *status);
	...
};

void MyVessel::clbkSetStateEx (const void *status)
{
    // specialised vessel initialisations
    // ...

    // default initialisation:
    DefSetStateEx (status);
}
