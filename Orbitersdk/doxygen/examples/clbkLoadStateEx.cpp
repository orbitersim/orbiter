class MyVessel: public VESSEL2 {
public:
	...
	clbkLoadStateEx (FILEHANDLE scn, void *status);
	...
};

void MyVessel::clbkLoadStateEx (FILEHANDLE scn, void *status)
{
    char *line;
    int my_value;

    while (oapiReadScenario_nextline (scn, line)) {
        if (!strnicmp (line, "my_option", 9)) {  // custom item
            sscanf (line+9, "%d", &my_value);
        } else if (...) { // more items
            ...
        } else {          // anything not recognised is passed on to Orbiter
            ParseScenarioLineEx (line, vs);
        }
    }
}
