Purpose of main classes in TransX
----------------------------------

The actual MFD class in TransX doesn't do much - mostly because Orbiter insists on deleting it and recreating it without warning. It therefore does little other than pass on the messages it receives to its viewstate.

Viewstate is the part of the code that holds information relevant to a particular open window to TransX. There can of course be several of these, all looking at different parts of the flight, and looking at different variable sets.

transxstate is an important one. This is where the collection of stages of a flight is housed and taken care of. New stages are created, and old stages deleted here. Each stage is looked after in basefunction, of which more later. At present there is only ever one of these - this will change in future versions, where there will be one per vessel.

Now is a good time to take a look at some other supporting stuff.

orbitelements is a class that encapsulates the state of an orbit. It can be created either via two handles, or through figures and a central body. Once created, you can use the numerous other get functions to query the properties of an orbit.

intercept is the class that calculates, in an iterative way, your closest approach to a target.

mfdvariable is the base class of a whole set of classes which define the behaviour of the user-alterable variables (the ones you can select using the buttons and tweak in various ways). The subclasses of mfdvariable define the different types of variable to alter - angles, simple numbers, target selectors - you name it. This hierarchy can be extended quite easily, so new types of variables are easy to define. The subclasses are found in the mfdvartypes file.

mfdvarhandler is the class that encapsulates the selection of mfdvariables, and the looking after them. It was written when I was fairly new to classes, and probably could be done better - but it works and isn't too bad.

The second class hierarchy is based on mfdfunction. mfdfunction looks after the business of scheduling a job into a timestep. So any class based on it can be placed into a queue of jobs to execute. This ability is used for a number of purposes - the main one being a background task that ensures that the computations for each stage are executed regularly, so that changes filter through from one stage to another.

mapfunction fits in this hierarchy, and is a solar system map. The map is built with background calls at startup. It is essential to TransX, which therefore displays a startup message until the map has been completely built. This map allows TransX to decide which object a moon belongs to, and thus build stage views correctly.

TransXfunction is another member of the mfdfunction hierarchy, and holds a set of abilities currently only used by basefunctions. All basefunctions are also TransXfunctions.

basefunction does the processing shared by all types of stage, irrespective of the plan type. It looks after the display of some views. It also looks after the business of obtaining information from preceding and following stages as required. It's got rather large - future versions may break this class up into smaller parts if the problem gets much worse. Every basefunction may have a planfunction.

planfunctions (of which there are several types) are designed to plug into basefunction to give extra specific abilities and user variables to a stage. This is how the plan selection process works. Future versions will extend TransX by adding new plan classes at this point.

globals holds things that aren't classes - some vector functions, the opctimestep process - the orbiter callbacks and TransX's response to them.

parser is a simple class designed to help with the processing of scenario files as they are loaded into basefunctions and plans.

