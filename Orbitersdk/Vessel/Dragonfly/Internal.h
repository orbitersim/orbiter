
#ifndef __INTERNAL_H__
#define __INTERNAL_H__


#include "panel.h"
#include "hsystems.h"
#include "esystems.h"
#include "orbitersdk.h"

class ShipInternal
{ public:
    VESSEL* parent;
    Valve* Valves[25];
	Tank* Tanks[20];
	FCell *FC[3];
	Battery *BT[5];
	Socket *Sock[10];
	Manifold *Man[5];
    DCbus *DC[10];
	ACbus *AC[3];
	Heater* HT[6];
	Fan  *Fans[2];
	inst_MFD *MFDS[3];
	Docker* Dk[1];
	Clock *Clk;
	CW *cw[15];
	Switch *Nav_mode_switch;
	Switch *Vern_mode_switch;
    Switch *Intr_mode_switch;
	char dig[10];
	int mjd_d;
	H_system H_systems;
	E_system E_systems;
	Panel PanelList[6];
//	float Flow;
	float Cabin_temp;
	float Cabin_press;
	float Cabin_dp;
    float Dock_temp;
	float Dock_press;
	float Dock_dp;
	float Fan_dp;
	ShipInternal();
	~ShipInternal();
	void Init(VESSEL *vessel);
	void MakePanels(VESSEL *vessel);
	void Save(FILEHANDLE scn);
	void Load(FILEHANDLE scn,void *def_vs);
	void Refresh(double dt);
};

  
#endif 