// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __PROPULSIONSUBSYS_H
#define __PROPULSIONSUBSYS_H

#include "QuadcopterSubsys.h"

class Rotor;

class PropulsionSubsystem : public QuadcopterSubsystem {
public:
	PropulsionSubsystem(Quadcopter *qc);
	double getAtmDensity() const;
	double getMaxThrust() const;

	void setDirectMode(bool active);
	void setAutoHeading(bool active);
	void setHeadingCmd(double heading);
	void setCourseCmd(double course);
	void setCourseCmd(double course, double hspd);
	void unsetCourseCmd();
	void setHspdCmd(double hspd);
	void unsetHspdCmd();
	void setVspdCmd(double vspd);
	void unsetVspdCmd();
	void setAltCmd(double alt);
	void unsetAltCmd();

	void clbkPreStep(double simt, double simdt, double mjd);

	enum RotorId {
		ROTOR_FL,
		ROTOR_FR,
		ROTOR_BL,
		ROTOR_BR
	};
	enum ThrottleMode {
		THROTTLE_DIRECT,
		THROTTLE_VSPD
	};
	enum AttitudeMode {
		ATTITUDE_DIRECT,
		ATTITUDE_CMD
	};

protected:
	double TiltAngle() const;
	void HoldActivate(bool active);
	void HoldHspd(double hh_tgt);
	void HoldVspd(double vh_tgt);
	void HoldAlt(double alt_tgt);
	void SetAttitude(double pitch_cmd, double roll_cmd, double &p_diff, double &b_diff);
	void SetYawVelocity(double yaw_cmd, double &y_diff);
	bool SetHeading(double hdg_cmd, double &y_diff);
	void SetCourse(double course_cmd, double tilt_cmd, double &pitch, double &roll);
	void SetHspdVector(double course_cmd, double hspd_cmd, double &pitch, double &roll);

private:
	Rotor *m_rotor[4];
	double m_throttle;
	double m_holdT;
	double m_pHspdT;
	double m_pvh;
	double m_phh;
	double m_pvx, m_pvz;
	double m_headingCmd;
	double m_courseCmd;
	double m_hspdCmd;
	double m_vspdCmd;
	double m_altCmd;
	double m_tiltTgt;
	bool m_headingActive;
	bool m_courseActive;
	bool m_hspdActive;
	bool m_vspdActive;
	bool m_altActive;
	bool m_holdActive;
	bool m_autoHeading;
	ThrottleMode m_throttleMode;
	AttitudeMode m_attitudeMode;
};

#endif // !__PROPULSIONSUBSYS_H
