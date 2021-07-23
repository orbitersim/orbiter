// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "PropulsionSubsys.h"

// ==============================================================
// Single-rotor implementation
// ==============================================================

class Rotor {
public:
	enum SpinDirection {
		SPIN_LEFT,
		SPIN_RIGHT
	};

	Rotor(const PropulsionSubsystem *ssys, const VECTOR3 &pos, SpinDirection spin);
	void setThrottle(double throttle);
	const VECTOR3 &getPos() const { return m_pos; }
	VECTOR3 getForce() const;

private:
	const PropulsionSubsystem *m_ssys;
	VECTOR3 m_pos;
	VECTOR3 m_liftDir;
	VECTOR3 m_torqueDir;
	SpinDirection m_spin;
	double m_throttle;
};

Rotor::Rotor(const PropulsionSubsystem *ssys, const VECTOR3 &pos, SpinDirection spin)
	: m_ssys(ssys)
	, m_pos(pos)
	, m_spin(spin)
	, m_throttle(0.0)
{
	m_liftDir = _V(0, 1, 0);
	VECTOR3 cgDir = unit(-pos);
	m_torqueDir = crossp(cgDir, m_liftDir);
	if (spin == SPIN_RIGHT)
		m_torqueDir = -m_torqueDir;
}

void Rotor::setThrottle(double throttle)
{
	m_throttle = throttle;
}

VECTOR3 Rotor::getForce() const
{
	// lift component
	const double thrust_max = 8.0;
	double dns = m_ssys->getAtmDensity();
	double lift = dns * m_throttle * thrust_max;
	VECTOR3 liftVec = m_liftDir * lift;

	// torque component
	const double torque_max = 0.5;
	double torque = m_throttle * torque_max;
	VECTOR3 torqueVec = m_torqueDir * torque;

	return liftVec + torqueVec;
}


// ==============================================================
// Rotor propulsion subsystem
// ==============================================================

PropulsionSubsystem::PropulsionSubsystem(Quadcopter *qc)
	: QuadcopterSubsystem(qc)
{
	const double px = 0.4, pz = 0.4, py = 0.15;

	m_rotor[ROTOR_FL] = new Rotor(this, _V(-px, py,  pz), Rotor::SPIN_LEFT);
	m_rotor[ROTOR_FR] = new Rotor(this, _V( px, py,  pz), Rotor::SPIN_RIGHT);
	m_rotor[ROTOR_BL] = new Rotor(this, _V(-px, py, -pz), Rotor::SPIN_RIGHT);
	m_rotor[ROTOR_BR] = new Rotor(this, _V( px, py, -pz), Rotor::SPIN_LEFT);
	m_holdActive = false;
	m_throttle = 0.0;

	m_attitudeMode = ATTITUDE_CMD;
	m_throttleMode = THROTTLE_VSPD;
	m_autoHeading = false;
	m_headingActive = false;
	m_courseActive = false;
	m_hspdActive = false;
	m_vspdActive = false;
	m_altActive = false;
	HoldActivate(true);
}

double PropulsionSubsystem::getAtmDensity() const
{
	return QC()->GetAtmDensity();
}

double PropulsionSubsystem::getMaxThrust() const
{
	return getAtmDensity() * 6.0 * 4.0;
}

void PropulsionSubsystem::clbkPreStep(double simt, double simdt, double mjd)
{
	QuadcopterSubsystem::clbkPreStep(simt, simdt, mjd);

	// throttle/vspd command
	if (m_vspdActive) {
		HoldVspd(m_vspdCmd);
	}
	else if (m_altActive) {
		HoldAlt(m_altCmd);
	}
	else {
		switch (m_throttleMode) {
		case THROTTLE_DIRECT:
			m_throttle = QC()->GetThrusterGroupLevel(THGROUP_MAIN);
			break;
		case THROTTLE_VSPD: {
			// use throttle setting to specify target vertical velocity -5 .. +5 m/s
			double vspd_tgt = (QC()->GetThrusterGroupLevel(THGROUP_MAIN) - 0.5) * 10.0;
			HoldVspd(vspd_tgt);
			}
			break;
		}
	}

	// attitude command
	double dp, db, dy;
	double pitch_cmd = -QC()->GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ROTMODE);
	if (!pitch_cmd) pitch_cmd = QC()->GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ROTMODE);

	double bank_cmd = -QC()->GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ROTMODE);
	if (!bank_cmd) bank_cmd = QC()->GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ROTMODE);

	double yaw_cmd = -QC()->GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ROTMODE);
	if (!yaw_cmd) yaw_cmd = QC()->GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ROTMODE);

	if (m_attitudeMode == ATTITUDE_DIRECT) {
		dp = pitch_cmd * 0.01;
		db = bank_cmd * 0.01;
		dy = yaw_cmd * 0.01;
	}
	else {
		const double PITCH_RANGE = PI*0.25;
		const double ROLL_RANGE = PI*0.25;
		double pitch_tgt, roll_tgt;
		const double YAW_VEL_RANGE = 1.0; // rad/s
		if (m_courseActive) {
			//if (m_hspdActive)
			//	HoldHspd(m_hspdCmd);
			//SetCourse(m_courseCmd, m_tiltTgt, pitch_tgt, roll_tgt);
			SetHspdVector(m_courseCmd, m_hspdCmd, pitch_tgt, roll_tgt);
		}
		else {
			pitch_tgt = pitch_cmd * PITCH_RANGE;
			roll_tgt = bank_cmd * ROLL_RANGE;
		}
		SetAttitude(pitch_tgt, roll_tgt, dp, db);

		double yaw_vel_tgt = yaw_cmd * YAW_VEL_RANGE;
		bool fixedHeading = false;
		if (m_autoHeading) {
			VECTOR3 v;
			QC()->GetHorizonAirspeedVector(v);
			if (length(v) > 5.0) {
				double course = atan2(v.x, v.z);
				SetHeading(course, dy);
				fixedHeading = true;
			}
		}
		else if (m_headingActive) {
			m_headingActive = !SetHeading(m_headingCmd, dy);
			fixedHeading = true;
		}
		if (!fixedHeading) {
			SetYawVelocity(yaw_vel_tgt, dy);
		}
	}

	if (m_throttle) {
		m_rotor[ROTOR_FL]->setThrottle(m_throttle + dp - db - dy);
		m_rotor[ROTOR_FR]->setThrottle(m_throttle + dp + db + dy);
		m_rotor[ROTOR_BL]->setThrottle(m_throttle - dp - db + dy);
		m_rotor[ROTOR_BR]->setThrottle(m_throttle - dp + db - dy);

		for (int i = 0; i < 4; i++) {
			QC()->AddForce(m_rotor[i]->getForce(), m_rotor[i]->getPos());
		}
	}
}

double PropulsionSubsystem::TiltAngle() const
{
	VECTOR3 hn, vn = { 0, 1, 0 };
	QC()->HorizonInvRot(vn, hn); // local horizon normal in vessel frame
	return acos(hn.y);
}

void PropulsionSubsystem::HoldActivate(bool active)
{
	m_holdActive = active;
	if (active) {
		m_holdT = oapiGetSimTime();
		VECTOR3 v;
		QC()->GetHorizonAirspeedVector(v);
		m_pvh = v.y;
	}
}

void PropulsionSubsystem::HoldHspd(double hh_tgt)
{
	const double alpha = 0.001;
	const double beta = 0.001;

	double t = oapiGetSimTime();
	double dt = t - m_pHspdT;
	if (!dt) return;
	m_pHspdT = t;

	VECTOR3 v;
	QC()->GetHorizonAirspeedVector(v);
	double hh = hypot(v.x, v.z);
	double ah = (hh - m_phh) / dt;
	m_phh = hh;

	double dphi = (hh_tgt - hh) * alpha - ah * beta;
	m_tiltTgt += dphi;
	if (m_tiltTgt < 0.0)
		m_tiltTgt = 0.0;
	else if (m_tiltTgt > PI*0.25)
		m_tiltTgt = PI*0.25;

	sprintf(oapiDebugString(), "dphi=%lf", dphi);
}

void PropulsionSubsystem::HoldVspd(double vh_tgt)
{
	double t = oapiGetSimTime();
	double dt = t - m_holdT;
	if (!dt) return;
	m_holdT = t;

	VECTOR3 v;
	QC()->GetHorizonAirspeedVector(v);
	double vh = v.y;
	double ah = (vh - m_pvh) / dt;
	m_pvh = vh;

	double dvh = vh_tgt - vh;
	double a_tgt = dvh;
	double da = a_tgt - ah;
	double a_max = getMaxThrust() / QC()->GetMass();
	double dlvl = da / a_max;
	double dlvl_max = dt;
	if (fabs(dlvl) > dlvl_max)
		dlvl = (dlvl > 0.0 ? dlvl_max : -dlvl_max);
	m_throttle = max(0, min(1.0, m_throttle + dlvl));
}

void PropulsionSubsystem::HoldAlt(double alt_tgt)
{
	double alt = QC()->GetAltitude(ALTMODE_GROUND);
	VECTOR3 v;
	QC()->GetHorizonAirspeedVector(v);
	double vspd = v.y;

	double dalt = alt_tgt - alt;
	double v_tgt = dalt*0.3;
	if (fabs(v_tgt > 5.0))
		v_tgt = (v_tgt > 0.0 ? 5.0 : -5.0);
	HoldVspd(v_tgt);
}

void PropulsionSubsystem::SetAttitude(double p_cmd, double b_cmd, double &p_diff, double &b_diff)
{
	const double alpha = 0.2;
	const double beta = 0.1;

	double p = QC()->GetPitch();
	double b = QC()->GetBank();

	double dp = p - p_cmd;
	double db = b - b_cmd;
	VECTOR3 avel;
	QC()->GetAngularVel(avel);
	double dpv =  avel.x;
	double dbv = -avel.z;

	p_diff = -alpha*dp - beta*dpv;
	b_diff = -alpha*db - beta*dbv;

	if (fabs(p_diff) > 0.2) p_diff = (p_diff > 0.0 ? 0.2 : -0.2);
	if (fabs(b_diff) > 0.2) b_diff = (b_diff > 0.0 ? 0.2 : -0.2);
}

void PropulsionSubsystem::SetYawVelocity(double y_cmd, double &y_diff)
{
	const double alpha = 0.2;

	VECTOR3 avel;
	QC()->GetAngularVel(avel);
	double dyv = avel.y - y_cmd;

	y_diff = -alpha*dyv;
	if (fabs(y_diff) > 0.2) y_diff = (y_diff > 0.0 ? 0.2 : -0.2);
}

void PropulsionSubsystem::setDirectMode(bool active)
{
	m_attitudeMode = (active ? ATTITUDE_DIRECT : ATTITUDE_CMD);
	m_throttleMode = (active ? THROTTLE_DIRECT : THROTTLE_VSPD);
}

void PropulsionSubsystem::setAutoHeading(bool active)
{
	m_autoHeading = active;
}

void PropulsionSubsystem::setHeadingCmd(double heading)
{
	m_autoHeading = false;
	m_headingActive = true;
	m_headingCmd = heading;
}

void PropulsionSubsystem::setCourseCmd(double course)
{
	m_courseActive = true;
	m_courseCmd = course;
}

void PropulsionSubsystem::setCourseCmd(double course, double hspd)
{
	m_courseActive = true;
	m_courseCmd = course;
	m_hspdCmd = hspd;
	m_pHspdT = oapiGetSimTime();

	VECTOR3 hspd_vec;
	QC()->GetAirspeedVector(FRAME_HORIZON, hspd_vec);
	double hdg = QC()->GetYaw();
	m_pvx = hspd_vec.x * cos(hdg) - hspd_vec.z * sin(hdg);
	m_pvz = hspd_vec.x * sin(hdg) + hspd_vec.z * cos(hdg);
}

void PropulsionSubsystem::unsetCourseCmd()
{
	m_courseActive = false;
}

void PropulsionSubsystem::setHspdCmd(double hspd)
{
	m_hspdActive = true;
	m_hspdCmd = hspd;
	m_pHspdT = oapiGetSimTime();
	VECTOR3 v;
	QC()->GetHorizonAirspeedVector(v);
	m_phh = hypot(v.x, v.y);
	m_tiltTgt = TiltAngle();
}

void PropulsionSubsystem::unsetHspdCmd()
{
	m_hspdActive = false;
}

void PropulsionSubsystem::setVspdCmd(double vspd)
{
	m_vspdActive = true;
	m_altActive = false;
	m_vspdCmd = vspd;
	HoldActivate(true);
}

void PropulsionSubsystem::unsetVspdCmd()
{
	m_vspdActive = false;
	HoldActivate(false);
}

void PropulsionSubsystem::setAltCmd(double alt)
{
	m_altActive = true;
	m_vspdActive = false;
	m_altCmd = alt;
	HoldActivate(true);
}

void PropulsionSubsystem::unsetAltCmd()
{
	m_altActive = false;
	HoldActivate(false);
}

bool PropulsionSubsystem::SetHeading(double hdg_cmd, double &y_diff)
{
	const double alpha = 0.2;
	const double beta = 0.25;

	VECTOR3 avel;
	QC()->GetAngularVel(avel);
	double yv = avel.y;

	double hdg = QC()->GetYaw();
	double dhdg = hdg - hdg_cmd;
	if (dhdg > 0.0) {
		while (dhdg > PI)
			dhdg -= PI2;
	}
	else {
		while (dhdg < -PI)
			dhdg += PI2;
	}

	y_diff = alpha*dhdg - beta*yv;
	if (fabs(y_diff) > 0.1) y_diff = (y_diff > 0.0 ? 0.1 : -0.1);

	return (fabs(dhdg) < 0.002 && fabs(yv) < 0.001);
}

void PropulsionSubsystem::SetCourse(double course_cmd, double tilt_cmd, double &pitch, double &roll)
{
	const double sint = sin(tilt_cmd);

	VECTOR3 crs_loc, crs_hor = { sin(course_cmd), 0, cos(course_cmd) };
	QC()->HorizonInvRot(crs_hor, crs_loc);

	double len = hypot(crs_loc.x, crs_loc.z);
	double crs_loc_x = crs_loc.x / len * sint;
	double crs_loc_z = crs_loc.z / len * sint;

	pitch = -asin(crs_loc_z);
	roll = -asin(crs_loc_x);
}

void PropulsionSubsystem::SetHspdVector(double course_cmd, double hspd_cmd, double &pitch, double &roll)
{
	double t = oapiGetSimTime();
	double dt = t - m_pHspdT;
	if (!dt) return;
	m_pHspdT = t;

	double hdg = QC()->GetYaw();
	double coshdg = cos(hdg), sinhdg = sin(hdg);

	double hspdx_cmd = sin(course_cmd) * hspd_cmd;
	double hspdz_cmd = cos(course_cmd) * hspd_cmd;
	double vx_cmd = hspdx_cmd * coshdg - hspdz_cmd * sinhdg; // the commanded  horizontal speed components in the vessel frame
	double vz_cmd = hspdx_cmd * sinhdg + hspdz_cmd * coshdg;

	VECTOR3 hspd_vec;
	QC()->GetAirspeedVector(FRAME_HORIZON, hspd_vec);
	double vx = hspd_vec.x * coshdg - hspd_vec.z * sinhdg; // the current horizontal speed components in the vessel frame
	double vz = hspd_vec.x * sinhdg + hspd_vec.z * coshdg;

	double ax = (vx - m_pvx) / dt; // acceleration components
	double az = (vz - m_pvz) / dt;
	m_pvx = vx;
	m_pvz = vz;

	double dvx = vx_cmd - vx;
	double dvz = vz_cmd - vz;
	double ax_tgt = dvx;
	double az_tgt = dvz;
	double dax = ax_tgt - ax;
	double daz = az_tgt - az;

	double dpitch = -daz*0.1;
	double droll = -dax*0.1;
	double dtilt_max = dt*1.0;
	if (fabs(dpitch) > dtilt_max)
		dpitch = (dpitch > 0.0 ? dtilt_max : -dtilt_max);
	if (fabs(droll) > dtilt_max)
		droll = (droll > 0.0 ? dtilt_max : -dtilt_max);

	pitch = QC()->GetPitch() + dpitch;
	if (fabs(pitch) > PI*0.25)
		pitch = (pitch > 0.0 ? PI*0.25 : -PI*0.25);
	roll = QC()->GetBank() + droll;
	if (fabs(roll) > PI*0.25)
		roll = (roll > 0.0 ? PI*0.25 : -PI*0.25);
}
