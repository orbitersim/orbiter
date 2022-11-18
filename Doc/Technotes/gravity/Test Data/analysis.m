clear;
clc;


D  = importdata("GL-NT.pos"," ",4);
GMATalt = importdata("PlotData.txt", ",",3);
X = importdata("X.txt", ",",3);
Y = importdata("Y.txt", ",",3);
Z = importdata("Z.txt", ",",3);
gmat_x = X.data(:,2);
gmat_y = Y.data(:,2);
gmat_z = Z.data(:,2);



t = D.data(:,1);
r = D.data(:,2);
phi = D.data(:,3);
theta  = D.data(:,4);

[x,y,z] = sph2cart(phi,theta,r);
[gmat_phi,gmat_theta,gmat_r] = cart2sph(gmat_x,gmat_y,gmat_z);
gmat_r *= 1000;
gmat_a = GMATalt.data(:,2);
gmat_t = 86400*(GMATalt.data(:,1)- GMATalt.data(1,1));

gmat_xi = interp1(gmat_t,gmat_x,t,"cubic")*1000;
gmat_yi = interp1(gmat_t,gmat_y,t,"cubic")*1000;
gmat_zi = interp1(gmat_t,gmat_z,t,"cubic")*1000;

gmat_ri = interp1(gmat_t,gmat_r,t,"cubic");
gmat_phii = interp1(gmat_t,gmat_phi,t,"cubic");
gmat_thetai = interp1(gmat_t,gmat_theta,t,"cubic");

res = sqrt((x-gmat_xi).^2 + (y-gmat_yi).^2 + (z-gmat_zi).^2);


a = r - 1.7385e6;
figure(1);
plot(t,(a/1000),'k--');
hold on;
plot(gmat_t,gmat_a,'k-');
hold off;
grid on;
grid minor;
legend("Orbiter-Pines","GMAT-Pines");
xlabel("Time [s]");
ylabel("Altitude above mean lunar radius [km]");
axis([0 100000 60 77])

figure(2);
plot(t,res,'k')
grid on;
grid minor;
xlabel("Time [s]");
ylabel("Orbiter absolute position error relative to GMAT [m]");
axis([0 100000 0 1000 ])

figure(3);
plot(t,r-gmat_ri,'k')
grid on;
grid minor;
xlabel("Time [s]");
ylabel("Orbiter radial position error relative to GMAT [m]");
axis([0 100000 -100 100 ])

##figure(4);
##hold on;
##plot(t,rad2deg(theta-gmat_thetai),'k--');
##plot(t,rad2deg(phi-gmat_phii),'k.');
##hold off;
##xlabel("Time [s]");
##ylabel("Orbiter lat/long position error");
##grid on;
##grid minor;

#print some initial position information
x(1)-gmat_xi(1)
y(1)-gmat_yi(1)
z(1)-gmat_zi(1)