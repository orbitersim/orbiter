function precession_anim

clear all
close all

RAD=pi/180.0;
DEG=180.0/pi;

col = ['b' 'r' 'g' 'c'];
obliq = [0 10 20 30]*RAD;
lan_ref = 40*RAD;
obliq_axis = 20*RAD;
lan_axis = 320*RAD;

rad = 1.5*tan(obliq_axis);

for n=1:length(obliq)
    obliq_ref = obliq(n);

    Oref = [[1 0 0];[0 cos(obliq_ref) -sin(obliq_ref)];[0 sin(obliq_ref) cos(obliq_ref)]];
    Lref = [[cos(lan_ref) -sin(lan_ref) 0];[sin(lan_ref) cos(lan_ref) 0];[0 0 1]];
    Rref = Lref*Oref;

    Oaxis = [[cos(obliq_axis) 0 sin(obliq_axis)];[0 1 0];[-sin(obliq_axis) 0 cos(obliq_axis)]];
    Laxis = [[cos(lan_axis) -sin(lan_axis) 0];[sin(lan_axis) cos(lan_axis) 0];[0 0 1]];
    Raxis = Laxis*Oaxis;
    ref_axis = Rref * [0 0 1.5]';

    lanrel = [0:2*pi/1000:2*pi];
    for i=1:length(lanrel)
        [obl(i) lan(i)] = axis_ecl(obliq_ref,lan_ref,obliq_axis,lanrel(i));
    end
    figure(1);
    hold on
    plot (lanrel,obl,col(n),'LineWidth',2);
    xlabel('L_{rel}');
    ylabel('\epsilon_{ecl}');
    axis([0 2*pi 0.0 0.9]);
    grid on
    figure(2);
    hold on
    sep = 0;
    for i=1:length(lan)-1
        if abs(lan(i)-lan(i+1)) > pi
            sep = i;
            break;
        end
    end
    if sep > 0
        lanrel2=[lanrel(1:i),(lanrel(i)+lanrel(i+1))/2,(lanrel(i)+lanrel(i+1))/2,lanrel(i+1:end)];
        lan2=[lan(1:i),inf,-inf,lan(i+1:end)];
        plot (lanrel2,lan2,col(n),'LineWidth',2);
        %plot(lanrel(1:i),lan(1:i),col(n),'LineWidth',2);
        %plot(lanrel(i+1:end),lan(i+1:end),col(n),'LineWidth',2);
    else
        plot (lanrel,lan,col(n),'LineWidth',2);
    end
    xlabel('L_{rel}');
    ylabel('L_{ecl}');
    axis([0 2*pi -pi pi]);
    grid on
end
for n=1:2
    figure(n);
    legend('\epsilon_{ref}=0','\epsilon_{ref}=10\circ','\epsilon_{ref}=20\circ','\epsilon_{ref}=30\circ');
end

end



function vtx=circle(rad)

nseg=64;
for i=0:nseg
    phi = i/nseg*pi*2;
    vtx(i+1,:) = [rad*cos(phi) rad*sin(phi) 0];
end

end


function vtx=arc(rad,phi0,phi1)

nseg=64;
vtx = [0 0 0];
nstep = ceil((phi1-phi0)/(2*pi)*nseg);
if nstep > 0
    dphi=(phi1-phi0)/nstep;
    i=1;
    for phi=phi0:dphi:phi1
        vtx(i,:) = [rad*cos(phi) rad*sin(phi) 0];
        i = i+1;
    end
end

end


% Calculate obliquity and LAN of axis w.r.t. ecliptic
% from obliquity and LAN of precession reference axis
% and relative obliquity and LAN
function [obliq lan]=axis_ecl(obliq_ref,lan_ref,obliq_rel,lan_rel)

Oref = [[1 0 0];[0 cos(obliq_ref) -sin(obliq_ref)];[0 sin(obliq_ref) cos(obliq_ref)]];
Lref = [[cos(lan_ref) -sin(lan_ref) 0];[sin(lan_ref) cos(lan_ref) 0];[0 0 1]];
Rref = Lref*Oref;
Orel = [[1 0 0];[0 cos(obliq_rel) -sin(obliq_rel)];[0 sin(obliq_rel) cos(obliq_rel)]];
Lrel = [[cos(lan_rel) -sin(lan_rel) 0];[sin(lan_rel) cos(lan_rel) 0];[0 0 1]];
Rrel = Lrel*Orel;
v=[0 0 1];

rot_axis = Rref * Rrel * v';
obliq = acos(rot_axis(3));
lan = atan2(rot_axis(1),-rot_axis(2));
end
