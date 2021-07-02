function fig1

clear all
close all

RAD=pi/180.0;
DEG=180.0/pi;

obliq_ref = 30*RAD;
lan_ref = 40*RAD;
obliq_axis = 19.5*RAD;
lan_axis = 320*RAD;

rad = 1.5*tan(obliq_axis);

Oref = [[1 0 0];[0 cos(obliq_ref) -sin(obliq_ref)];[0 sin(obliq_ref) cos(obliq_ref)]];
Lref = [[cos(lan_ref) -sin(lan_ref) 0];[sin(lan_ref) cos(lan_ref) 0];[0 0 1]];
Rref = Lref*Oref;

Oaxis = [[cos(obliq_axis) 0 sin(obliq_axis)];[0 1 0];[-sin(obliq_axis) 0 cos(obliq_axis)]];
Laxis = [[cos(lan_axis) -sin(lan_axis) 0];[sin(lan_axis) cos(lan_axis) 0];[0 0 1]];
Raxis = Laxis*Oaxis;
ref_axis = Rref * [0 0 1.5]';

figure;
line([-1 1],[0 0],[0 0]);
line([0 0],[-1 1],[0 0]);
line([0 0],[0 0],[0 1.5]);
c=circle(1);
line(c(:,1),c(:,2),c(:,3));
%patch([-1 -1 1 1],[-1 1 1 -1],[0 0 0 0],[.7 .7 .7],'FaceAlpha',0.2);
text(-1.05,0,0,'\Upsilon (x)','HorizontalAlignment','right');
text(0.05,0,1.6,'ecliptic north pole (y)','HorizontalAlignment','right');
text(-0.05,-1.2,0,'z');
text(-0.9,1.0,0,'ecliptic');
line([0 ref_axis(1)],[0 ref_axis(2)],[0 ref_axis(3)],'LineWidth',2);
line([ref_axis(1) ref_axis(1)],[ref_axis(2) ref_axis(2)],[0 ref_axis(3)]);
line([0 ref_axis(1)],[0 ref_axis(2)],[0 0]);
c=Lref*[1.2 0 0]';
line([c(1) -c(1)],[c(2) -c(2)],[0 0],'LineStyle','--');
text(c(1)*(-1.15),c(2)*(-1.15),0,'N_{ref}');

%c = circle(rad);
%c(:,3) = c(:,3)+1.5;
%for i=1:size(c,1), c(i,:) = Rref*c(i,:)'; end
%line(c(:,1),c(:,2),c(:,3),'Color',[.3 .3 .3]);

c = arc(0.7,pi,pi+obliq_ref);
for i=1:size(c,1), c(i,:) = Lref * [[0 0 1];[0 1 0];[-1 0 0]] * c(i,:)'; end
line(c(:,1),c(:,2),c(:,3));
c = arc(0.6,pi,pi+lan_ref);
line(c(:,1),c(:,2),c(:,3));
text(0.05,0.0,0.5,'\epsilon_{ref}');
text(-0.73,-0.33,0,'L_{ref}');

text(-0.04,0,-0.1,'O');
text(ref_axis(1)*1.04,ref_axis(2)*1.04,ref_axis(3)*1.04,'P');

axis equal tight
axis off
view(-12,16)
set(gca,'Projection','perspective');
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