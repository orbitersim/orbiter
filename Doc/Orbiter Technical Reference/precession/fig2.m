function precession_anim

clear all
close all

RAD=pi/180.0;
DEG=180.0/pi;

obliq_ref = 30*RAD;
lan_ref = 40*RAD;
obliq_axis = 19.5*RAD;
lan_axis = 320*RAD;

rad = 1.4*tan(obliq_axis);
axis_length = 1.3;

Oref = [[1 0 0];[0 cos(obliq_ref) -sin(obliq_ref)];[0 sin(obliq_ref) cos(obliq_ref)]];
Lref = [[cos(lan_ref) -sin(lan_ref) 0];[sin(lan_ref) cos(lan_ref) 0];[0 0 1]];
Rref = Lref*Oref;

Oaxis = [[cos(obliq_axis) 0 sin(obliq_axis)];[0 1 0];[-sin(obliq_axis) 0 cos(obliq_axis)]];
Laxis = [[cos(lan_axis) -sin(lan_axis) 0];[sin(lan_axis) cos(lan_axis) 0];[0 0 1]];
Raxis = Laxis*Oaxis;
ref_axis = Rref * [0 0 axis_length]';

figure;
line([-1 1],[0 0],[0 0]);
line([0 0],[-1 1],[0 0]);
line([0 0],[0 0],[0 1.5]);
c=circle(1);
line(c(:,1),c(:,2),c(:,3));
%patch([-1 -1 1 1],[-1 1 1 -1],[0 0 0 0],[.7 .7 .7],'FaceAlpha',0.2);
text(-1.05,0,0,'\Upsilon','HorizontalAlignment','right');
%text(0,0,1.6,'ecliptic north pole','HorizontalAlignment','right');
text(-0.9,1.0,0,'ecliptic');
line([0 ref_axis(1)],[0 ref_axis(2)],[0 ref_axis(3)],'LineWidth',2);
line([ref_axis(1) ref_axis(1)],[ref_axis(2) ref_axis(2)],[0 ref_axis(3)]);
line([0 ref_axis(1)],[0 ref_axis(2)],[0 0]);
c=Lref*[1.2 0 0]';
line([c(1) -c(1)],[c(2) -c(2)],[0 0],'LineStyle','--');
text(c(1)*(-1.15),c(2)*(-1.15),0,'N_{ref}','Color','blue');

c = circle(rad);
c(:,3) = c(:,3)+axis_length;
for i=1:size(c,1), c(i,:) = Rref*c(i,:)'; end
line(c(:,1),c(:,2),c(:,3),'Color',[.3 .3 .3]);

c = arc(0.7,pi,pi+obliq_ref);
for i=1:size(c,1), c(i,:) = Lref * [[0 0 1];[0 1 0];[-1 0 0]] * c(i,:)'; end
line(c(:,1),c(:,2),c(:,3));
c = arc(0.65,pi,pi+lan_ref);
line(c(:,1),c(:,2),c(:,3));
text(0.05,0.0,0.5,'\epsilon_{ref}','Color','blue');
text(-0.42,-0.3,0,'L_{ref}','Color','blue','HorizontalAlignment','right');
text(-0.7,-0.4,0,'L_{ecl}','Color','red','HorizontalAlignment','right');
axis equal tight
axis off
view(-12,16)
set(gca,'Projection','perspective');

lanrel = [0:2*pi/200:2*pi];
for i=1:length(lanrel)
    [obl(i) lan(i)] = axis_ecl(obliq_ref,lan_ref,obliq_axis,lanrel(i));
end


for dphi = 55:55
    lan_axis = RAD*dphi;
    Laxis = [[cos(lan_axis) -sin(lan_axis) 0];[sin(lan_axis) cos(lan_axis) 0];[0 0 1]];

    [obliq_axis_tot lan_axis_tot] = axis_ecl(obliq_ref, lan_ref, obliq_axis, lan_axis);
    Oaxistot = [[1 0 0];[0 cos(obliq_axis_tot) -sin(obliq_axis_tot)];[0 sin(obliq_axis_tot) cos(obliq_axis_tot)]];
    Laxistot = [[cos(lan_axis_tot) -sin(lan_axis_tot) 0];[sin(lan_axis_tot) cos(lan_axis_tot) 0];[0 0 1]];
    Raxistot = Laxistot*Oaxistot;
    rot_axis = Raxistot * [0 0 norm([rad 0 axis_length])]';

    h(1) = line([0 rot_axis(1)],[0 rot_axis(2)],[0 rot_axis(3)],'LineWidth',2,'Color','red');
    h(2) = line([rot_axis(1) rot_axis(1)],[rot_axis(2) rot_axis(2)],[0 rot_axis(3)],'Color','red');
    h(3) = line([0 rot_axis(1)],[0 rot_axis(2)],[0 0],'Color','red');
    h(4) = line([ref_axis(1) rot_axis(1)],[ref_axis(2) rot_axis(2)],[ref_axis(3) rot_axis(3)],'LineWidth',2,'Color',[0 0.5 0]);

    c = [rad 0 0]';
    c(3) = axis_length;
    c = [Rref*c,Rref*[-c(1);-c(2);c(3)]];
    h(5) = line([c(1,1) c(1,2)],[c(2,1) c(2,2)],[c(3,1) c(3,2)]);
    
    c = Laxis*[rad*1.2 0 0]';
    c(3) = axis_length;
    c = [Rref*c,Rref*[-c(1);-c(2);c(3)]];
    h(6) = line([c(1,1) c(1,2)],[c(2,1) c(2,2)],[c(3,1) c(3,2)],'LineStyle','--','Color',[0 0.5 0]);
    h(7) = text(c(1,2)+0.02, c(2,2), c(3,2)-0.04,'N_{rel}','Color',[0 0.5 0],'VerticalAlignment','top','HorizontalAlignment','center');
    
    c=Laxistot*[1.2 0 0]';
    h(8) = line([c(1) -c(1)],[c(2) -c(2)],[0 0],'Color','red','LineStyle','--');
    h(9) = text(c(1)*(-1.15),c(2)*(-1.15),0,'N_{ecl}','Color','red');

    c = arc(0.7,pi,pi+obliq_axis_tot);
    for i=1:size(c,1), c(i,:) = Laxistot * [[0 0 1];[0 1 0];[-1 0 0]] * c(i,:)'; end
    h(10) = line(c(:,1),c(:,2),c(:,3),'Color','red');
    h(11) = text(0.15,0.0,0.75,'\epsilon_{ecl}','Color','red');

    c = arc(0.75,pi,pi+lan_axis_tot);
    nh = 0; % size(c,1);
    h(12) = line(c(:,1),c(:,2),c(:,3),'Color','red');

    c = arc(0.3,pi,pi+lan_axis);
    c(:,3) = c(:,3)+axis_length;
    for i=1:size(c,1) c(i,:) = Rref * c(i,:)'; end
    h(13) = line(c(:,1),c(:,2),c(:,3),'Color',[0 0.5 0]);
    
    text(-0.04,0,-0.07,'O');
    text(-0.03,0,1.55,'N');
    text(ref_axis(1)*1.04-0.1,ref_axis(2)*1.04,ref_axis(3)*1.04+0.02,'P');
    text(rot_axis(1)*1.04,rot_axis(2)*1.04,rot_axis(3)*1.04,'S');
    text(0.40,0,0.78,'L_{rel}','Color',[0 0.5 0]);
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
