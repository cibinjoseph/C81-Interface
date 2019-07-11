% Creates csv file for flat plate
clc; clear;
mach=linspace(0.0,1.0,5.0);
alpha=linspace(1.0,6.0,99.0);
CL=alpha'*pi/180.0*2.0*pi;
CL=[CL CL CL CL CL];
CLout=[0.0 mach; alpha' CL];
dlmwrite('flatplate.csv',CLout);
