clc
clear all

%data percobaan

x0 = load('data_plot_x0.txt');
y0 = load('data_plot_y0.txt');

%data interpolasi hermite kubik
xhk = load('data_plot_xhk.txt');
yhk = load('data_plot_yhk.txt');

plot(x0,y0,'.',xhk,yhk,MarkerSize=20,LineWidth=1.5)
title  ('Plot Data dan Interpolasinya','FontSize',15)
grid on
legend ('Data Percobaan','interpolasi hermite kubik','location','northwest','FontSize',10)