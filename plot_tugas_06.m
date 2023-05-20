clc
clear all

%data percobaan
dp = load("input_matrix.txt");

nn = size(dp);
n = nn(1,1);

for i=1:n
 xdp(i) = dp(i,1);
 ydp(i) = dp(i,2);
end

x0 = load("data_x0.txt");
y0  = load("data_y0.txt");

dx = x0(3)-x0(2);

ssy = size(x0);
sy = ssy(1,1);

I= 0.0;
for i=1:sy
    I = I + dx*y0(i);
end

%integral numerik
A = load("koefisien_pol_orde_7.txt");
anti_pol_7 = @(x) A(1)*x + (1/2)*A(2)*x^2 + (1/3)*A(3)*x^3 + (1/4)*A(4)*x^4 + (1/5)*A(5)*x^5 + (1/6)*A(6)*x^6 + (1/7)*A(7)*x^7 + (1/8)*A(8)*x^8;

I_analitik = anti_pol_7(55) - anti_pol_7(10);

fatas = anti_pol_7(55);
fbawah = anti_pol_7(10);

plot(xdp,ydp,'.',x0,y0,MarkerSize=20,LineWidth=1.5)
grid on
