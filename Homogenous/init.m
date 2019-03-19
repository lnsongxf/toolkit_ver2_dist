clc;
clear all;

global psi gamma taua taur tauw taui za zm z;
global yshare_a_data yshare_m_data yshare_x_data
global tshare_v_data tshare_b_data tshare_p_data tyratio_data
global mu_u mu_r mu_f ;
global abar beta delta alpha_s alpha_a alpha_a1 alpha_s1 alpha_s2 alpha_m ;
global dr ds dl pm pst zs;
global r wf w cur cf bur kf hu hr ha hs km hm;
global ps pa ;
global pif pis cs_a cs_m cs_s BB OP OP1;
global out_a out_m out_x out_v out_p out_b;
OP = optimoptions('fminunc'); 
OP.Display = 'notify';
OP.Algorithm = 'quasi-newton';
OP1 = optimoptions('fsolve');
OP1.Display = 'none';

ps = 33.7854754960 ;
pa = 40.0984443924 ;
wf = 7.7803668062 ;

taua   = 0.0803361532 ;   
taur   = 0.1305349787 ;   
tauw   = 0.0589203613 ;   
za     = 0.6488502896 ;   
zm     = 8.2389901797 ;   
z      = 0.6922109209 ;   

psi    = 0.4944552241 ;   
gamma  = 0.8167964379 ;   

mu_u = 0.28 ;
mu_r = 0.69 ;
mu_f = 1.0 - mu_u - mu_r ;
if mu_f < 0
   disp('Warning: Measure of Large Farmer Exogenously Set to 3%.')
   mu_f = 0.03;
   mu_r = mu_r - mu_f ;
end

abar   = 0.0008314562 ;
yshare_a_data = 0.427;
yshare_m_data = 0.33 ;
yshare_x_data = 0.083 ;
tshare_v_data = 0.45 ;
tshare_p_data = 0.17 ;
tshare_b_data = 1 - tshare_v_data - tshare_p_data ;
if tshare_b_data <= 0
    disp('Warning: Negative Business Tax!')
    disp('Type "dbquit" to terminate execution.')
    keyboard ;
end
tyratio_data  = 0.08 ;

beta     = 0.96 ;
delta    = 0.06 ;

alpha_s  = 0.365 ;       
alpha_a  = 0.49 ;        
alpha_a1 = 0.49 ;        
alpha_s1 = 0.49 ;        
alpha_s2 = 0.32 ;        
alpha_m  = 0.365 ;       

dr = 0.273 ;             
ds = 0.273 ;             
dl = 1.0 ;               

pm   = 1.0 ;
pst  = 20.7 ;
zs   = 1.0 ;
taui = 0.365*taur ;

BB = zeros(5,1) ;